;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; This file implements "legacy" x11 drawing routines. We are not concerned
;;; with the ink - we expect the gc to be created beforehand. Functions defined
;;; in this file are responsible for transforming and rounding coordinates to
;;; values acceptable by clx.

(in-package #:clim-clx)

(defun make-clx-render-color (r g b a)
  ;; Hmm, XRender uses pre-multiplied alpha, how useful!
  (vector (clamp (truncate (* #xffff a r)) 0 #xffff)
          (clamp (truncate (* #xffff a g)) 0 #xffff)
          (clamp (truncate (* #xffff a b)) 0 #xffff)
          (clamp (truncate (* #xffff a)) 0 #xffff)))

(defparameter +transparent-black+
  (make-clx-render-color 0 0 0 0))

(defparameter +solid-black+
  (make-clx-render-color 0 0 1 1))

(defun transform-picture (transformation picture)
  ;; 1. XRender expects a transformation to the target's plane
  ;;      (X SOURCE) -> TARGET
  ;;
  ;; 2. Ink transformation is specified for the source's plane
  ;;      (Y DESIGN) -> SOURCE
  ;;
  ;; 3. Untransformed design has the same plane as the target
  ;;      DESIGN = TARGET
  ;;
  ;; 4. Let's substitute the DESIGN with the TARGET in (2):
  ;;      1: (X SOURCE) -> TARGET
  ;;      2: (Y TARGET) -> SOURCE
  ;;
  ;; C: In other words Y is the inverse transformation of X -- jd 2021-01-22
  (multiple-value-bind (rxx rxy ryx ryy dx dy)
      (climi::get-transformation (invert-transformation transformation))
    (flet ((clx-fixed (value)
             ;; 32 bit value (top 16 integer, bottom 16 fraction)
             (logand (truncate (* value #x10000)) #xFFFFFFFF)))
      (apply #'xlib:render-set-picture-transform picture
             (mapcar #'clx-fixed (list rxx rxy dx ryx ryy dy 0 0 1))))))

;;; Quick routine to fill a rectangle with an uniform ink.
(defun clx-fill-rectangle (op clx-render-color dst tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (let ((x (clamp (min x1 x2) #x-8000 #x7FFF))
          (y (clamp (min y1 y2) #x-8000 #x7FFF))
          (w (clamp (abs (- x2 x1)) 0 #xffff))
          (h (clamp (abs (- y2 y1)) 0 #xffff)))
      (xlib:render-fill-rectangle dst op clx-render-color x y w h))))

(defun clx-wipe-picture (picture width height color)
  (xlib:render-fill-rectangle picture :src color 0 0 width height))

;;; PSA window pictures won't work as a source. Why? Who knows.. -- jd
(defun clx-fill-composite (op src clp dst tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (let ((x (min x1 x2))
          (y (min y1 y2))
          (w (abs (- x2 x1)))
          (h (abs (- y2 y1))))
      (xlib:render-composite op src clp dst x y x y x y w h))))

;;; Note that if the format is :NONE or does not have the alpha component, then
;;; all figures will be rendered as if they were specified separately.

(defun clx-fill-triangles (op src dst format tr coord-seq)
  (with-round-coordinates (tr coord-seq)
    (xlib:render-triangles dst op src 0 0 format coord-seq)))

(defun clx-fill-trifan (op src dst format tr coord-seq)
  (with-round-coordinates (tr coord-seq)
    (xlib:render-triangle-fan dst op src 0 0 format coord-seq)))

(defun clx-fill-tristrip (op src dst tr format coord-seq)
  (with-round-coordinates (tr coord-seq)
    (xlib:render-triangle-strip dst op src 0 0 format coord-seq)))

(defun clx-fill-polygon (op src dst format tr coord-seq)
  (let ((coords (climi::expand-point-seq
                 (climi::triangulate-polygon (make-polygon* coord-seq)))))
    (clx-fill-triangles op src dst format tr coords)))


;;; Legacy drawing routines.

(defun clx-draw-point (mi gc tr x y)
  (with-round-positions (tr x y)
    (when (and (typep x 'clx-coordinate)
               (typep y 'clx-coordinate))
      (xlib:draw-point mi gc x y))))

(defun clx-draw-line (mi gc tr x1 y1 x2 y2)
  (with-clipped-line (tr x1 y1 x2 y2)
    (xlib:draw-line mi gc x1 y1 x2 y2)))

(defun clx-draw-polygon (mi gc tr coords closed filled)
  (with-clipped-poly (tr coords closed)
    (xlib:draw-lines mi gc coords :fill-p filled)))

(defun clx-draw-rectangle (mi gc tr x1 y1 x2 y2 filled)
  (with-clipped-rect (tr x1 y1 x2 y2)
    (xlib:draw-rectangle mi gc x1 y1 (- x2 x1) (- y2 y1) filled)))

(defun clx-draw-aligned-ellipse (mi gc cx cy
                                 rdx1 rdy1 rdx2 rdy2
                                 eta1 eta2 filled)
  (let ((rdx (abs (+ rdx1 rdx2)))
        (rdy (abs (+ rdy1 rdy2))))
    (let* ((x1 (round-coordinate (- cx rdx)))
           (y1 (round-coordinate (- cy rdy)))
           (x2 (round-coordinate (+ cx rdx)))
           (y2 (round-coordinate (+ cy rdy)))
           (arc-angle (- eta2 eta1))
           (arc-angle (if (< arc-angle 0)
                          (+ (* pi 2) arc-angle)
                          arc-angle))
           (eta1 (mod eta1 (* 2 pi))))
      (when (and (typep x1 'clx-coordinate)
                 (typep y1 'clx-coordinate))
        (xlib:draw-arc mi gc x1 y1 (- x2 x1) (- y2 y1)
                       eta1 arc-angle filled)))))

;;; This function is different from clx-draw-ellipse in the fact that the radius
;;; is not transformed and that always a full circle is drawn.
(defun clx-draw-circle (mi gc tr x y radius filled)
  (with-transformed-position (tr x y)
    (let* ((x1 (round-coordinate (- x radius)))
           (y1 (round-coordinate (- y radius)))
           (x2 (round-coordinate (+ x radius)))
           (y2 (round-coordinate (+ y radius))))
      (when (and (typep x1 'clx-coordinate)
                 (typep y1 'clx-coordinate))
        (xlib:draw-arc mi gc x1 y1 (- x2 x1) (- y2 y1) 0 (* 2 pi) filled)))))
