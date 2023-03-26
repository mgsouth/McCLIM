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

(defun clx-fill-composite (op src clp dst tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (let ((x (min x1 x2))
          (y (min y1 y2))
          (w (abs (- x2 x1)))
          (h (abs (- y2 y1))))
      (xlib:render-composite op src clp dst x y x y x y w h))))

(defun clx-fill-triangles (op src dst tr coord-seq)
  (with-round-coordinates (tr coord-seq)
    (xlib:render-triangles dst op src 0 0 :none coord-seq)))

(defun make-clx-render-color (r g b a)
  ;; Hmm, XRender uses pre-multiplied alpha, how useful!
  (list (clamp (round (* #xffff a r)) 0 #xffff)
        (clamp (round (* #xffff a g)) 0 #xffff)
        (clamp (round (* #xffff a b)) 0 #xffff)
        (clamp (round (* #xffff a)) 0 #xffff)))

;;; Quick routine to fill a rectangle with an uniform ink. SRC is a color
;;; (that is a list of four elements).
(defun clx-fill-rectangle (op src dst tr x1 y1 x2 y2)
  (with-round-positions (tr x1 y1 x2 y2)
    (let ((x (clamp (min x1 x2) #x-8000 #x7FFF))
          (y (clamp (min y1 y2) #x-8000 #x7FFF))
          (w (clamp (abs (- x2 x1)) 0 #xffff))
          (h (clamp (abs (- y2 y1)) 0 #xffff)))
      (xlib:render-fill-rectangle dst op src x y w h))))

(defun clx-fill-polygon (op src dst tr coord-seq)
  (let ((coords (climi::expand-point-seq
                 (climi::triangulate-polygon (make-polygon* coord-seq)))))
    (clx-fill-triangles op src dst tr coords)))

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
