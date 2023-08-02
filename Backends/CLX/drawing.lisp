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

(defun %clx-draw-aligned-ellipse (mi gc cx cy rx ry eta1 eta2 filled)
  (let* ((x (round-coordinate (- cx rx)))
         (y (round-coordinate (- cy ry)))
         (w (round-coordinate (* 2 rx)))
         (h (round-coordinate (* 2 ry)))
         (arc-angle (- eta2 eta1))
         (arc-angle (if (< arc-angle 0)
                        (+ (* pi 2) arc-angle)
                        arc-angle))
         (eta1 (mod eta1 (* 2 pi))))
    (when (and (typep x 'clx-coordinate)
               (typep y 'clx-coordinate))
      (xlib:draw-arc mi gc x y w h eta1 arc-angle filled))))

(defun %clx-draw-polygon-ellipse (mi gc cx cy rx ry theta eta1 eta2 filled)
  (let ((coords (climi::polygonalize-ellipse* cx cy rx ry theta eta1 eta2 filled)))
    (clx-draw-polygon mi gc +identity-transformation+ coords nil filled)))

(defun clx-draw-ellipse (mi gc tr cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 filled)
  (multiple-value-bind (cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2)
      (climi::transform-ellipse tr cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2)
    (multiple-value-bind (rx ry theta)
        (climi::ellipse-normalized-representation* rdx1 rdy1 rdx2 rdy2)
      (unless (null rx)
        (if (zerop theta)
            (%clx-draw-aligned-ellipse mi gc cx cy rx ry eta1 eta2 filled)
            (%clx-draw-polygon-ellipse mi gc cx cy rx ry theta eta1 eta2 filled))))))

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

;;; The underlying assumption is that the mirror has a rectangle clip and
;;; parameters x1 y1 x2 y2 are supplied only for a guidance to deal with
;;; unbounded regions and/or for optimization purposes.
(defgeneric clx-draw-region (mi gc tr x1 y1 x2 y2 region)
  (:method (mi gc tr x1 y1 x2 y2 (region nowhere-region)))
  (:method (mi gc tr x1 y1 x2 y2 (region everywhere-region))
    (clx-draw-rectangle mi gc tr x1 y1 x2 y2 t))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-rectangle))
    (with-bounding-rectangle* (x1 y1 x2 y2) region
      (clx-draw-rectangle mi gc tr x1 y1 x2 y2 t)))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-polygon))
    (let ((coords (climi::expand-point-seq (polygon-points region))))
      (clx-draw-polygon mi gc tr coords t t)))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-bezigon))
    (let ((coords (climi::polygonalize-bezigon
                   (climi::expand-point-seq (bezigon-points region)))))
      (clx-draw-polygon mi gc tr coords t t)))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-ellipse))
    (multiple-value-bind (cx cy) (ellipse-center-point* region)
      (multiple-value-bind (rdx1 rdy1 rdx2 rdy2) (ellipse-radii region)
        (let ((eta1 (or (ellipse-start-angle region) 0.0))
              (eta2 (or (ellipse-end-angle region) (* 2.0 pi))))
          (clx-draw-ellipse mi gc tr cx cy rdx1 rdy1 rdx2 rdy2 eta1 eta2 t)))))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-region-complement))
    (let ((fg (xlib:gcontext-foreground gc))
          (bg (xlib:gcontext-background gc))
          (r* (region-complement region)))
      (clx-draw-rectangle mi gc tr x1 y1 x2 y2 t)
      (setf (xlib:gcontext-foreground gc) bg)
      (clx-draw-region mi gc tr x1 y1 x2 y2 r*)
      (setf (xlib:gcontext-foreground gc) fg)))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-region-union))
    (flet ((draw-it (region*)
             (clx-draw-region mi gc tr x1 y1 x2 y2 region*)))
      (map-over-region-set-regions #'draw-it region)))
  (:method (mi gc tr x1 y1 x2 y2 (region standard-region-intersection))
    ;; This is a bit excessive approach but it works. -- jd 2023-08-03
    (labels ((remask (target mask)
               (destructuring-bind (buf dgc zgc) target
                 (declare (ignore buf zgc))
                 (setf (xlib:gcontext-clip-mask dgc) (first mask))))
             (paint (target region)
               (destructuring-bind (buf dgc zgc) target
                 (xlib:draw-rectangle buf zgc x1 y1 x2 y2 t)
                 (clx-draw-region buf dgc tr x1 y1 x2 y2 region)))
             (make ()
               (let* ((buffer (xlib:create-pixmap :drawable mi :depth 1 :width x2 :height y2))
                      (draw (xlib:create-gcontext :drawable buffer :foreground 1 :background 0))
                      (zero (xlib:create-gcontext :drawable buffer :foreground 0 :background 1)))
                 (list buffer draw zero)))
             (free (palette)
               (destructuring-bind (buf dgc zgc) palette
                 (xlib:free-gcontext dgc)
                 (xlib:free-gcontext zgc)
                 (xlib:free-pixmap buf))))
      (let* ((palette-1 (make))
             (palette-2 (make))
             (regions (region-set-regions region :normalize t))
             (region0 (pop regions)))
        ;; A x B x C x D x E
        ;; draw A           -> 1
        ;; draw B through 1 -> 2
        ;; draw C through 2 -> 1
        ;; draw D through 1 -> 2
        ;; draw E through 2 -> 1
        ;; fill M through 1
        (paint palette-1 region0)
        (remask palette-2 palette-1)
        (remask palette-1 palette-2)
        (dolist (r regions)
          (paint  palette-2 r)
          (rotatef palette-1 palette-2))
        (climi::letf (((xlib:gcontext-clip-mask gc) (first palette-1)))
          (xlib:draw-rectangle mi gc x1 y1 (- x2 x1) (- y2 y1) t))
        (free palette-1)
        (free palette-2))))
  (:method (mi gc tr x1 y1 x2 y2 region) ; fallback
    (warn "clx-draw-region: unoptimized path for ~s." (type-of region))
    (loop for x from (floor x1) upto (ceiling x2) do
      (loop for y from (floor y1) upto (ceiling y2) do
        (when (region-contains-position-p region x y)
          (xlib:draw-point mi gc x y))))))
