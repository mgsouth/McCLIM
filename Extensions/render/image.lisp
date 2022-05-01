(in-package #:mcclim-render)

;;; Image

(defun draw-image* (medium image x y
                    &rest args
                    &key clipping-region transformation)
  (declare (ignorable clipping-region transformation args))
  (climi::with-medium-options (medium args)
    (draw-pattern* medium image x y)))

(clim-internals::def-graphic-op draw-image* (image x y))

;;; Image operations

(defun make-image (width height)
  "Create an empty transparent image of size WIDTH x HEIGHT."
  ;; XXX something in text rendering depends image being transparent by
  ;; default. This should be fixed.
  (make-instance 'clime:image-pattern :array (make-argb-pixel-array width height)))

;;; Unsafe versions of COPY-IMAGE. Caller must ensure that all arguments are
;;; valid and arrays are of proper type.
(macrolet
    ((define-copy-image (name backwardp)
       `(progn
          (declaim (inline ,name))
          (defun ,name (src-array dst-array x1s y1s x1d y1d x2 y2)
            (declare (type image-index x1s y1s x1d y1d x2 y2)
                     (type argb-pixel-array src-array dst-array)
                     (optimize (speed 3)
                               #-ccl (safety 0)
                               #+ccl (safety 1)))
            (do-regions ((src-j dest-j y1s y1d y2)
                         (src-i dest-i x1s x1d x2)
                         ,@(when backwardp
                             `(:backward t)))
              (setf (aref dst-array dest-j dest-i)
                    (aref src-array src-j src-i)))))))
  (define-copy-image %copy-image nil)
  (define-copy-image %copy-image* t))

;;; XXX: We should unify it with COPY-AREA and MEDIUM-COPY-AREA. That means that
;;; raster images should be mediums on their own rights (aren't they?).
(defun copy-image (src-image sx sy width height dst-image dx dy
                   &aux
                   (sx (round sx))
                   (sy (round sy))
                   (dx (round dx))
                   (dy (round dy))
                   (width (round width))
                   (height (round height))
                   (src-array (climi::pattern-array src-image))
                   (dst-array (climi::pattern-array dst-image)))
  "Copy SRC-IMAGE to DST-IMAGE region-wise. Both may be the same image."
  (unless (%check-coords src-array dst-array sx sy dx dy width height)
    (return-from copy-image +nowhere+))
  (let ((max-x (+ dx width))
        (max-y (+ dy height)))
    (declare (fixnum max-x max-y))
    (cond ((not (eq src-array dst-array))
           #1=(%copy-image src-array dst-array sx sy dx dy max-x max-y))
          ((> sy dy) #1#)
          ((< sy dy) #2=(%copy-image* src-array dst-array sx sy dx dy max-x max-y))
          ((> sx dx) #1#)
          ((< sx dx) #2#)
          (t nil))
    (make-rectangle* dx dy max-x max-y)))

(macrolet
    ((define-blend-image (name backwardp)
       `(progn
          (declaim (inline ,name))
          (defun ,name (src-array dst-array x1s y1s x1d y1d x2 y2)
            (declare (type image-index x1s y1s x1d y1d x2 y2)
                     (type argb-pixel-array src-array dst-array)
                     (optimize (speed 3) (safety 0)))
            (do-regions ((src-j dest-j y1s y1d y2)
                         (src-i dest-i x1s x1d x2) ,@(when backwardp
                                                       `(:backward t)))
              (let-rgba ((r.fg g.fg b.fg a.fg) (aref src-array src-j src-i))
                (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dest-j dest-i))
                  (setf (aref dst-array dest-j dest-i)
                        (octet-blend-function* r.fg g.fg b.fg a.fg
                                               r.bg g.bg b.bg a.bg)))))))))
  (define-blend-image %blend-image nil)
  (define-blend-image %blend-image* t))

(defun blend-image (src-image sx sy width height dst-image dx dy
                    &aux
                    (sx (round sx))
                    (sy (round sy))
                    (dx (round dx))
                    (dy (round dy))
                    (width (round width))
                    (height (round height))
                    (src-array (climi::pattern-array src-image))
                    (dst-array (climi::pattern-array dst-image)))
  "Blend SRC-IMAGE into DST-IMAGE region-wise. Both may be the same image."
  (unless (%check-coords src-array dst-array sx sy dx dy width height)
    (return-from blend-image nil))
  (let ((max-x (+ dx width))
        (max-y (+ dy height)))
    (cond ((eq src-array dst-array)
           #1=(%blend-image src-array dst-array sx sy dx dy max-x max-y))
          ((> sy dy) #1#)
          ((< sy dy) #2=(%blend-image* src-array dst-array sx sy dx dy max-x max-y))
          ((> sx dx) #1#)
          ((< sx dx) #2#)
          (t nil))
    (make-rectangle* dx dy max-x max-y)))

(defun clone-image (image)
  (let ((src-array (climi::pattern-array image)))
    (declare (type argb-pixel-array src-array))
    (make-instance 'climi::%rgba-pattern :array (alexandria:copy-array src-array))))

(defun fill-image (dst-array design x1 y1 x2 y2 clip-region)
  "Blends DESIGN onto IMAGE with a CLIP-REGION."
  (declare (type argb-pixel-array dst-array)
           (type design design))
  (maxf x1 0)
  (maxf y1 0)
  (minf x2 (array-dimension dst-array 1))
  (minf y2 (array-dimension dst-array 0))
  (when (typep design 'bounded-region)
    (with-bounding-rectangle* (a b c d) design
      (maxf x1 a) (maxf y1 b)
      (minf x2 c) (minf y2 d)))
  (when (region-contains-region-p clip-region (make-rectangle* x1 y1 x2 y2))
    (setf clip-region nil))
  (setf x1 (floor x1)
        y1 (floor y1)
        x2 (ceiling x2)
        y2 (ceiling y2))
  (locally
      (declare (optimize (speed 3) (safety 0))
               (type image-index x1 y1 x2 y2)
               (type (or null region) clip-region))
    (let (;; Current mode and color
          old-ink ink mode
          source-rgba source-r source-g source-b source-a)
      (declare (type argb-pixel-array dst-array)
               (type octet old-alpha alpha))
      (flet ((update-ink (i j)
               (setf ink (climi::design-ink* design i j))
               (when (eq old-ink ink)
                 (return-from update-ink))
               (setf old-ink ink)
               (if (typep ink 'standard-flipping-ink)
                   (setf source-rgba (let ((d1 (slot-value ink 'climi::design1))
                                           (d2 (slot-value ink 'climi::design2)))
                                       (logand #x00ffffff
                                               (logxor (climi::%rgba-value d1)
                                                       (climi::%rgba-value d2))))
                         mode :flipping)
                   (let ((ink-rgba (climi::%rgba-value ink)))
                     (if (= 255 (ldb (byte 8 24) ink-rgba))
                         (setf source-rgba ink-rgba
                               mode :copy)
                         (let-rgba ((r g b a) ink-rgba)
                           (setf source-r r
                                 source-g g
                                 source-b b
                                 source-a a
                                 mode :blend)))))))
        (do-regions ((src-j dst-j y1 y1 y2)
                     (src-i dst-i x1 x1 x2))
          (when (or (null clip-region)
                    (region-contains-position-p clip-region src-i src-j))
            (update-ink dst-i dst-j)
            (case mode                  ; do nothing if MODE is NIL
              (:flipping
               (setf (aref dst-array dst-j dst-i) (logxor source-rgba
                                                          (aref dst-array dst-j dst-i))))
              (:copy
               (setf (aref dst-array dst-j dst-i) source-rgba))
              (:blend
               (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dst-j dst-i))
                 (setf (aref dst-array dst-j dst-i)
                       (octet-blend-function* source-r source-g source-b source-a
                                              r.bg     g.bg     b.bg     a.bg)))))))))
    (make-rectangle* x1 y1 x2 y2)))

(defun fill-image-mask (dst-array design x1 y1 x2 y2 clip-region
                        stencil-array stencil-dx stencil-dy)
  "Blends DESIGN onto IMAGE with STENCIL and a CLIP-REGION."
  (declare (type argb-pixel-array dst-array)
           (type design design)
           (type stencil-array stencil-array)
           (type image-index-displacement stencil-dx stencil-dy))
  (maxf x1 0)
  (maxf y1 0)
  (minf x2 (array-dimension dst-array 1))
  (minf y2 (array-dimension dst-array 0))
  (when (typep design 'bounded-region)
    (with-bounding-rectangle* (a b c d) design
      (maxf x1 a) (maxf y1 b)
      (minf x2 c) (minf y2 d)))
  (when (region-contains-region-p clip-region (make-rectangle* x1 y1 x2 y2))
    (setf clip-region nil))
  (setf x1 (floor x1)
        y1 (floor y1)
        x2 (ceiling x2)
        y2 (ceiling y2))
  (locally
      (declare (optimize (speed 3) (safety 0))
               (type image-index x1 y1 x2 y2))
    (let* ((stencil-width-max (1- (array-dimension stencil-array 1)))
           (stencil-height-max (1- (array-dimension stencil-array 0)))
           (old-alpha 255) (alpha 255)
           old-ink ink
           mode
           source-rgba source-r source-g source-b source-a)
      (declare (type image-dimension stencil-width-max stencil-height-max)
               (type argb-pixel-array dst-array)
               (type octet old-alpha alpha))
      (flet ((update-alpha (i j)
               (let ((stencil-x (+ stencil-dx i))
                     (stencil-y (+ stencil-dy j)))
                 (setf alpha (if (and (<= 0 stencil-y stencil-height-max)
                                      (<= 0 stencil-x stencil-width-max))
                                 (aref stencil-array stencil-y stencil-x)
                                 0))))
             (update-ink (i j)
               (setf ink (climi::design-ink* design i j))
               (when (and (eq old-ink ink) (= old-alpha alpha))
                 (return-from update-ink))
               (setf old-alpha alpha
                     old-ink ink)
               (cond ((zerop alpha)
                      (setf mode nil))
                     ((typep ink 'standard-flipping-ink)
                      (setf source-rgba (let ((d1 (slot-value ink 'climi::design1))
                                              (d2 (slot-value ink 'climi::design2)))
                                          (logand #x00ffffff
                                                  (logxor (climi::%rgba-value d1)
                                                          (climi::%rgba-value d2)))))
                      (if (= alpha 255)
                          (setf mode :flipping)
                          (setf source-a alpha
                                mode :flipping/blend)))
                     ((= alpha 255)
                      (let ((ink-rgba (climi::%rgba-value ink)))
                        (if (= 255 (ldb (byte 8 24) ink-rgba))
                            (setf source-rgba ink-rgba
                                  mode :copy)
                            (let-rgba ((r g b a) ink-rgba)
                              (setf source-r r
                                    source-g g
                                    source-b b
                                    source-a a
                                    mode :blend)))))
                     (t                 ; If we get here, ALPHA is [1, 254].
                      (locally (declare (type (integer 1 254) alpha))
                        (let-rgba ((r.fg g.fg b.fg a.fg) (climi::%rgba-value ink))
                          (setf source-r r.fg
                                source-g g.fg
                                source-b b.fg
                                source-a (octet-mult a.fg alpha)
                                ;; SOURCE-A is [0, 254], so never :COPY.
                                mode (if (zerop source-a)
                                         nil
                                         :blend))))))))
        (do-regions ((src-j dst-j y1 y1 y2)
                     (src-i dst-i x1 x1 x2))
          (when (or (null clip-region)
                    (region-contains-position-p clip-region src-i src-j))
            (update-alpha dst-i dst-j)
            (update-ink dst-i dst-j)
            (case mode                  ; do nothing if MODE is NIL
              (:flipping
               (setf (aref dst-array dst-j dst-i) (logxor source-rgba
                                                          (aref dst-array dst-j dst-i))))
              (:flipping/blend
               (let ((dest-rgba (aref dst-array dst-j dst-i)))
                 (let-rgba ((r.bg g.bg b.bg a.bg) dest-rgba)
                   (let-rgba ((r g b) (logxor source-rgba dest-rgba))
                     (setf (aref dst-array dst-j dst-i)
                           (octet-blend-function* r    g    b    source-a
                                                  r.bg g.bg b.bg a.bg))))))
              (:copy
               (setf (aref dst-array dst-j dst-i) source-rgba))
              (:blend
               (let-rgba ((r.bg g.bg b.bg a.bg) (aref dst-array dst-j dst-i))
                 (setf (aref dst-array dst-j dst-i)
                       (octet-blend-function* source-r source-g source-b source-a
                                              r.bg     g.bg     b.bg     a.bg)))))))))
    (make-rectangle* x1 y1 x2 y2)))
