;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2003 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2018-2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-clx)

(defclass clx-render-medium (climi::multiline-medium-mixin
                             ttf-medium-mixin clx-medium)
  ((%buffer% ;; stores the drawn string glyph ids.
    :initform (make-array 1024
                          :element-type '(unsigned-byte 32)
                          :adjustable nil
                          :fill-pointer nil)
    :accessor clx-render-medium-%buffer%
    :type (simple-array (unsigned-byte 32)))))

(defun uniform-ink-p (ink)
  (typecase ink
    (color t)
    (uniform-compositum t)
    (indirect-ink (uniform-ink-p (indirect-ink-ink ink)))
    (opacity (uniform-ink-p (compose-in +foreground-ink+ ink)))
    (otherwise nil)))

(defun legacy-ink-p (ink)
  (typecase ink
    (color t)
    (indirect-ink (legacy-ink-p (indirect-ink-ink ink)))
    (opacity (legacy-ink-p (compose-in +foreground-ink+ ink)))
    (standard-flipping-ink t)
    (otherwise nil)))

(defun medium-target-picture (medium)
  (clx-drawable-picture medium))

(defun medium-stencil-picture (medium x1 y1 x2 y2)
  (let* ((mirror (medium-drawable medium))
         (width  (ceiling x2))
         (height (ceiling y2))
         (pixmap (ensure-help-buffer mirror :stencil
                   (create-pixmap mirror width height 8))))
    (resize-pixmap pixmap width height)
    (ensure-clx-drawable-object (pixmap :gcontext)
      (xlib:create-gcontext :drawable pixmap
                            :function boole-1
                            :foreground #xff
                            :background #x00
                            :fill-style :solid))
    (let ((stencil (ensure-clx-drawable-object (pixmap 'clx-picture)
                     (let* ((display (xlib:drawable-display pixmap))
                            (format (xlib:find-standard-picture-format display :a8)))
                       (xlib:render-create-picture pixmap :format format)))))
      (clx-fill-rectangle :src +transparent-black+ stencil +identity-transformation+ x1 y1 x2 y2)
      stencil)))

(defun medium-stencil-format (medium)
  (let* ((mirror  (clx-drawable medium))
         (display (xlib:drawable-display mirror))
         (format  (xlib:find-standard-picture-format display :a8)))
    format))

(defun medium-stencil-brush (medium)
  (let* ((pixmap (ensure-help-buffer (medium-drawable medium) :brush
                   (create-pixmap medium 1 1 8)))
         (picture
           (ensure-clx-drawable-object (pixmap 'clx-picture)
             (let* ((display (clx-drawable-display medium))
                    (format (xlib:find-standard-picture-format display :a8)))
               (xlib:render-create-picture pixmap :format format :repeat :on)))))
    (clx-wipe-picture picture 1 1 +solid-black+)
    picture))

;;; FIXME this method is wrong because we collapse the pattern's designs along
;;; with the pattern, while "14.2 Patterns and Stencils" says:
;;;
;;;  Applying a coordinate transformation to a pattern does not affect the
;;;  designs that make up the pattern. It only changes the position, size, and
;;;  shape of the cells' holes, allowing different portions of the designs in
;;;  the cells to show through. Consequently, applying make-rectangular-tile
;;;  to a pattern of nonuniform designs can produce a different appearance in
;;;  each tile.
;;;
;;; The "right thing" could be achieved by composing each design over the
;;; source pattern with masks for each array cell. -- jd 2021-01-25
(defun make-clx-render-pixmap (medium pattern)
  (let* ((drawable (clx-drawable medium))
         (width  (ceiling (pattern-width pattern)))
         (height (ceiling (pattern-height pattern)))
         (idata  (make-clx-render-image/argb32 pattern))
         (pixmap (xlib:create-pixmap :drawable drawable
                                     :width width
                                     :height height
                                     :depth 32))
         (gcontext (xlib:create-gcontext :drawable pixmap))
         (ximage   (xlib:create-image :width  width
                                      :height height
                                      :depth 32
                                      :bits-per-pixel 32
                                      :data idata)))
    (put-image-recursively pixmap gcontext ximage width height 0 0)
    (xlib:free-gcontext gcontext)
    pixmap))

(defun clx-render-pattern-picture (medium pattern transformation repeat)
  (let* ((pixmap
           (ensure-gethash pattern (port-design-cache (port medium))
             (make-clx-render-pixmap medium pattern)))
         (picture
           (ensure-clx-drawable-object (pixmap 'clx-picture)
             (let* ((display (clx-drawable-display pixmap))
                    (format (xlib:find-standard-picture-format display :argb32)))
               (xlib:render-create-picture pixmap :format format)))))
    (setf (xlib:picture-repeat picture) repeat)
    (let* ((ntr (medium-native-transformation medium))
           (etr (compose-transformations ntr transformation)))
      (transform-picture etr picture))
    picture))

(defun invert-drawable (pixmap flipper w h)
  (let ((gcontext (ensure-clx-drawable-object (pixmap :flipper)
                    (xlib:create-gcontext :drawable pixmap
                                          :function boole-xor
                                          :fill-style :solid))))
    (setf (xlib:gcontext-foreground gcontext) flipper)
    (setf (xlib:gcontext-background gcontext) flipper)
    (xlib:draw-rectangle pixmap gcontext 0 0 w h t)))

;;; Porter-Duff XOR is _not_ a bitwise XOR. It works only on alpha values. To
;;; have a flipping ink we need to copy the target picture and do the bitwise
;;; xor manually. We use an intermediate pixmap because xlib:copy-area
;;; requires the same depth between drawables, and XRender won't work with a
;;; window as a source picture.
(defun clx-render-flipping-picture (medium design)
  (flet ((make-pixmap (mirror name w h depth)
           (let ((p (ensure-help-buffer mirror name
                      (create-pixmap mirror w h depth))))
             (resize-pixmap p w h))))
    (let* ((mirror (medium-drawable medium))
           (w (mirror-width mirror))
           (h (mirror-height mirror))
           (pixmap-24 (make-pixmap mirror :flipper-24 w h 24))
           (pixmap-32 (make-pixmap mirror :flipper-32 w h 32))
           (%pixmap24 (clx-drawable pixmap-24))
           (picture-24
             (ensure-clx-drawable-object (pixmap-24 'clx-picture)
               (let* ((display (clx-drawable-display medium))
                      (format (xlib:find-standard-picture-format display :rgb24)))
                 (xlib:render-create-picture pixmap-24 :format format))))
           (picture-32
             (ensure-clx-drawable-object (pixmap-32 'clx-picture)
               (let* ((display (clx-drawable-display medium))
                      (format (xlib:find-standard-picture-format display :argb32)))
                 (xlib:render-create-picture pixmap-32 :format format))))
           (design1 (flipping-ink-design1 design))
           (design2 (flipping-ink-design2 design))
           (flipper (ldb (byte 24 0)
                         (logxor (climi::%pattern-rgba-value design1 0 0)
                                 (climi::%pattern-rgba-value design2 0 0)))))
      (%drawable-copy-area (clx-drawable mirror) 0 0 w h %pixmap24 0 0)
      (invert-drawable %pixmap24 flipper w h)
      (with-bounding-rectangle* (x1 y1 x2 y2) (medium-device-region medium)
        (clx-fill-composite :src picture-24 nil picture-32
                            +identity-transformation+ x1 y1 x2 y2))
      picture-32)))

;;; This version is a straightforward and reasonably fast implementation of
;;; the flipping ink with an alpha channel. It is nice except for the fact
;;; that it doesn't work. XRender accepts a picture associated with a window
;;; as a source but instead of the contents of the window it contains noise.
;;; This function may be viable when we switch to triple buffering and the
;;; mirror is always a pixmap (not a window). -- jd 2023-04-12
#+ (or)
(defun clx-render-flipping-picture (medium design)
  (flet ((make-pixmap (mirror w h)
           (let ((p (ensure-help-buffer mirror :flipper
                      (create-pixmap mirror w h 32))))
             (resize-pixmap p w h))))
    (let* ((mirror (medium-drawable medium))
           (w (mirror-width mirror))
           (h (mirror-height mirror))
           (pixmap (make-pixmap mirror w h))
           (picture
             (ensure-clx-drawable-object (pixmap 'clx-picture)
               (let* ((display (clx-drawable-display medium))
                      (format (xlib:find-standard-picture-format display :argb32)))
                 (xlib:render-create-picture pixmap :format format))))
           (design1 (flipping-ink-design1 design))
           (design2 (flipping-ink-design2 design))
           (flipper (logxor (climi::%pattern-rgba-value design1 0 0)
                            (climi::%pattern-rgba-value design2 0 0))))
      (with-bounding-rectangle* (x1 y1 x2 y2) (medium-device-region medium)
        (clx-fill-composite :src
                            (clx-drawable-picture mirror)
                            nil
                            picture
                            +identity-transformation+
                            x1 y1 x2 y2)
        (invert-drawable (clx-drawable pixmap) flipper w h))
      picture)))

;;; KLUDGE by default we maintain only a single picture for colors and fill it
;;; on demand with the RGBA value. Sometimes though we need to hold two unique
;;; pictures to blend them together (when necessary). That's why we allow to
;;; overwrite the uniform picture key.
(defvar *reuse-uniform-picture-p* nil)

(defun clx-render-uniform-picture (medium design)
  (let* ((pixmap (ensure-help-buffer (medium-drawable medium)
                     (if *reuse-uniform-picture-p* :uniform design)
                   (create-pixmap medium 1 1 32)))
         (picture
           (ensure-clx-drawable-object (pixmap 'clx-picture)
             (let* ((display (clx-drawable-display medium))
                    (format (xlib:find-standard-picture-format display :argb32)))
               (xlib:render-create-picture pixmap :format format :repeat :on)))))
    (multiple-value-bind (r g b a) (clime:color-rgba design)
      (let ((color (make-clx-render-color r g b a)))
        (xlib:render-fill-rectangle picture :src color 0 0 1 1)))
    picture))

;;; FIXME this could be probably optimized further (drawable-width calls etc).
(defun clx-render-over-compositum (medium design)
  (let* ((*reuse-uniform-picture-p* nil)
         (fg (medium-source-picture medium (compositum-foreground design)))
         (bg (medium-source-picture medium (compositum-background design))))
    (let ((w (max (xlib:drawable-width  (clx-drawable fg))
                  (xlib:drawable-width  (clx-drawable bg))))
          (h (max (xlib:drawable-height (clx-drawable fg))
                  (xlib:drawable-height (clx-drawable bg))))
          (repeat (if (or (eq (xlib:picture-repeat fg) :on)
                          (eq (xlib:picture-repeat bg) :on))
                      :on :off)))
      (let ((pixmap (ensure-help-buffer (medium-drawable medium) :over
                      (create-pixmap medium w h 32))))
        (resize-pixmap pixmap w h)
        (let ((picture
                (ensure-clx-drawable-object (pixmap 'clx-picture)
                  (let* ((display (clx-drawable-display medium))
                         (format (xlib:find-standard-picture-format display :argb32)))
                    (xlib:render-create-picture pixmap :format format :repeat repeat)))))
          (clx-fill-composite :src  bg nil picture +identity-transformation+ 0 0 w h)
          (clx-fill-composite :over fg nil picture +identity-transformation+ 0 0 w h)
          picture)))))

(defgeneric medium-source-picture (medium ink)
  (:method ((medium clx-render-medium) (ink indirect-ink))
    (medium-source-picture medium (indirect-ink-ink ink)))
  (:method ((medium clx-render-medium) (design opacity))
    (medium-source-picture medium (compose-in +foreground-ink+ design)))
  (:method ((medium clx-render-medium) (design over-compositum))
    (clx-render-over-compositum medium design))
  (:method ((medium clx-render-medium) (design color))
    (clx-render-uniform-picture medium design))
  (:method ((medium clx-render-medium) (design uniform-compositum))
    (clx-render-uniform-picture medium design))
  (:method ((medium clx-render-medium) (design standard-flipping-ink))
    (clx-render-flipping-picture medium design))
  (:method ((medium clx-render-medium) (pattern transformed-pattern))
    (let* ((design (transformed-design-design pattern))
           (transf (transformed-design-transformation pattern))
           (picture (if (typep design 'clime:rectangular-tile)
                        (clx-render-pattern-picture medium design transf :on)
                        (clx-render-pattern-picture medium design transf :off))))
      picture))
  (:method ((medium clx-render-medium) (design rectangular-tile))
    (clx-render-pattern-picture medium design +identity-transformation+ :on))
  (:method ((medium clx-render-medium) (design pattern))
    (clx-render-pattern-picture medium design +identity-transformation+ :off))
  (:method ((medium clx-render-medium) design)
    (medium-source-picture medium (compose-in +deep-pink+ (make-opacity .75)))))

(defmacro with-render-context ((source target) medium &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,source ,target) ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-render-context (function ,cont) ,medium))))

(defun invoke-with-render-context (cont medium)
  (let ((target  (medium-target-picture medium))
        (source  (medium-source-picture medium (medium-ink medium))))
    (let* (;; "legacy" clipping.
           (^cleanup nil)
           (drawable (clx-drawable medium))
           (gcontext (ensure-clx-drawable-object (drawable :gcontext)
                       (xlib:create-gcontext :drawable drawable))))
      (unwind-protect
           ;; FIXME write an optimized and antialiased equivalent of
           ;; %set-gc-clipping-region for pictures.
           (progn
             (%set-gc-clipping-region medium gcontext)
             (setf (xlib:picture-clip-mask target)
                   (xlib:gcontext-clip-mask gcontext))
             (funcall cont source target))
        (mapc #'funcall ^cleanup)))))

(defmethod invoke-with-clx-graphics (cont (medium clx-render-medium))
  (when (legacy-ink-p (medium-ink medium))
    (return-from invoke-with-clx-graphics
      (call-next-method)))
  (with-render-context (source target) medium
    (with-bounding-rectangle* (x1 y1 x2 y2) (medium-device-region medium)
      (let ((stencil (medium-stencil-picture medium x1 y1 x2 y2)))
        (let* ((mi (clx-drawable stencil))
               (gc (ensure-clx-drawable-object (mi :gcontext)))
               (tr (medium-device-transformation medium)))
          (update-line-style gc medium (medium-line-style medium))
          (funcall cont mi gc tr)
          (clx-fill-composite :over source stencil target
                              +identity-transformation+ x1 y1 x2 y2))))))

#+ (or)
;;; These methods work althought drawing is noticeably slow. The culpirt is
;;; CLX itself - sending coord-seq is 100s slower than "normal" drawing
;;; routines even for dummy calls with hardcoded coordinates. -- jd 2023-04-13
(progn
  (defmethod medium-draw-polygon* ((medium clx-render-medium) coords closed filled)
    (if (not filled)
        (call-next-method)
        (with-render-context (source target) medium
          (let ((format (medium-stencil-format medium))
                (transf (medium-device-transformation medium)))
            (clx-fill-polygon :over source target format transf coords)))))

  (defmethod medium-draw-ellipse* ((medium clx-render-medium) cx cy
                                   rdx1 rdy1 rdx2 rdy2
                                   eta1 eta2 filled)
    (if (not filled)
        (call-next-method)
        (with-render-context (source target) medium
          (let ((format (medium-stencil-format medium))
                (transf (medium-device-transformation medium))
                (coords (climi::polygonalize-ellipse cx cy rdx1 rdy1 rdx2 rdy2
                                                     eta1 eta2 :filled t)))
            (clx-fill-trifan :over source target format transf coords))))))

;;; We don't need to use the stencil when the transformation is translation and
;;; the ink is uniform[*].
;;;
;;; [*] Rendering with non-uniform ink through the stencil is faster.
(defun draw-glyphs/fast (glyph-set glyph-ids length
                         medium x y transformation)
  (declare (optimize (speed 3))
           (type index length))
  (with-transformed-position (transformation x y)
    (setf x (truncate (+ x .5)))
    (setf y (truncate (+ y .5)))
    (when (and (typep x 'clx-coordinate)
               (typep y 'clx-coordinate))
      (with-render-context (source target) medium
        (xlib:render-composite-glyphs target glyph-set source
                                      x y glyph-ids :end length)))))

;;; This function first renders untransformed text in the stencil and then
;;; composes it over the target with the transformation. The result is a little
;;; blurred, but that's expected when transforming text with the alpha channel.
(defun draw-glyphs/fine (glyph-set glyph-ids length
                         medium x y transformation
                         xmin ymin xmax ymax)
  (declare (optimize (speed 3))
           (type index length)
           (type real x y xmin ymin xmax ymax))
  ;; Compute the stencil width and height. It is a bit tricky, because the
  ;; rectangle in the stencil coordinates after a transformation may be
  ;; non-rectilinear rectangle, so the bounding rectangle of the result must be
  ;; untransformed back to the stencil coordinates.
  (let* ((transf (compose-translation-with-transformation transformation xmin ymin))
         (rect1 (make-rectangle* 0 0 (- xmax xmin) (- ymax ymin)))
         (rect2 (transform-region transf rect1))
         (rect3 (untransform-region transf (bounding-rectangle rect2))))
    (let* ((x0 (round-coordinate (- x xmin)))
           (y0 (round-coordinate (- y ymin)))
           (sw (round-coordinate (bounding-rectangle-max-x rect3)))
           (sh (round-coordinate (bounding-rectangle-max-y rect3)))
           (stencil (medium-stencil-picture medium 0 0 sw sh))
           (brush (medium-stencil-brush medium)))
      (xlib:render-composite-glyphs stencil glyph-set brush x0 y0 glyph-ids :end length)
      (transform-picture transf stencil)
      ;; (xlib::render-set-filter stencil "nearest")
      (xlib::render-set-filter stencil "bilinear")
      (with-render-context (source target) medium
        (with-bounding-rectangle* (x y :width w :height h) rect2
          (setf x (round-coordinate x)
                y (round-coordinate y)
                w (min #xffff (round-coordinate w))
                h (min #xffff (round-coordinate h)))
          (xlib:render-composite :over source stencil target x y x y x y w h)))
      (transform-picture +identity-transformation+ stencil))))

(defmethod medium-draw-text* ((medium clx-render-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start)
           (type string string))
  (climi::orf end (length string))
  (when (or (alexandria:emptyp string) (>= start end))
    (return-from medium-draw-text*))
  ;; KLUDGE CLX has an output buffer limit that may be quite easily exceeded.
  ;; We cap the text larger than 4096. -- jd-2024-02-19
  (when (> (- end start) 4096)
    (setf end (+ start 4096)))
  (when (< (length (the (simple-array (unsigned-byte 32))
                        (clx-render-medium-%buffer% medium)))
           (- end start))
    (setf (clx-render-medium-%buffer% medium)
          (make-array (- end start)
                      :element-type '(unsigned-byte 32)
                      :adjustable nil :fill-pointer nil)))
  (let* ((port (port medium))
         (text-style (medium-text-style medium))
         (font (text-style-mapping port text-style))
         (glyph-ids (clx-render-medium-%buffer% medium))
         (glyph-set (ensure-glyph-set port))
         (direction (medium-line-direction medium))
         (transformation (medium-text-transformation
                          medium x y toward-x toward-y
                          direction transform-glyphs)))
    (fill-glyph-indexes medium font string start end glyph-ids)
    (multiple-value-bind (x y xmin ymin xmax ymax)
        (font-prepare-glyphs medium font string start end align-x align-y)
      ;; When the source is not uniform then render-compsite-glyphs is much
      ;; slower than first drawing on a stencil and then filling the
      ;; composite. Both paths are correct for any case. -- jd 2023-04-13
      (if (and (translation-transformation-p transformation)
               (uniform-ink-p (medium-ink medium)))
          (draw-glyphs/fast glyph-set glyph-ids (- end start)
                            medium x y transformation)
          (draw-glyphs/fine glyph-set glyph-ids (- end start)
                            medium x y transformation
                            xmin ymin xmax ymax)))))
