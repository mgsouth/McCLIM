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

(defclass clx-render-medium (ttf-medium-mixin clx-medium)
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

(defun medium-stencil-picture (medium)
  (let* ((mirror (medium-drawable medium))
         (width (mirror-width mirror))
         (height (mirror-height mirror))
         (pixmap (ensure-help-buffer mirror :stencil
                   (create-pixmap mirror width height 8))))
    (resize-pixmap pixmap width height)
    (let ((picture (ensure-clx-drawable-object (pixmap 'clx-picture)
                     (let* ((display (xlib:drawable-display pixmap))
                            (format (xlib:find-standard-picture-format display :a8)))
                       (xlib:render-create-picture pixmap :format format)))))
      (ensure-clx-drawable-object (pixmap :gcontext)
        (xlib:create-gcontext :drawable pixmap
                              :function boole-1
                              :foreground #xff
                              :background #x00
                              :fill-style :solid))
      picture)))

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
         (idata  (climi::%collapse-pattern pattern 0 0 width height))
         (pixmap (xlib:create-pixmap :drawable drawable
                                     :width width
                                     :height height
                                     :depth 32))
         (gcontext (xlib:create-gcontext :drawable pixmap))
         (ximage   (xlib:create-image :width  width
                                      :height height
                                      :depth 32
                                      :bits-per-pixel 32
                                      :data (pattern-array idata))))
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
           (flipper (ldb (byte 24 0)
                         (logxor (climi::%rgba-value (flipping-ink-design1 design))
                                 (climi::%rgba-value (flipping-ink-design2 design))))))
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
                 (xlib:render-create-picture pixmap :format format)))))
      (with-bounding-rectangle* (x1 y1 x2 y2) (medium-device-region medium)
        (clx-fill-composite :src
                            (clx-drawable-picture mirror)
                            nil
                            picture
                            +identity-transformation+
                            x1 y1 x2 y2)
        (invert-drawable (clx-drawable pixmap)
                         (logxor (climi::%rgba-value (flipping-ink-design1 design))
                                 (climi::%rgba-value (flipping-ink-design2 design)))
                         w h))
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

(defmacro with-render-context ((source stencil target) medium &body body)
  (let ((cont (gensym))
        (svar (or stencil (gensym))))
    `(flet ((,cont (,source ,svar ,target)
              ,@(and (null stencil) `((declare (ignore ,svar))))
              ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-render-context (function ,cont) ,medium ',stencil))))

(defun invoke-with-render-context (cont medium stencilp)
  (let ((target  (medium-target-picture medium))
        (source  (medium-source-picture medium (medium-ink medium)))
        (stencil (and stencilp (medium-stencil-picture medium))))
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
             (when stencilp
               (setf (xlib:picture-clip-mask stencil)
                     (xlib:picture-clip-mask target))
               (with-bounding-rectangle* (x1 y1 x2 y2)
                   (medium-device-region medium)
                 (clx-fill-rectangle :src +transparent-black+ stencil
                                     +identity-transformation+ x1 y1 x2 y2)))
             (funcall cont source stencil target))
        (mapc #'funcall ^cleanup)))))

(defmethod invoke-with-clx-graphics (cont (medium clx-render-medium))
  (when (legacy-ink-p (medium-ink medium))
    (return-from invoke-with-clx-graphics
      (call-next-method)))
  (with-render-context (source stencil target) medium
    (let* ((mi (clx-drawable stencil))
           (gc (ensure-clx-drawable-object (mi :gcontext)))
           (tr (medium-device-transformation medium)))
      (update-line-style gc medium (medium-line-style medium))
      (funcall cont mi gc tr)
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (medium-device-region medium)
        (clx-fill-composite :over source stencil target
                            +identity-transformation+ x1 y1 x2 y2)))))

#+ (or)
;;; These methods work althought drawing is noticeably slow. The culpirt is
;;; CLX itself - sending coord-seq is 100s slower than "normal" drawing
;;; routines even for dummy calls with hardcoded coordinates. -- jd 2023-04-13
(progn
  (defmethod medium-draw-polygon* ((medium clx-render-medium) coord-seq closed filled)
    (if (not filled)
        (call-next-method)
        (with-render-context (source stencil target) medium
          (clx-fill-polygon :over source target
                            (xlib::picture-format stencil)
                            (medium-device-transformation medium)
                            coord-seq))))

  (defmethod medium-draw-ellipse* ((medium clx-render-medium) cx cy
                                   rdx1 rdy1 rdx2 rdy2
                                   eta1 eta2 filled)
    (if (not filled)
        (call-next-method)
        (with-render-context (source stencil target) medium
          (clx-fill-trifan :over source target
                           (xlib::picture-format stencil)
                           (medium-device-transformation medium)
                           (climi::polygonalize-ellipse cx cy rdx1 rdy1 rdx2 rdy2
                                                        eta1 eta2 :filled t))))))

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))
(defmethod medium-draw-text* ((medium clx-render-medium) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs
                              &aux (end (if (null end)
                                            (length string)
                                            (min end (length string)))))
  (declare (ignore toward-x toward-y))
  (when (or (alexandria:emptyp string) (>= start end))
    (return-from medium-draw-text*))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (draw-glyphs medium x y string
                 :start start :end end
                 :align-x align-x :align-y align-y
                 :translate #'translate
                 :transformation (medium-device-transformation medium)
                 :transform-glyphs transform-glyphs)))



;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-width values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.
(defun draw-glyphs (medium x y string
                    &key start end
                      align-x align-y
                      translate direction
                      transformation transform-glyphs
                    &aux (text-style (medium-text-style medium))
                         (port (port medium))
                         (font (text-style-mapping port text-style)))
  (declare (optimize (speed 3))
           (ignore translate direction)
           (type #-sbcl (integer 0 #.array-dimension-limit)
                 #+sbcl sb-int:index
                 start end)
           (type string string))
  (when (< (length (the (simple-array (unsigned-byte 32))
                        (clx-render-medium-%buffer% medium)))
           (- end start))
    (setf (clx-render-medium-%buffer% medium)
          (make-array (* 256 (ceiling (- end start) 256))
                      :element-type '(unsigned-byte 32)
                      :adjustable nil :fill-pointer nil)))
  (macrolet ((fix-alignment ()
               ;; This macro deliberely captures ORIGIN-X.
               `(progn
                  (ecase align-x
                    (:left)
                    (:center (decf x (/ origin-x 2.0)))
                    (:right  (decf x origin-x)))
                  (ecase align-y
                    ((:baseline :baseline*))
                    (:top    (incf y (font-ascent font)))
                    (:center (incf y (/ (- (font-ascent font) (font-descent font)) 2.0)))
                    (:bottom (decf y (font-descent font)))))))
    (when (and transform-glyphs
               (not (translation-transformation-p transformation)))
      (setq string (subseq string start end))
      (symbol-macrolet ((origin-x (text-size medium string :text-style text-style)))
        (fix-alignment))
      (return-from draw-glyphs
        (%render-transformed-glyphs
         medium font string x y align-x align-y transformation)))
    (let ((glyph-ids (clx-render-medium-%buffer% medium))
          (glyph-set (ensure-glyph-set port))
          (origin-x 0))
      (loop
        with char = (char string start)
        with i* = 0
        for i from (1+ start) below end
        as next-char = (char string i)
        as next-char-code = (char-code next-char)
        as code = (dpb next-char-code (byte #.(ceiling (log char-code-limit 2))
                                            #.(ceiling (log char-code-limit 2)))
                       (char-code char))
        do
           (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
                 (the (unsigned-byte 32) (font-glyph-id font code)))
           (setf char next-char)
           (incf i*)
           (incf origin-x (font-glyph-dx font code))
        finally
           (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
                 (the (unsigned-byte 32)
                      (font-glyph-id font (char-code char))))
           (incf origin-x (font-glyph-dx font (char-code char))))
      (with-transformed-position (transformation x y)
        (fix-alignment)
        (setf x (truncate (+ x .5)))
        (setf y (truncate (+ y .5)))
        (when (and (typep x 'clx-coordinate)
                   (typep y 'clx-coordinate))
          ;; When the source is not uniform then render-compsite-glyphs is
          ;; much slower than first drawing on a stencil and then filling the
          ;; composite. Both paths are correct for any case. -- jd 2023-04-13
          (if (uniform-ink-p (medium-ink medium))
              (with-render-context (source nil target) medium
                (xlib:render-composite-glyphs target glyph-set source
                                              x y glyph-ids :end (- end start)))
              (with-render-context (source stencil target) medium
                (with-bounding-rectangle* (x1 y1 x2 y2) (medium-device-region medium)
                  (let ((brush (medium-stencil-brush medium)))
                    (xlib:render-composite-glyphs stencil glyph-set brush
                                                  x y glyph-ids :end (- end start))
                    (clx-fill-composite :over source stencil target
                                        +identity-transformation+ x1 y1 x2 y2))))))))))

(defmethod font-generate-glyph :around
    ((port clx-ttf-port) font code &key glyph-set)
  (declare (ignore code font))
  (let* ((info (call-next-method))
         (pixarray (glyph-info-pixarray info))
         (x1 (glyph-info-left info))
         (y1 (glyph-info-top info))
         (dx (glyph-info-advance-width info))
         (dy (glyph-info-advance-height info)))
    (when (= (array-dimension pixarray 0) 0)
      (setf pixarray (make-array (list 1 1)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    ;; We negate X1 because we want to start drawing array X1 pixels /after/ the
    ;; pen (pixarray contains only a glyph without its left-side bearing). TOP
    ;; is not negated because glyph coordiantes are in the first quardant (while
    ;; array's are in the fourth). -- jd 2018-09-29
    (let ((glyph-set (or glyph-set (ensure-glyph-set port)))
          (glyph-id (draw-glyph-id port)))
      (xlib:render-add-glyph glyph-set glyph-id
                             :data pixarray
                             :x-origin (- x1) :y-origin y1
                             :x-advance dx :y-advance dy)
      (setf (glyph-info-id info) glyph-id))
    info))

;;; Transforming glyphs is very inefficient because we don't cache them.
(defun %render-transformed-glyphs (medium font string x y align-x align-y
                                   tr &aux (end (length string)))
  (declare (ignore align-x align-y))
  (with-render-context (source nil target) medium
    (loop
      with glyph-tr = (multiple-value-bind (x0 y0)
                          (transform-position tr 0 0)
                        (compose-transformation-with-translation tr (- x0) (- y0)))
      ;; for rendering one glyph at a time
      with current-x = x
      with current-y = y
      ;; ~
      with glyph-ids = (clx-render-medium-%buffer% medium)
      with glyph-set = (make-glyph-set (clx-drawable-display medium))
      with char = (char string 0)
      with i* = 0
      for i from 1 below end
      as next-char = (char string i)
      as next-char-code = (char-code next-char)
      as code = (dpb next-char-code (byte #.(ceiling (log char-code-limit 2))
                                          #.(ceiling (log char-code-limit 2)))
                     (char-code char))
      as glyph-info = (font-generate-glyph (port medium) font code
                                           :transformation glyph-tr
                                           :glyph-set glyph-set)
      do
         (setf (aref (the (simple-array (unsigned-byte 32))
                          glyph-ids)
                     i*)
               (the (unsigned-byte 32)
                    (glyph-info-id glyph-info)))
      do ;; rendering one glyph at a time
         (with-round-positions (tr current-x current-y)
           (when (and (typep current-x '(signed-byte 16))
                      (typep current-y '(signed-byte 16)))
             (xlib:render-composite-glyphs target glyph-set source
                                           current-x current-y
                                           glyph-ids :start i* :end (1+ i*))))
         ;; INV advance values are untransformed - see FONT-GENERATE-GLYPH.
         (incf current-x (glyph-info-advance-width* glyph-info))
         (incf current-y (glyph-info-advance-height* glyph-info))
      do
         (setf char next-char)
         (incf i*)
      finally
         (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) i*)
               (the (unsigned-byte 32)
                    (glyph-info-id
                     (font-generate-glyph (port medium) font (char-code char)
                                          :transformation glyph-tr
                                          :glyph-set glyph-set))))
      finally
         ;; rendering one glyph at a time (last glyph)
         (with-round-positions (tr current-x current-y)
           (when (and (typep current-x '(signed-byte 16))
                      (typep current-y '(signed-byte 16)))
             (xlib:render-composite-glyphs target glyph-set source
                                           current-x current-y
                                           glyph-ids :start i* :end (1+ i*))))
         (xlib:render-free-glyphs glyph-set glyph-ids :start 0 :end (1+ i*))
      #+ (or)
      ;; rendering all glyphs at once
      ;;
      ;; This solution is correct in principle, but advance-width and
      ;; advance-height are victims of rounding errors and they don't hold the
      ;; line for longer text in case of rotations and other hairy transforms.
      ;; That's why we render one glyph at a time. -- jd 2018-10-04
         (with-round-positions (tr x y)
           (when (and (typep x '(signed-byte 16))
                      (typep y '(signed-byte 16)))
             (xlib:render-composite-glyphs target glyph-set source
                                           x y glyph-ids :start 0 :end end)))
      finally
         (xlib:render-free-glyph-set glyph-set))))
