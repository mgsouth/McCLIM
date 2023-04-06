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

(defun medium-target-picture (medium)
  (clx-drawable-picture medium))

(defun medium-source-picture (medium)
  (let ((design (medium-ink medium)))
    (when (clime:indirect-ink-p design)
      (setf design (clime:indirect-ink-ink design)))
    (unless (typep design '(or climi::uniform-compositum color opacity))
      (setf design (compose-in +deep-pink+ (make-opacity .5))))
    (let* ((mirror (medium-drawable medium))
           (pixmap (ensure-help-buffer mirror :uniform
                     (create-pixmap mirror 1 1 32)))
           (picture (ensure-clx-drawable-object (pixmap 'clx-picture)
                      (let* ((display (xlib:drawable-display pixmap))
                             (format (xlib:find-standard-picture-format display :argb32)))
                        (xlib:render-create-picture pixmap :format format :repeat :on)))))
      (multiple-value-bind (r g b a) (clime:color-rgba design)
        (let ((color (make-clx-render-color r g b a)))
          (xlib:render-fill-rectangle picture :src color 0 0 1 1)))
      picture)))

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
      (clx-wipe-picture picture width height +transparent-black+)
      picture)))

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
  (with-clx-graphics (mi gc tr) medium
    (clim-sys:with-lock-held (*draw-font-lock*)
      (draw-glyphs medium mi gc x y string
                   :start start :end end
                   :align-x align-x :align-y align-y
                   :translate #'translate
                   :transformation tr
                   :transform-glyphs transform-glyphs))))



;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-width values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.
(defun draw-glyphs (medium mirror gc x y string
                    &key start end
                      align-x align-y
                      translate direction
                      transformation transform-glyphs
                    &aux (text-style (medium-text-style medium))
                         (port (port medium))
                         (font (text-style-mapping port text-style))
                         (target-picture (medium-target-picture medium))
                         (source-picture (medium-source-picture medium))
                         (clip-mask (xlib:gcontext-clip-mask gc)))
  (declare (optimize (speed 3))
           (ignore translate direction)
           (type #-sbcl (integer 0 #.array-dimension-limit)
                 #+sbcl sb-int:index
                 start end)
           (type string string))

  ;; Sync the picture-clip-mask with that of the gcontext.
  (unless (eq (xlib:picture-clip-mask target-picture) clip-mask)
    (setf (xlib:picture-clip-mask target-picture) clip-mask))

  (when (< (length (the (simple-array (unsigned-byte 32))
                        (clx-render-medium-%buffer% medium)))
           (- end start))
    (setf (clx-render-medium-%buffer% medium)
          (make-array (* 256 (ceiling (- end start) 256))
                      :element-type '(unsigned-byte 32)
                      :adjustable nil :fill-pointer nil)))
  (when (and transform-glyphs
             (not (translation-transformation-p transformation)))
    (setq string (subseq string start end))
    (ecase align-x
      (:left)
      (:center
       (let ((origin-x (text-size medium string :text-style text-style)))
         (decf x (/ origin-x 2.0))))
      (:right
       (let ((origin-x (text-size medium string :text-style text-style)))
         (decf x origin-x))))
    (ecase align-y
      (:top
       (incf y (font-ascent font)))
      (:baseline)
      (:center
       (let* ((ascent (font-ascent font))
              (descent (font-descent font))
              (height (+ ascent descent))
              (middle (- ascent (/ height 2.0s0))))
         (incf y middle)))
      (:baseline*)
      (:bottom
       (decf y (font-descent font))))
    (return-from draw-glyphs
      (%render-transformed-glyphs
       medium font string x y align-x align-y transformation mirror gc
       target-picture source-picture)))
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
    (multiple-value-bind (x y) (transform-position transformation x y)
      (setq x (ecase align-x
                (:left
                 (truncate (+ x 0.5)))
                (:center
                 (truncate (+ (- x (/ origin-x 2.0)) 0.5)))
                (:right
                 (truncate (+ (- x origin-x) 0.5)))))
      (setq y (ecase align-y
                (:top
                 (truncate (+ y (font-ascent font) 0.5)))
                (:baseline
                 (truncate (+ y 0.5)))
                (:center
                 (let* ((ascent (font-ascent font))
                        (descent (font-descent font))
                        (height (+ ascent descent))
                        (middle (- ascent (/ height 2.0s0))))
                   (truncate (+ y middle 0.5))))
                (:baseline*
                 (truncate (+ y 0.5)))
                (:bottom
                 (truncate (+ y (- (font-descent font)) 0.5)))))
      (when (and (typep x '(signed-byte 16))
                 (typep y '(signed-byte 16)))
        (xlib:render-composite-glyphs target-picture
                                      glyph-set
                                      source-picture
                                      x y
                                      glyph-ids
                                      :end (- end start))))))

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
                                   tr mirror gc
                                   target-picture source-picture
                                   &aux (end (length string)))
  (declare (ignore gc align-x align-y))
  (loop
    with glyph-tr = (multiple-value-bind (x0 y0)
                        (transform-position tr 0 0)
                      (compose-transformation-with-translation tr (- x0) (- y0)))
    ;; for rendering one glyph at a time
    with current-x = x
    with current-y = y
    ;; ~
    with glyph-ids = (clx-render-medium-%buffer% medium)
    with glyph-set = (make-glyph-set (xlib:drawable-display mirror))
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
           (xlib:render-composite-glyphs target-picture glyph-set source-picture
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
           (xlib:render-composite-glyphs target-picture glyph-set source-picture
                                         current-x current-y
                                         glyph-ids :start i* :end (1+ i*))))
       (xlib:render-free-glyphs glyph-set (subseq glyph-ids 0 (1+ i*)))
    #+ (or)
    ;; rendering all glyphs at once
    ;; This solution is correct in principle, but advance-width and
    ;; advance-height are victims of rounding errors and they don't hold the
    ;; line for longer text in case of rotations and other hairy
    ;; transformations. That's why we take our time and render one glyph at a
    ;; time. -- jd 2018-10-04
       (destructuring-bind (source-picture source-pixmap)
           (gcontext-picture mirror gc)
         (declare (ignore source-pixmap))

         (with-round-positions (tr x y)
           (when (and (typep x '(signed-byte 16))
                      (typep y '(signed-byte 16)))
             (xlib:render-composite-glyphs (drawable-picture mirror)
                                           glyph-set
                                           source-picture
                                           x y
                                           glyph-ids :start 0 :end end))))
    finally
       (xlib:render-free-glyph-set glyph-set)))
