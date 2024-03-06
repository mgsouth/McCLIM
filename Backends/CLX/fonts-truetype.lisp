(in-package #:clim-clx)

(defclass clx-ttf-port (ttf-port-mixin clim-clx:clx-render-port)
  ((glyph-set
    :initform nil
    :accessor glyph-set)
   (next-glyph-id
    :initform 0
    :accessor next-glyph-id)))

(defmethod find-port-type ((port (eql :clx-ttf)))
  (values 'clx-ttf-port (nth-value 1 (find-port-type :clx))))

(defun make-glyph-set (display)
  (xlib:render-create-glyph-set
   (xlib:find-standard-picture-format display :a8)))

(defun ensure-glyph-set (port)
  (or (glyph-set port)
      (setf (glyph-set port) (make-glyph-set (clx-port-display port)))))

(defun free-glyph-set (port)
  (alexandria:when-let ((glyph-set (glyph-set port)))
    (xlib:render-free-glyph-set glyph-set)
    (setf (glyph-set port) nil)))

(defun draw-glyph-id (port)
  (incf (next-glyph-id port)))

(defmethod font-generate-glyph :around
    ((port clx-ttf-port) font code direction transformation)
  (declare (ignore font code direction transformation))
  (let* ((info (call-next-method))
         (pixarray (glyph-info-pixarray info))
         (x0 (glyph-info-origin-x info))
         (y0 (glyph-info-origin-y info))
         (dx (glyph-info-advance-dx info))
         (dy (glyph-info-advance-dy info)))
    (when (= (array-dimension pixarray 0) 0)
      (setf pixarray (make-array (list 1 1)
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)))
    ;; We negate X1 because we want to start drawing array X1 pixels /after/ the
    ;; pen (pixarray contains only a glyph without its left-side bearing). TOP
    ;; is not negated because glyph coordiantes are in the first quardant (while
    ;; array's are in the fourth). -- jd 2018-09-29
    (let ((glyph-set (ensure-glyph-set port))
          (glyph-id (draw-glyph-id port)))
      (xlib:render-add-glyph glyph-set glyph-id
                             :data pixarray
                             :x-origin x0 :y-origin y0
                             :x-advance dx :y-advance dy)
      (setf (glyph-info-id info) glyph-id))
    info))

;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-dx values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))

(defun font-prepare-glyphs (glyph-ids font string start end
                            x y align-x align-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string))
  (ecase transform-glyphs
    (:left-to-right
     (font-prepare-glyphs/ltr
      glyph-ids font string start end x y align-x align-y transform-glyphs))
    (:right-to-left
     (font-prepare-glyphs/rtl
      glyph-ids font string start end x y align-x align-y transform-glyphs))
    (:top-to-bottom
     (font-prepare-glyphs/ttb
      glyph-ids font string start end x y align-x align-y transform-glyphs))
    (:bottom-to-top
     (font-prepare-glyphs/btt
      glyph-ids font string start end x y align-x align-y transform-glyphs))))

(defun font-prepare-glyphs/ltr (glyph-ids font string start end
                                x y align-x align-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (let ((firstp t) xmin ymin xmax ymax advance-x)
      (flet ((process-code (code index)
               (let ((info (font-glyph-info font code transform-glyphs)))
                 (when firstp
                   (setf firstp nil
                         xmin x
                         ymin (- y (font-ascent font))
                         ymax (+ y (font-descent font))
                         advance-x 0))
                 (incf advance-x (glyph-info-advance-dx info))
                 (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) index)
                       (the (unsigned-byte 32) (glyph-info-id info))))))
        (declare (inline process-code))
        (loop with this-char = (char string start)
              with idx0 of-type index = 0
              for idx1 of-type index from (1+ start) below end
              as next-char = (char string idx1)
              as code = (char-glyph-code this-char next-char)
              do (process-code code idx0)
                 (setf this-char next-char)
                 (incf idx0)
              finally
                 (process-code (char-code this-char) idx0)
                 (setf xmax (+ xmin advance-x))))
      (let ((dx (ecase align-x
                  (:left   0)
                  (:center (- (/ (- xmax xmin) 2.0)))
                  (:right  (- (- xmax xmin)))))
            (dy (ecase align-y
                  (:baseline 0)
                  (:center (- y (/ (+ ymax ymin) 2.0)))
                  (:top    (- y ymin))
                  (:bottom (- y ymax)))))
        (incf x dx) (incf xmin dx) (incf xmax dx)
        (incf y dy) (incf ymin dy) (incf ymax dy))
      (values x y xmin ymin xmax ymax))))

(defun font-prepare-glyphs/ttb (glyph-ids font string start end
                                x y align-x align-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (let ((firstp t) xmin ymin xmax ymax advance-y
          (line-height (+ (font-descent font)
                          (font-ascent font))))
      (flet ((process-code (code index)
               (let ((info (font-glyph-info font code transform-glyphs)))
                 (when firstp
                   (setf firstp nil
                         ymin y
                         xmin (- x (/ line-height 2))
                         xmax (+ x (/ line-height 2))
                         advance-y 0))
                 (incf advance-y (glyph-info-advance-dy info))
                 (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) index)
                       (the (unsigned-byte 32) (glyph-info-id info))))))
        (declare (inline process-code))
        (loop with this-char = (char string start)
              with idx0 of-type index = 0
              for idx1 of-type index from (1+ start) below end
              as next-char = (char string idx1)
              as code = (char-glyph-code this-char next-char)
              do (process-code code idx0)
                 (setf this-char next-char)
                 (incf idx0)
              finally
                 (process-code (char-code this-char) idx0)
                 (setf ymax (+ ymin advance-y))))
      (let ((dy (ecase align-x
                  (:left   0)
                  (:center (- (/ (- ymax ymin) 2.0)))
                  (:right  (- (- ymax ymin)))))
            (dx (ecase align-y
                  (:baseline 0)
                  (:center (- x (/ (+ xmax xmin) 2.0)))
                  (:top    (- x xmax))
                  (:bottom (- x xmin)))))
        (incf x dx) (incf xmin dx) (incf xmax dx)
        (incf y dy) (incf ymin dy) (incf ymax dy))
      (values x y xmin ymin xmax ymax))))

(defun font-prepare-glyphs/rtl (glyph-ids font string start end
                                x y align-x align-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (let ((firstp t) xmin ymin xmax ymax advance-x)
      (flet ((process-code (code index)
               (let ((info (font-glyph-info font code transform-glyphs)))
                 (when firstp
                   (setf firstp nil
                         xmax x
                         ymin (- y (font-ascent font))
                         ymax (+ y (font-descent font))
                         advance-x 0))
                 (incf advance-x (glyph-info-advance-dx info))
                 (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) index)
                       (the (unsigned-byte 32) (glyph-info-id info))))))
        (declare (inline process-code))
        (loop with this-char = (char string start)
              with idx0 of-type index = 0
              for idx1 of-type index from (1+ start) below end
              as next-char = (char string idx1)
              as code = (char-glyph-code this-char next-char)
              do (process-code code idx0)
                 (setf this-char next-char)
                 (incf idx0)
              finally
                 (process-code (char-code this-char) idx0)
                 (setf xmin (+ xmax advance-x))))
      (let ((dx (ecase align-x
                  (:left   (- xmax xmin))
                  (:center (+ (/ (- xmax xmin) 2.0)))
                  (:right  0)))
            (dy (ecase align-y
                  (:baseline 0)
                  (:center (- y (/ (+ ymax ymin) 2.0)))
                  (:top    (- y ymin))
                  (:bottom (- y ymax)))))
        (incf x dx) (incf xmin dx) (incf xmax dx)
        (incf y dy) (incf ymin dy) (incf ymax dy))
      (values x y xmin ymin xmax ymax))))

(defun font-prepare-glyphs/btt (glyph-ids font string start end
                                x y align-x align-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (let ((firstp t) xmin ymin xmax ymax advance-y
          (line-height (+ (font-descent font)
                          (font-ascent font))))
      (flet ((process-code (code index)
               (let ((info (font-glyph-info font code transform-glyphs)))
                 (when firstp
                   (setf firstp nil
                         ymax y
                         xmin (- x (/ line-height 2))
                         xmax (+ x (/ line-height 2))
                         advance-y 0))
                 (incf advance-y (glyph-info-advance-dy info))
                 (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) index)
                       (the (unsigned-byte 32) (glyph-info-id info))))))
        (declare (inline process-code))
        (loop with this-char = (char string start)
              with idx0 of-type index = 0
              for idx1 of-type index from (1+ start) below end
              as next-char = (char string idx1)
              as code = (char-glyph-code this-char next-char)
              do (process-code code idx0)
                 (setf this-char next-char)
                 (incf idx0)
              finally
                 (process-code (char-code this-char) idx0)
                 (setf ymin (+ ymax advance-y))))
      (let ((dy (ecase align-x
                  (:left   (- ymax ymin))
                  (:center (/ (- ymax ymin) 2.0))
                  (:right  0)))
            (dx (ecase align-y
                  (:baseline 0)
                  (:center (- x (/ (+ xmax xmin) 2.0)))
                  (:top    (- x xmax))
                  (:bottom (- x xmin)))))
        (incf x dx) (incf xmin dx) (incf xmax dx)
        (incf y dy) (incf ymin dy) (incf ymax dy))
      (values x y xmin ymin xmax ymax))))
