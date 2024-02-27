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
         (x1 (glyph-info-left info))
         (y1 (glyph-info-top info))
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
                             :x-origin (- x1) :y-origin y1
                             :x-advance dx :y-advance dy)
      (setf (glyph-info-id info) glyph-id))
    info))

;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-dx values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.

(defvar *draw-font-lock* (clim-sys:make-lock "draw-font"))

(declaim (inline font-prepare-glyphs))
(defun font-prepare-glyphs (glyph-ids font string start end
                            x y align-x align-y transform-glyphs)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string)
           (ignore transform-glyphs))
  (clim-sys:with-lock-held (*draw-font-lock*)
    (loop with origin-x = nil
          with origin-y = (font-ascent font)
          with advance-x = 0
          with advance-y = (font-descent font)
          with this-char = (char string start)
          with idx0 of-type index = 0
          for idx1 of-type index from (1+ start) below end
          as next-char = (char string idx1)
          as code = (char-glyph-code this-char next-char)
          as glyph = (font-glyph-info font code :left-to-right)
          do
             (when (null origin-x)
               (setf origin-x (- (glyph-info-left glyph))))
             (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) idx0)
                   (the (unsigned-byte 32) (glyph-info-id glyph)))
             (setf this-char next-char)
             (incf idx0)
             (incf advance-x (glyph-info-advance-dx glyph))
          finally
             (setf glyph (font-glyph-info font (char-code this-char) :left-to-right))
             (when (null origin-x)
               (setf origin-x (- (glyph-info-left glyph))))
             (setf (aref (the (simple-array (unsigned-byte 32)) glyph-ids) idx0)
                   (the (unsigned-byte 32) (glyph-info-id glyph)))
             (incf advance-x (glyph-info-advance-dx glyph))
             (return (values origin-x origin-y advance-x advance-y
                             (ecase align-x
                               (:left x)
                               (:center (decf x (+ (/ advance-x 2.0))))
                               (:right  (decf x advance-x)))
                             (ecase align-y
                               (:baseline y)
                               (:top    (incf y origin-y))
                               (:center (incf y (/ (- origin-y advance-y) 2.0)))
                               (:bottom (decf y advance-y))))))))
