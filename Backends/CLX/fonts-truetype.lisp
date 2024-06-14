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

;;; Restriction: no more than 65536 glyph pairs cached on a single display. I
;;; don't think that's unreasonable. Having keys as glyph pairs is essential for
;;; kerning where the same glyph may have different advance-dx values for
;;; different next elements. (byte 16 0) is the character code and (byte 16 16)
;;; is the next character code. For standalone glyphs (byte 16 16) is zero.

(defmethod font-generate-glyph :around
    ((port clx-ttf-port) font code direction)
  (declare (ignore font code direction))
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
    (let ((glyph-set (ensure-glyph-set port))
          (glyph-id (draw-glyph-id port)))
      (xlib:render-add-glyph glyph-set glyph-id
                             :data pixarray
                             :x-origin x0 :y-origin y0
                             :x-advance dx :y-advance dy)
      (setf (glyph-info-id info) glyph-id))
    info))
