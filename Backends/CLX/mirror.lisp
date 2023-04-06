(in-package #:clim-clx)

(defgeneric clx-drawable (object)
  (:method ((object sheet))
    (clx-drawable (sheet-mirror object)))
  (:method ((object medium))
    (clx-drawable (medium-drawable object)))
  (:method ((object xlib::picture))
    (xlib:picture-drawable object))
  (:method ((object xlib:drawable))
    object)
  (:method ((object null))
    nil))

(defclass clx-mirror ()
  ((mirror
    :initarg :mirror
    :accessor mirror
    :reader clx-drawable)
   (help-buffers
    :initform (make-hash-table)
    :reader help-buffers))
  (:default-initargs :mirror (alexandria:required-argument :mirror)))

(defgeneric mirror-width (mirror)
  (:method ((mirror clx-mirror))
    (xlib:drawable-width (mirror mirror))))

(defgeneric mirror-height (mirror)
  (:method ((mirror clx-mirror))
    (xlib:drawable-height (mirror mirror))))

(defgeneric mirror-depth (mirror)
  (:method ((mirror clx-mirror))
    (clx-drawable-depth (mirror mirror))))

(defmacro ensure-help-buffer (window key &body body)
  `(ensure-gethash ,key (help-buffers ,window) ,@body))

(defun release-help-buffer (window key)
  (let ((ht (help-buffers window)))
    (when-let ((pixmap (gethash key ht)))
      (remhash key ht)
      (deallocate-pixmap pixmap))))

(defun release-mirror-resources (mirror)
  (free-clx-drawable-resources (clx-drawable mirror))
  (let ((help-buffers (help-buffers mirror)))
    (climi::dohash ((key val) help-buffers)
      (declare (ignore key))
      (deallocate-pixmap val))
    (clrhash help-buffers)))

(defmacro ensure-clx-drawable-object ((drawable name) &body body)
  `(when-let ((,drawable (clx-drawable ,drawable)))
     (or (getf (xlib:drawable-plist ,drawable) ,name)
         (setf (getf (xlib:drawable-plist ,drawable) ,name)
               (progn ,@body)))))

;;; The purpose of this is to reduce local network traffic for the case of many
;;; calls to compute-rgb-image, for example when drawing a pattern.
;;; For more details, see also: https://github.com/sharplispers/clx/pull/146
(defun clx-drawable-depth (drawable)
  (ensure-clx-drawable-object (drawable 'clx-depth)
    (xlib:drawable-depth drawable)))

(defun clx-drawable-format (drawable)
  (ensure-clx-drawable-object (drawable 'clx-format)
    (xlib:find-window-picture-format (xlib:drawable-root drawable))))

(defun clx-drawable-display (drawable)
  (ensure-clx-drawable-object (drawable 'clx-display)
    (xlib:drawable-display drawable)))

;;; This function maintains the "primary" picture, there may be more.
(defun clx-drawable-picture (drawable)
  (ensure-clx-drawable-object (drawable 'clx-picture)
    (xlib:render-create-picture drawable :format (clx-drawable-format drawable))))

(defun free-clx-drawable-resources (drawable)
  (loop for (key val) on (xlib:drawable-plist drawable) by #'cddr
        do (typecase val
             (xlib::picture (xlib:render-free-picture val))
             (xlib:gcontext (xlib:free-gcontext val))
             (xlib:pixmap   (%deallocate-pixmap val)))))
