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
    :reader clx-drawable))
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

(defun free-clx-drawable-resources (drawable)
  (loop for (key val) on (xlib:drawable-plist drawable) by #'cddr
        do (typecase val
             (xlib::picture (xlib:render-free-picture val))
             (xlib:gcontext (xlib:free-gcontext val))
             (xlib:pixmap   (%deallocate-pixmap val)))))
