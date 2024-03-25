(in-package #:clim-clx)

;;; Pixmap

(defun %allocate-pixmap (drawable width height depth)
  (xlib:create-pixmap :width width
                      :height height
                      :depth depth
                      :drawable drawable))

(defun %deallocate-pixmap (drawable)
  (xlib:free-pixmap drawable))

(defun %fill-pixmap (pixmap value width height)
  (let ((gcontext (xlib:create-gcontext :drawable pixmap))
        (w (xlib:drawable-width pixmap))
        (h (xlib:drawable-height pixmap)))
    (setf (xlib:gcontext-function gcontext) boole-1)
    (setf (xlib:gcontext-foreground gcontext) value)
    (xlib:draw-rectangle pixmap gcontext 0 0 width height t)
    (xlib:free-gcontext gcontext)))

(defclass clx-pixmap (clx-mirror)
  ((width
    :initarg :width
    :reader mirror-width
    :accessor pixmap-width)
   (height
    :initarg :height
    :reader mirror-height
    :accessor pixmap-height)
   (depth
    :initarg :depth
    :reader pixmap-depth
    :reader mirror-depth)))

(defmethod allocate-pixmap ((medium clx-medium) width height)
  (when-let ((mirror (clx-drawable medium)))
    (let* ((width (ceiling width))
           (height (ceiling height))
           (depth (clx-drawable-depth mirror))
           (pixmap (%allocate-pixmap mirror width height depth)))
      (%fill-pixmap pixmap #xff0000ff width height)
      (make-instance 'clx-pixmap :mirror pixmap
                                 :width width
                                 :height height
                                 :depth depth))))

(defmethod deallocate-pixmap ((mirror clx-pixmap))
  (release-mirror-resources mirror)
  (let ((pixmap (clx-drawable mirror)))
    (%deallocate-pixmap pixmap)))

(defun create-pixmap (mirror width height depth)
  (let* ((target (clx-drawable mirror))
         (pixmap (%allocate-pixmap target width height depth)))
    (make-instance 'clx-pixmap
                   :mirror pixmap
                   :width width
                   :height height
                   :depth depth)))

(defun resize-pixmap (pixmap width height)
  (let ((old-width  (pixmap-width pixmap))
        (old-height (pixmap-height pixmap)))
    (when (or (< old-width width)
              (< old-height height))
      (let* ((depth (pixmap-depth pixmap))
             (old-pixmap (clx-drawable pixmap))
             (new-width (max width old-width))
             (new-height (max height old-height))
             (new-pixmap (%allocate-pixmap old-pixmap new-width new-height depth)))
        (deallocate-pixmap pixmap)
        (setf (mirror pixmap) new-pixmap
              (pixmap-width pixmap) new-width
              (pixmap-height pixmap) new-height))))
  pixmap)

(defun ensure-pixmap (mirror pixmap width height)
  (if (null pixmap)
      (create-pixmap mirror width height (mirror-depth mirror))
      (resize-pixmap pixmap width height)))


;;; These methods will work on mirrors (both pixmaps and windows).

;;; WIDTH and HEIGHT arguments should be integers, but we'll leave the calls
;;; to round "in" for now.

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position
      ((medium-native-transformation from-drawable) from-x from-y)
    (with-transformed-position
        ((medium-native-transformation to-drawable) to-x to-y)
      (multiple-value-bind (width height)
          (transform-distance (medium-transformation from-drawable) width height)
        (xlib:copy-area (clx-drawable from-drawable)
                        (medium-gcontext to-drawable +background-ink+)
                        (round-coordinate from-x) (round-coordinate from-y)
                        (round width) (round height)
                        (clx-drawable to-drawable)
                        (round-coordinate to-x) (round-coordinate to-y))))))

(defmethod medium-copy-area ((from-drawable clx-medium) from-x from-y width height
                             (to-drawable clx-mirror) to-x to-y)
  (with-transformed-position
      ((medium-native-transformation from-drawable) from-x from-y)
    (let* ((to-drawable (clx-drawable to-drawable))
           (gcontext (ensure-clx-drawable-object (to-drawable :copy-area)
                       (xlib:create-gcontext :drawable to-drawable))))
      (xlib:copy-area (clx-drawable from-drawable)
                      gcontext
                      (round-coordinate from-x)
                      (round-coordinate from-y)
                      (round width)
                      (round height)
                      to-drawable
                      (round-coordinate to-x)
                      (round-coordinate to-y)))))

(defmethod medium-copy-area ((from-drawable clx-mirror) from-x from-y width height
                             (to-drawable clx-medium) to-x to-y)
  (with-transformed-position ((medium-native-transformation to-drawable) to-x to-y)
    (xlib:copy-area (clx-drawable from-drawable)
                    (medium-gcontext to-drawable +background-ink+)
                    (round-coordinate from-x) (round-coordinate from-y)
                    (round width) (round height)
                    (clx-drawable to-drawable)
                    (round-coordinate to-x) (round-coordinate to-y))))

(defun %drawable-copy-area (from fx fy w h to tx ty)
  (let ((gcontext (ensure-clx-drawable-object (to :copy-area)
                    (xlib:create-gcontext :drawable to))))
    (xlib:copy-area from gcontext fx fy w h to tx ty)))

(defmethod medium-copy-area ((from-drawable clx-mirror) from-x from-y width height
                             (to-drawable clx-mirror) to-x to-y)
  (%drawable-copy-area (clx-drawable from-drawable)
                       (round-coordinate from-x) (round-coordinate from-y)
                       (round width) (round height)
                       (clx-drawable to-drawable)
                       (round-coordinate to-x) (round-coordinate to-y)))
