(in-package #:clim-clx-fb)

(defclass clx-fb-port (render-port-mixin
                       clim-xcommon:keysym-port-mixin
                       clim-clx::clx-basic-port)
  ((all-mirrors :initform '() :accessor all-mirrors)))

(defmethod find-port-type ((type (eql :clx-fb)))
  (values 'clx-fb-port (nth-value 1 (find-port-type :clx))))

(defmethod initialize-instance :after ((port clx-fb-port) &rest args)
  (declare (ignore args))
  (push (make-instance 'clx-fb-frame-manager :port port)
        (slot-value port 'frame-managers))
  (setf (slot-value port 'pointer)
        (make-instance 'clim-clx::clx-basic-pointer :port port))
  (initialize-clx port)
  (clim-extensions:port-all-font-families port))

(defmethod realize-mirror ((port clx-fb-port) (sheet mirrored-sheet-mixin))
  (let* ((window (clim-clx::%realize-mirror port sheet))
         (mirror (make-instance 'clx-fb-mirror :mirror window)))
    (setf (getf (xlib:window-plist window) 'sheet) sheet)
    (setf (slot-value mirror 'gcontext)
          (xlib:create-gcontext :drawable window
                                :background (values 0 0 0)
                                :foreground (values 255 255 255)))
    (push mirror (all-mirrors port))
    mirror))

(defmethod destroy-mirror ((port clx-fb-port) (sheet mirrored-sheet-mixin))
  (let* ((mirror (sheet-direct-mirror sheet))
         (window (window mirror)))
    ;; There is no xlib:free-image because it is a lisp structure and it is
    ;; simply garbage collected.
    (xlib:free-gcontext (mirror-gcontext mirror))
    (setf (mirror-clx-image mirror) nil
          (mirror-gcontext mirror) nil)
    (remf (xlib:window-plist window) 'sheet)
    (alexandria:deletef (all-mirrors port) mirror)
    (xlib:destroy-window window)
    (xlib:display-force-output (clx-port-display port))))

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet top-level-sheet-mixin))
  (let ((window (clim-clx::realize-mirror-aux port sheet
                                              :width (bounding-rectangle-width sheet)
                                              :height (bounding-rectangle-height sheet)))
        (pname (clime:sheet-pretty-name sheet))
        (uname (clime:sheet-name sheet)))
    (setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
    (setf (xlib:wm-name window) pname)
    (setf (xlib:wm-icon-name window) pname)
    (xlib:set-wm-class
     window
     (string-downcase uname)
     (string-capitalize (string-downcase uname)))
    (setf (xlib:wm-protocols window) `(:wm_delete_window))
    (xlib:change-property window
                          :WM_CLIENT_LEADER (list (xlib:window-id window))
                          :WINDOW 32)
    window))

(defmethod clim-clx::%realize-mirror ((port clx-fb-port) (sheet unmanaged-sheet-mixin))
  (clim-clx::realize-mirror-aux port sheet :override-redirect :on))

(defmethod make-medium ((port clx-fb-port) sheet)
  (make-instance 'clx-fb-medium :sheet sheet :port port))

(defmethod port-force-output ((port clx-fb-port))
  (map nil #'%mirror-force-output (all-mirrors port))
  (xlib:display-force-output (clx-port-display port)))
