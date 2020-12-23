(in-package :clim-gtk)

(defclass gtk-pointer (standard-pointer)
  ((cursor :accessor pointer-cursor :initform :upper-left)
   (x :initform 0)
   (y :initform 0)))

(defclass gtk-port (basic-port)
  ((id)
   (pointer         :accessor port-pointer
                    :initform (make-instance 'gtk-pointer))
   (window          :initform nil
                    :accessor gtk-port-window)
   (sheet-to-mirror :initform (make-hash-table :test 'eql)
                    :reader gtk-port/sheet-to-mirror)))

(defclass gtk-mirror ()
  ((window        :initarg :window
                  :accessor gtk-mirror/window)
   (image         :initarg :image
                  :initform (alexandria:required-argument :image)
                  :accessor gtk-mirror/image)
   (sheet         :initarg :sheet
                  :initform (alexandria:required-argument :sheet)
                  :reader gtk-mirror/sheet)
   (drawing-area  :initarg :drawing-area
                  :accessor gtk-mirror/drawing-area)
   (pango-context :initarg :pango-context
                  :accessor gtk-mirror/pango-context)))

(defmethod find-port-type ((type (eql :null)))
  (values 'gtk-port 'identity))

(defun parse-gtk-server-path (path)
  path)

(setf (get :gtk :port-type) 'gtk-port)
(setf (get :gtk :server-path-parser) 'parse-gtk-server-path)

(defun gtk-main-no-traps ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (gtk:gtk-main)))

(defmethod initialize-instance :after ((port gtk-port) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value port 'id) (gensym "GTK-PORT-"))
  ;; FIXME: it seems bizarre for this to be necessary
  (push (make-instance 'gtk-frame-manager :port port)
	(slot-value port 'climi::frame-managers))
  (bordeaux-threads:make-thread #'gtk-main-no-traps :name "GTK Event Thread"))

(defmethod print-object ((object gtk-port) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S ~S" :id (slot-value object 'id))))

(defclass gtk-renderer-sheet (mirrored-sheet-mixin)
  ())

(defclass gtk-top-level-sheet-pane (gtk-renderer-sheet climi::top-level-sheet-pane)
  ())

(defmethod port-set-mirror-region ((port gtk-port) sheet region)
  ())

(defmethod port-set-mirror-transformation ((port gtk-port) sheet transformation)
  ())

(defmethod climi::port-lookup-mirror ((port gtk-port) (sheet gtk-renderer-sheet))
  (gethash sheet (gtk-port/sheet-to-mirror port)))

(defmethod climi::port-register-mirror ((port gtk-port) (sheet gtk-renderer-sheet) mirror)
  (setf (gethash sheet (gtk-port/sheet-to-mirror port)) mirror))

(defmethod climi::port-unregister-mirror ((port gtk-port) (sheet gtk-renderer-sheet) mirror)
  (remhash sheet (gtk-port/sheet-to-mirror port)))

(defun draw-window-content (cr mirror)
  (alexandria:when-let ((image (gtk-mirror/image mirror)))
    (cairo:cairo-set-source-surface cr image 0 0)
    (cairo:cairo-rectangle cr 0 0
                           (cairo:cairo-image-surface-get-width image)
                           (cairo:cairo-image-surface-get-height image))
    (cairo:cairo-fill cr)))

(defun make-backing-image (width height)
  (let* ((image (cairo:cairo-image-surface-create :argb32 width height))
         (cr (cairo:cairo-create image)))
    (cairo:cairo-set-source-rgb cr 1 1 1)
    (cairo:cairo-paint cr)
    image))

(defmethod realize-mirror ((port gtk-port) (sheet mirrored-sheet-mixin))
  (log:info "Realising mirror: port=~s sheet=~s" port sheet)
  (let* ((q (compose-space sheet))
         (width (climi::space-requirement-width q))
         (height (climi::space-requirement-height q))
         (image (make-backing-image width height))
         (mirror (make-instance 'gtk-mirror :sheet sheet :image image)))
    (multiple-value-bind (window drawing-area pango-context)
        (in-gtk-thread ()
          (let ((window (make-instance 'gtk:gtk-window
                                       :type :toplevel
                                       :default-width width
                                       :default-height height)))
            (gtk:gtk-widget-add-events window '(:all-events-mask))
            (let ((drawing-area (make-instance 'gtk:gtk-drawing-area)))
              (gobject:g-signal-connect drawing-area "draw"
                                        (lambda (widget cr)
                                          (declare (ignore widget))
                                          (draw-window-content (gobject:pointer cr) mirror)
                                          t))
              (gtk:gtk-container-add window drawing-area)
              (let ((pango-context (gtk:gtk-widget-create-pango-context drawing-area)))
                (gtk:gtk-widget-show-all window)
                (values window drawing-area pango-context)))))
      (setf (gtk-mirror/window mirror) window)
      (setf (gtk-mirror/drawing-area mirror) drawing-area)
      (setf (gtk-mirror/pango-context mirror) pango-context)
      (climi::port-register-mirror port sheet mirror))))

(defmethod destroy-mirror ((port gtk-port) (sheet mirrored-sheet-mixin))
  (let ((mirror (climi::port-lookup-mirror port sheet)))
    (in-gtk-thread ()
      (gtk:gtk-widget-destroy (gtk-mirror/window mirror)))
    (cairo:cairo-destroy (gtk-mirror/image mirror))))

(defmethod mirror-transformation ((port gtk-port) mirror)
  nil)

(defmethod port-enable-sheet ((port gtk-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod port-disable-sheet ((port gtk-port) (mirror mirrored-sheet-mixin))
  nil)

(defmethod destroy-port :before ((port gtk-port))
  (in-gtk-thread ()
    (gtk:gtk-main-quit)))

(defmethod process-next-event ((port gtk-port) &key wait-function (timeout nil))
  (cond ((maybe-funcall wait-function)
         (values nil :wait-function))
        ((not (null timeout))
         (sleep timeout)
         (if (maybe-funcall wait-function)
             (values nil :wait-function)
             (values nil :timeout)))
        ((not (null wait-function))
         (loop do (sleep 0.1)
               until (funcall wait-function)
               finally (return (values nil :wait-function))))
        (t
         (error "Game over. Listening for an event on GTK backend."))))

(defmethod make-graft
    ((port gtk-port) &key (orientation :default) (units :device))
  (make-instance 'gtk-graft
                 :port port :mirror (gensym)
                 :orientation orientation :units units))

(defmethod make-medium ((port gtk-port) sheet)
  (make-instance 'gtk-medium :sheet sheet))

(defmethod text-style-mapping
    ((port gtk-port) (text-style text-style) &optional character-set)
  (declare (ignore port text-style character-set))
  nil)

(defmethod (setf text-style-mapping) (font-name
                                      (port gtk-port)
                                      (text-style text-style)
                                      &optional character-set)
  (declare (ignore font-name text-style character-set))
  nil)

(defmethod graft ((port gtk-port))
  (first (climi::port-grafts port)))

(defmethod port-allocate-pixmap ((port gtk-port) sheet width height)
  (declare (ignore sheet width height))
  ;; FIXME: this isn't actually good enough; it leads to errors in
  ;; WITH-OUTPUT-TO-PIXMAP
  nil)

(defmethod port-deallocate-pixmap ((port gtk-port) pixmap)
  #+nil
  (when (pixmap-mirror port pixmap)
    (destroy-mirror port pixmap)))

(defmethod pointer-position ((pointer gtk-pointer))
  (values (slot-value pointer 'x) (slot-value pointer 'y)))

(defmethod pointer-button-state ((pointer gtk-pointer))
  nil)

(defmethod port-modifier-state ((port gtk-port))
  nil)

(defmethod synthesize-pointer-motion-event ((pointer gtk-pointer))
  nil)

(defmethod (setf port-keyboard-input-focus) (focus (port gtk-port))
  focus)

(defmethod port-keyboard-input-focus ((port gtk-port))
  nil)

(defmethod port-force-output ((port gtk-port))
  nil)

(defmethod distribute-event :around ((port gtk-port) event)
  (declare (ignore event))
  nil)

(defmethod set-sheet-pointer-cursor ((port gtk-port) sheet cursor)
  (declare (ignore sheet cursor))
  nil)