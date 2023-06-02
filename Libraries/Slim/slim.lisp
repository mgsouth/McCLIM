(defpackage #:slim
  (:use #:clim-lisp)
  (:export #:+golden-ratio+
           #:parent #:children :add-child :delete-child :repaint
           #:defapp #:display #:present* #:defpresent
           #:with-table #:row #:col #:cell #:*pane*))
(in-package #:slim)

(defparameter +golden-ratio+ #. (/ (+ 1 (sqrt 5)) 2)
  "Golden Ratio constant.")

(defun car* (object)
  (if (consp object)
      (car object)
      object))


#| Major issue: There is a proposal on the table to unify the sheet and output
record protocols, not by unifying the class structure, but by making them ; ;
implement the same generic functions where that makes sense. For instance, ; ;
sheets and output records both have regions, transformations (that relate sheets ; ;
to their parents), both support a repainting operation, and so forth. ; ;
                                        ; ;
In particular, sheet-parent and output-record-parent are equivalent, as are ; ;
sheet-children and output-record-children, sheet-adopt-child and ; ;
add-output-record, sheet-disown-child and delete-output-record, and ; ;
repaint-sheet and replay-output-record, and the mapping ; ;
functions. output-record-position and its setf function have sheet analogs. The ; ;
sheet and output record notification functions are also equivalent. ; ;
                                        ; ;
This simplifies the conceptual framework of CLIM, and could eventually simplify ; ;
the implementation as well. Doing this work now opens the door for later ; ;
unifications, such unifying the pane layout functionality with table ; ;
formatting. --- York, SWM |#

(defgeneric parent (object)
  (:method ((sheet clim:sheet))
    (clim:sheet-parent sheet))
  (:method ((output-record clim:output-record))
    (clim:output-record-parent output-record)))

(defgeneric children (object)
  (:method ((sheet clim:sheet))
    (clim:sheet-children sheet))
  (:method ((output-record clim:output-record))
    (clim:output-record-children output-record)))

(defgeneric add-child (object child)
  (:method ((sheet clim:sheet) child)
    (clim:sheet-adopt-child sheet child))
  (:method ((output-record clim:output-record) child)
    (clim:add-output-record child output-record)))

(defgeneric delete-child (object child &optional errorp)
  (:method ((sheet clim:sheet) child &optional errorp)
    (clim:sheet-disown-child sheet child :errorp errorp))
  (:method ((output-record clim:output-record) child  &optional errorp)
    (clim:delete-output-record child output-record errorp)))

(defgeneric repaint (object target region)
  (:method ((sheet clim:sheet) medium region)
    (declare (ignore medium))
    (clim:repaint-sheet sheet region))
  (:method ((output-record clim:output-record) sheet region)
    (clim:replay-output-record output-record sheet region)))


;;; Presentation utilities.
(defvar *current-view* nil)

;;; This function is compatible with "standard" print functions like 'princ
;;; thanks to the acceptance of the stream as a second argument.
(defun present* (object &optional (stream *standard-output*) &rest args
                 &key (ptype (clim:presentation-type-of object))
                   ((:view *current-view*)
                    (or *current-view* (clim:stream-default-view stream)))
                 &allow-other-keys)
  (climi::with-stream-designator (stream *standard-output*)
    (climi::with-keywords-removed (args (:ptype))
      (apply #'clim:present object ptype :stream stream
             (append args (list :single-box t :view *current-view*))))))

(defmacro defpresent ((object type &optional (stream (gensym)) (view (gensym))) &body body)
  (when (keywordp view)
    (setf view `(,(gensym) (eql ,view))))
  `(clim:define-presentation-method clim:present
       (,object (type ,type) ,stream ,view &rest args)
     (declare (ignorable type ,(car* stream) ,(car* view) args))
     ,@body))


;;; It would be nice if we had a canonical function DISPLAY that may be
;;; specialized on both the frame and the stream and passed uniformly as
;;; #'DISPLAY. That's what we do here.
(defgeneric display (frame stream)
  (:method (frame stream)
    (format stream "~a" frame)))

;;; application frames
(defmacro defapp (name-and-opts args &body body)
  "Macro allows defining a single-pane application frame as if it were
a function.

NAME-AND-OPTS is a list where first element is a symbol being the
frame and the function name. The rest of a list are arguments passed
to the application pane as the initialization arguments. NAME-AND-OPTS
may be a symbol, then it is treated as a list with a single element.

Args are function arguments. They are available in the body. Bindings
are closed over the display function.

Examples:

  (defapp example-1 (x y z)
    (clim:draw-rectangle* *standard-output* x y (+ x 100) (+ y 200))
    (format t \"~&Z is ~s\" z))

  (defapp (example-2 :text-margins `(:left 20 :top 10 :right 20 :bottom 10))
      (x y z)
    (clim:draw-rectangle* *standard-output* x y (+ x 100) (+ y 200))
    (format t \"~&Z is ~s\" z))

  (example-1 100 100 \"jackdaniel\")
  (example-2  50  50 \"another\")
"
  (destructuring-bind (name &rest pane-options)
      (alexandria:ensure-list name-and-opts)
    `(progn
       (clim:define-application-frame ,name ()
         ((app-args :initarg :app-args :reader app-args))
         (:menu-bar nil)
         (:reinitialize-frames t)
         (:pane :application
          :display-function #'display
          ,@pane-options))
       (defmethod display ((clim:*application-frame* ,name) *standard-output*)
         (destructuring-bind ,args (app-args clim:*application-frame*)
           ,@body))
       (defun ,name (&rest args)
         (clim:find-application-frame ',name :app-args args :create :force)))))


;;; context
(defvar *pane*)

(defmacro with-pane ((pane) &body body)
  `(let* ((*pane* ,pane)
          (*standard-output* *pane*))
     ,@body))


;;; with-table
(defmacro with-table ((&optional (pane *pane*) &rest options)
                      &body body)
  `(with-pane (,pane)
     (clim:formatting-table (*pane* ,@options)
       ,@body)))

(defmacro row  (&body body) `(clim:formatting-row    (*pane*) ,@body))
(defmacro col  (&body body) `(clim:formatting-column (*pane*) ,@body))
(defmacro cell (&body body) `(clim:formatting-cell   (*pane*) ,@body))
