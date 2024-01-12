;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 1998-2001 by Michael McDonald <mikemac@mikemac.com>
;;;  (c) Copyright 2000,2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;

(in-package #:clim-internals)

(defparameter *tab-string* "        ")

;;; Standard-Output-Stream class
(defclass standard-output-stream (output-stream) ())

(defmethod stream-recording-p ((stream output-stream)) nil)
(defmethod stream-drawing-p ((stream output-stream)) t)

;;; Standard-Extended-Output-Stream class

(defclass standard-extended-output-stream (extended-output-stream
                                           standard-output-stream
                                           standard-page-layout
                                           filling-output-mixin)
  ((foreground :initarg :foreground :reader foreground)
   (background :initarg :background :reader background)
   (text-style :initarg :text-style :reader stream-text-style)
   (view :initarg :default-view :accessor stream-default-view)
   (vspace :initarg :vertical-spacing :accessor stream-vertical-spacing)
   (hspace :initarg :horizontal-spacing :accessor stream-horizontal-spacing)
   (end-of-line-action :accessor stream-end-of-line-action)
   (end-of-page-action :accessor stream-end-of-page-action))
  (:default-initargs
   :foreground +black+ :background +white+ :text-style *default-text-style*
   :vertical-spacing 2 :horizontal-spacing 2
   :default-view +textual-view+))

(defmethod stream-cursor-position ((stream standard-extended-output-stream))
  (cursor-position (stream-text-cursor stream)))

(defmethod* (setf stream-cursor-position) (x y (stream standard-extended-output-stream))
  (setf (cursor-position (stream-text-cursor stream)) (values x y)))

(defmethod stream-baseline ((sheet standard-extended-output-stream))
  (let ((cursor (stream-text-cursor sheet)))
    (ecase (stream-line-direction sheet)
      ((:left-to-right :right-to-left) (cursor-offset-y cursor))
      ((:top-to-bottom :bottom-to-top) (cursor-offset-x cursor)))))

(defmethod (setf stream-baseline) (baseline (sheet standard-extended-output-stream))
  (let ((cursor (stream-text-cursor sheet)))
   (ecase (stream-line-direction sheet)
     ((:left-to-right :right-to-left) (setf (cursor-offset-y cursor) baseline))
     ((:top-to-bottom :bottom-to-top) (setf (cursor-offset-x cursor) baseline)))))

(defmethod stream-cursor-size ((stream standard-extended-output-stream))
  (let ((cursor (stream-text-cursor stream)))
    (values (cursor-width cursor) (cursor-height cursor))))

(defmethod* (setf stream-cursor-size) ((stream standard-extended-output-stream))
  (let ((cursor (stream-text-cursor stream)))
    (values (cursor-width cursor) (cursor-height cursor))))

(defmethod stream-cursor-height ((sheet standard-extended-output-stream))
  (cursor-height (stream-text-cursor sheet)))

(defun (setf stream-cursor-height) (value stream)
  (setf (cursor-height (stream-text-cursor stream)) value))

(defmethod stream-cursor-width ((sheet standard-extended-output-stream))
  (cursor-width (stream-text-cursor sheet)))

(defun (setf stream-cursor-width) (value stream)
  (setf (cursor-width (stream-text-cursor stream)) value))

(defmethod stream-set-cursor-position ((stream standard-extended-output-stream) x y)
  (setf (stream-cursor-position stream) (values x y)))

(defmethod stream-increment-cursor-position
    ((stream standard-extended-output-stream) dx dy)
  (let ((cursor (stream-text-cursor stream))
        (dx (or dx 0))
        (dy (or dy 0)))
   (multiple-value-bind (x y) (cursor-position cursor)
     (setf (cursor-position cursor)
           (values (+ x dx) (+ y dy))))))

(defun reset-stream-cursor (stream cursor)
  (let* ((text-style (stream-text-style stream))
         (width (text-style-width text-style stream))
         (height (text-style-height text-style stream)))
    (setf (cursor-position cursor) (stream-cursor-initial-position stream)
          (cursor-offset cursor) (values 0 0)
          (cursor-size cursor) (values width height))))

(defun text-style-offset (text-style stream)
  (ecase (stream-page-direction stream)
    (:top-to-bottom (values 0 (text-style-ascent text-style stream)))
    (:bottom-to-top (values 0 (- (text-style-ascent text-style stream))))
    (:left-to-right (values (text-style-width text-style stream) 0))
    (:right-to-left (values (- (text-style-width text-style stream)) 0))))

(defmethod stream-force-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-force-output medium)))

(defmethod stream-finish-output :after
    ((stream standard-extended-output-stream))
  (with-sheet-medium (medium stream)
    (medium-finish-output medium)))

(defmethod note-sheet-grafted :after ((stream standard-extended-output-stream))
  (reset-stream-cursor stream (stream-text-cursor stream)))

(defmacro with-end-of-line-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-line-action ,stream) ,action))
     ,@body))

(defmacro with-end-of-page-action ((stream action) &body body)
  (when (eq stream t)
    (setq stream '*standard-output*))
  (check-type stream symbol)
  `(letf (((stream-end-of-page-action ,stream) ,action))
     ,@body))

(defgeneric stream-write-output (stream line x y &rest args)
  (:documentation
   "Writes the object on the current line of the STREAM. The caller is responsible
for estabilishing an appropriate drawing system with origin at (0,0) and updating
the cursor after the operation. This function does not wrap.")
  (:method ((stream standard-extended-output-stream) (line string) x y &rest args)
    (apply #'draw-text* stream line x y args))
  (:method ((stream standard-extended-output-stream)
            (item bounding-rectangle) x y &rest args)
    (with-translation (stream x y)
      (apply #'draw-design stream item args))))

(defun seos-finish-output (stream)
  (when (stream-drawing-p stream)
    (change-stream-space-requirements stream)
    (let ((lscroll (eq (stream-end-of-line-action stream) :scroll))
          (pscroll (eq (stream-end-of-page-action stream) :scroll))
          (cursor (stream-text-cursor stream)))
      (cond
        ((and lscroll pscroll) (scroll-extent* stream cursor))
        (lscroll               (scroll-extent/line stream cursor))
        (pscroll               (scroll-extent/page stream cursor))))))

(defun seos-write-newline (stream soft-newline-p)
  (nest
   (let ((cursor (stream-text-cursor stream))
         (hspace (stream-horizontal-spacing stream))
         (vspace (stream-vertical-spacing stream))))
   (multiple-value-bind (x0 y0) (stream-cursor-initial-position stream))
   ;; (multiple-value-bind (xn yn) (stream-cursor-final-position stream))
   (multiple-value-bind (cx cy) (cursor-position cursor))
   (multiple-value-bind (hsize vsize) (cursor-size cursor))
   (multiple-value-bind (updated-cx updated-cy)
       (ecase (stream-page-direction stream)
         (:top-to-bottom (values x0 (+ cy vsize vspace)))
         (:bottom-to-top (values x0 (- cy vsize vspace)))
         (:left-to-right (values (+ cx hsize hspace) y0))
         (:right-to-left (values (- cx hsize hspace) y0)))
     (unless nil                      ; soft-newline-p
       ;; This will close the output record if the stream is recorded.
       (reset-stream-cursor stream cursor)
       (force-output stream))
     (setf (cursor-position cursor) (values updated-cx updated-cy))))
  (when-let ((after-line-break-fn (after-line-break stream)))
    (funcall after-line-break-fn stream soft-newline-p)))

(defun seos-write-object (stream object)
  (nest
   (with-identity-transformation (stream))
   (with-sheet-medium (medium stream))
   (let* ((cursor (stream-text-cursor stream))
          (end-of-page-action (stream-end-of-page-action stream))
          (end-of-line-action (stream-end-of-line-action stream))
          (wrapl (member end-of-line-action '(:wrap :wrap*)))))
   (tagbody
      (go :start-line)
    :break-page
      (setf (stream-cursor-position stream)
            (stream-cursor-initial-position stream))
      (go :start-line)
    :break-line
      (seos-write-newline stream t)
    :start-line
      (multiple-value-bind (dx dy fx fy bx by cw ch eol-p eop-p)
          (stream-cursor-motion stream cursor object)
        (setf (cursor-offset cursor) (values bx by))
        (setf (cursor-size cursor) (values cw ch))
        (when (and eop-p (member end-of-page-action '(:wrap :wrap*)))
          (go :break-page))
        (when (and eol-p wrapl (plusp (stream-text-offset stream cursor)))
          (go :break-line))
        (stream-write-output stream object dx dy)
        (setf (cursor-position cursor) (values fx fy))))))

;;; This function is responsible for managing the cursor and invoking drawing.
;;; Text wrapping and sheet dimensions are updated as we go, while scrolling
;;; (when applicable) is performed in the end (if needed).
(defun seos-write-vector (stream vector start end)
  (when (>= start end)
    (return-from seos-write-vector))
  (nest
   (with-identity-transformation (stream))
   (with-sheet-medium (medium stream))
   (let* ((cursor (stream-text-cursor stream))
          (end-of-page-action (stream-end-of-page-action stream))
          (end-of-line-action (stream-end-of-line-action stream))
          (text-style (medium-text-style medium))
          (split end)
          (wrapl (member end-of-line-action '(:wrap :wrap*)))))
   (tagbody
      (go :start-line)
    :break-page
      (setf (stream-cursor-position stream)
            (stream-cursor-initial-position stream))
      (go :start-line)
    :break-line
      (seos-write-newline stream t)
      (setf start split
            split end)
    :start-line
      (multiple-value-bind (dx dy fx fy bx by cw ch eol-p eop-p)
          (stream-cursor-motion stream cursor vector :start start :end end
                                                     :text-style text-style)
        (setf (cursor-offset cursor) (values bx by))
        (setf (cursor-size cursor) (values cw ch))
        (when (and eop-p (member end-of-page-action '(:wrap :wrap*)))
          (go :break-page))
        (when (and eol-p wrapl)
          (setf split (stream-text-break stream cursor vector start end)))
        (stream-write-output stream vector dx dy :start start :end split)
        (when (/= split end)
          (go :break-line))
        (setf (cursor-position cursor) (values fx fy))))))

(defgeneric stream-write-object (stream object)
  (:method ((stream standard-extended-output-stream) object)
    (seos-write-object stream object)
    (seos-finish-output stream)))

;;; FIXME we can call STREAM-CURSOR-MOTION for the whole vector, but what about
;;; drawing then? (in other words - implement line-aware STREAM-WRITE-OUTPUT).
(defgeneric stream-write-vector (stream vector start end)
  (:method ((stream standard-extended-output-stream) vector start end)
    #+ (or) (seos-write-vector stream vector start end)
    #- (or) (loop for index from start below end
                  for object = (aref vector index)
                  do (seos-write-object stream object))
    (seos-finish-output stream)))

(defmethod stream-write-char ((stream standard-extended-output-stream) char)
  (case char
    (#\newline (seos-write-newline stream nil))
    (#\tab     (seos-write-vector stream *tab-string* 0 (length *tab-string*)))
    (otherwise (seos-write-vector stream (string char) 0 1)))
  (seos-finish-output stream))

(defmethod stream-write-string ((stream standard-extended-output-stream) string
                                &optional (start 0) end)
  (let ((seg-start start)
        (end (or end (length string))))
    (loop for i from start below end do
      (case (char string i)
        (#\newline
         (seos-write-vector stream string seg-start i)
         (seos-write-newline stream nil)
         (setq seg-start (1+ i)))
        (#\tab
         (seos-write-vector stream string seg-start i)
         (seos-write-vector stream *tab-string* 0 (length *tab-string*))
         (setq seg-start (1+ i)))))
    (seos-write-vector stream string seg-start end))
  (seos-finish-output stream)
  string)

(defmethod stream-character-width ((stream standard-extended-output-stream) char
                                   &key (text-style nil))
  (with-sheet-medium (medium stream)
    (text-style-character-width (or text-style (medium-text-style medium))
                                medium
                                char)))

(defmethod stream-string-width ((stream standard-extended-output-stream) string
                                &key (start 0) (end nil) (text-style nil))
  (with-sheet-medium (medium stream)
    (if (null text-style)
        (setq text-style (medium-text-style (sheet-medium stream))))
    (multiple-value-bind (total-width total-height final-x)
        (text-size medium string :text-style text-style
                   :start start :end end)
      (declare (ignore total-height))
      (values final-x total-width))))

(defmethod stream-text-margin ((stream standard-extended-output-stream))
  (bounding-rectangle-max-x (stream-page-region stream)))

(defmethod (setf stream-text-margin) (margin (stream standard-extended-output-stream))
  (setf (stream-text-margins stream)
        (if margin
            `(:right (:absolute ,margin))
            `(:right (:relative 0)))))

(defmethod stream-line-height ((stream standard-extended-output-stream)
                               &key (text-style nil))
  (with-sheet-medium (medium stream)
    (+ (text-style-height (or text-style (medium-text-style medium)) medium)
       (stream-vertical-spacing stream))))

(defmethod stream-line-width ((stream standard-extended-output-stream))
  (bounding-rectangle-width (stream-page-region stream)))

(defmethod stream-line-column ((stream standard-extended-output-stream))
  (let ((line-width (- (stream-cursor-position stream)
                       (stream-cursor-initial-position stream))))
    (if (minusp line-width)
        nil
        ;; Some PPRINT implemenations require STREAM-LINE-COLUMN to return an
        ;; integer here. May be worth revising in the future. -- jd 2021-12-18
        (floor (/ line-width (stream-character-width stream #\M))))))

(defmethod stream-start-line-p ((stream standard-extended-output-stream))
  (zerop (stream-text-offset stream (stream-text-cursor stream))))

(defmethod beep (&optional medium)
  (if medium
      (medium-beep medium)
      (when (sheetp *standard-output*)
        (medium-beep (sheet-medium *standard-output*)))))

(defmethod invoke-with-local-coordinates ((medium extended-output-stream) cont x y)
  ;; For now we do as real CLIM does.
  ;; Default seems to be the cursor position.
  ;; Moore suggests we use (0,0) if medium is no stream.
  ;;
  ;; Furthermore, the specification is vague about possible scalings ...
  (unless (and x y)
    (multiple-value-bind (cx cy) (stream-cursor-position medium)
      (orf x cx)
      (orf y cy)))
  (multiple-value-bind (mxx mxy myy myx tx ty)
      (get-transformation (medium-transformation medium))
    (declare (ignore tx ty))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   mxx mxy myy myx
                                   x y))
        (funcall cont medium)))))

(defmethod invoke-with-first-quadrant-coordinates ((medium extended-output-stream) cont x y)
  ;; First we do the same as invoke-with-local-coordinates but rotate and deskew
  ;; it so that it becomes first-quadrant. We do this by simply measuring the
  ;; length of the transformed x and y "unit vectors".  [That is (0,0)-(1,0) and
  ;; (0,0)-(0,1)] and setting up a transformation which features an upward
  ;; pointing y-axis and a right pointing x-axis with a length equal to above
  ;; measured vectors.
  (unless (and x y)
    (multiple-value-bind (cx cy) (stream-cursor-position medium)
      (orf x cx)
      (orf y cy)))
  (let* ((tr (medium-transformation medium))
         (xlen
          (multiple-value-bind (dx dy) (transform-distance tr 1 0)
            (sqrt (+ (expt dx 2) (expt dy 2)))))
         (ylen
          (multiple-value-bind (dx dy) (transform-distance tr 0 1)
            (sqrt (+ (expt dx 2) (expt dy 2))))))
    (with-identity-transformation (medium)
      (with-drawing-options
          (medium :transformation (make-transformation
                                   xlen 0 0 (- ylen)
                                   x y))
        (funcall cont medium)))))

;;; Backend part of the output destination mechanism
;;;
;;; See clim-core/commands.lisp for the "user interface" part.

(defgeneric invoke-with-standard-output (continuation destination)
  (:documentation
   "Call CONTINUATION (with no arguments) with *STANDARD-OUTPUT*
rebound according to DESTINATION."))

(defmethod invoke-with-standard-output (continuation (destination null))
  ;; Call CONTINUATION without rebinding *STANDARD-OUTPUT* at all.
  (funcall continuation))

(defclass output-destination ()
  ())

(defclass stream-destination (output-destination)
  ((destination-stream :accessor destination-stream
                       :initarg :destination-stream)))

(defmethod invoke-with-standard-output
    (continuation (destination stream-destination))
  (let ((*standard-output* (destination-stream destination)))
    (funcall continuation)))

(defclass file-destination (output-destination)
  ((file :reader destination-file :initarg :file)))

(defmethod destination-element-type ((destination file-destination))
  :default)

(defmethod invoke-with-standard-output
    (continuation (destination file-destination))
  (with-open-file (*standard-output* (destination-file destination)
                                     :element-type (destination-element-type
                                                    destination)
                                     :direction :output
                                     :if-exists :supersede)
    (funcall continuation)))

(defparameter *output-destination-types*
  '(("Stream" stream-destination)))

(defun register-output-destination-type (name class-name)
  (let ((class (find-class class-name nil)))
    (cond ((null class)
           (error "~@<~S is not the name of a class.~@:>" class-name))
          ((not (subtypep class #1='output-destination))
           (error "~@<~A is not a subclass of ~S.~@:>" class #1#))))
  (pushnew (list name class-name) *output-destination-types* :test #'equal))
