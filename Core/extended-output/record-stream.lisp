;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 1998,1999,2000,2001,2003 Michael McDonald <mikemac@mikemac.com>
;;;  (c) copyright 2000-2003,2009-2016 Robert Strandh <robert.strandh@gmail.com>
;;;  (c) copyright 2001 Arnaud Rouanet <rouanet@emi.u-bordeaux.fr>
;;;  (c) copyright 2001 Lionel Salabartan <salabart@emi.u-bordeaux.fr>
;;;  (c) copyright 2001,2002 Alexey Dejneka <adejneka@comail.ru>
;;;  (c) copyright 2002,2003,2004 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2002,2003,2004,2005 Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) copyright 2003-2008 Andy Hefner <ahefner@common-lisp.net>
;;;  (c) copyright 2005,2006 Christophe Rhodes <crhodes@common-lisp.net>
;;;  (c) copyright 2006 Andreas Fuchs <afuchs@common-lisp.net>
;;;  (c) copyright 2007 David Lichteblau <dlichteblau@common-lisp.net>
;;;  (c) copyright 2007 Robert Goldman <rgoldman@common-lisp.net>
;;;  (c) copyright 2017 Cyrus Harmon <cyrus@bobobeach.com>
;;;  (c) copyright 2018 Elias Martenson <lokedhs@gmail.com>
;;;  (c) copyright 2018-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2016-2024 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Output recording streams.
;;;

(in-package #:clim-internals)

;;; Macro masturbation...

(defmacro define-invoke-with (macro-name func-name record-type doc-string)
  `(defmacro ,macro-name ((stream
                           &optional
                           (record-type '',record-type)
                           (record (gensym))
                           &rest initargs)
                          &body body)
     ,doc-string
     (with-stream-designator (stream '*standard-output*)
       (with-gensyms (continuation)
         (multiple-value-bind (bindings m-i-args)
             (rebind-arguments initargs)
           `(let ,bindings
              (flet ((,continuation (,stream ,record)
                       ,(declare-ignorable-form* stream record)
                       ,@body))
                (declare (dynamic-extent #',continuation))
                (,',func-name ,stream #',continuation ,record-type ,@m-i-args))))))))

(define-invoke-with with-new-output-record invoke-with-new-output-record
  standard-sequence-output-record
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record, and inserts the new
record into the current \"open\" output record assotiated with STREAM.
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used.")

(define-invoke-with with-output-to-output-record
    invoke-with-output-to-output-record
  standard-sequence-output-record
  "Creates a new output record of type RECORD-TYPE and then captures
the output of BODY into the new output record. The cursor position of
STREAM is initially bound to (0,0)
    If RECORD is supplied, it is the name of a variable that will be
lexically bound to the new output record inside the body. INITARGS are
CLOS initargs that are passed to MAKE-INSTANCE when the new output
record is created.
    It returns the created output record.
    The STREAM argument is a symbol that is bound to an output
recording stream. If it is T, *STANDARD-OUTPUT* is used.")

(defmacro with-output-recording-options ((stream
                                          &key (record nil record-supplied-p)
                                               (draw nil draw-supplied-p))
                                         &body body)
  (with-stream-designator (stream '*standard-output*)
    (with-gensyms (continuation)
      `(flet ((,continuation  (,stream)
                ,(declare-ignorable-form* stream)
                ,@body))
         (declare (dynamic-extent #',continuation))
         (with-drawing-options (,stream)
           (invoke-with-output-recording-options
            ,stream #',continuation
            ,(if record-supplied-p record `(stream-recording-p ,stream))
            ,(if draw-supplied-p draw `(stream-drawing-p ,stream))))))))

(defun replay (record stream &optional region)
  (when (typep stream 'encapsulating-stream)
    (return-from replay (replay record (encapsulating-stream-stream stream) region)))
  (unless region
    (setf region (sheet-visible-region stream)))
  (with-output-recording-options (stream :draw nil)
    (stream-close-text-output-record stream))
  (when (stream-drawing-p stream)
    (nest
     (with-cursor-off ((stream-text-cursor stream)))
     (with-output-recording-options (stream :record nil))
     (with-identity-transformation (stream)
       (replay-output-record record stream region)))))

(defmethod replay-output-record
    ((record compound-output-record) (stream encapsulating-stream)
     &optional region (x-offset 0) (y-offset 0))
  (replay-output-record record (encapsulating-stream-stream stream)
                        region x-offset y-offset))

(defmethod highlight-output-record ((record output-record) stream state)
  (with-identity-transformation (stream)
    (ecase state
      (:highlight
       ;; We can't "just" draw-rectangle :filled nil because the path lines
       ;; rounding may get outside the bounding rectangle. -- jd 2019-02-01
       (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
         (draw-design stream
                      (if (or (> (1+ x1) (1- x2))
                              (> (1+ y1) (1- y2)))
                          (bounding-rectangle record)
                          (region-difference
                           (bounding-rectangle record)
                           (make-rectangle* (1+ x1) (1+ y1) (1- x2) (1- y2))))
                      :ink +foreground-ink+)))
      (:unhighlight
       (dispatch-repaint stream (bounding-rectangle record))))))


(defclass updating-output-stream-mixin (updating-output-map-mixin)
  ((redisplaying-p
    :initform nil
    :reader stream-redisplaying-p)
   (incremental-redisplay
    :initform nil
    :initarg :incremental-redisplay
    :accessor pane-incremental-redisplay)
   ;; For incremental output, holds the top level updating-output-record.
   (updating-record
    :initform nil
    :initarg :updating-record
    :accessor updating-record)))

(defmacro with-stream-redisplaying ((stream) &body body)
  `(letf (((slot-value ,stream 'redisplaying-p) t)) ,@body))

(defmethod redisplayable-stream-p ((stream updating-output-stream-mixin))
  (declare (ignore stream))
  t)

(defmethod pane-needs-redisplay :around ((pane updating-output-stream-mixin))
  (let ((redisplayp (call-next-method)))
    (values redisplayp (and (not (eq redisplayp :no-clear))
                            (not (pane-incremental-redisplay pane))))))

(defmethod window-clear :after ((pane updating-output-stream-mixin))
  "Get rid of any updating output records stored in the stream; they're gone
  from the screen."
  (clear-map pane))



;;; 16.4. Output Recording Streams
(defclass standard-output-recording-stream (updating-output-stream-mixin
                                            output-recording-stream)
  ((recording-p :initform t :reader stream-recording-p)
   (drawing-p :initform t :accessor stream-drawing-p)
   (output-history :initform (make-instance 'standard-tree-output-history)
                   :initarg :output-record
                   :reader stream-output-history)
   (current-output-record :accessor stream-current-output-record)
   (current-text-output-record :accessor stream-current-text-output-record))
  (:documentation "This class is mixed into some other stream class to
add output recording facilities. It is not instantiable."))

(defmethod initialize-instance :after
    ((stream standard-output-recording-stream) &rest args)
  (declare (ignore args))
  (let ((history (stream-output-history stream)))
    (setf (slot-value history 'stream) stream
          (slot-value stream 'current-text-output-record) nil
          (slot-value stream 'output-history) history
          (stream-current-output-record stream) history)))

;;; 16.4.1 The Output Recording Stream Protocol
(defmethod (setf stream-recording-p)
    (recording-p (stream standard-output-recording-stream))
  (let ((old-val (slot-value stream 'recording-p)))
    (unless (eq old-val recording-p)
      (setf (slot-value stream 'recording-p) recording-p)
      (stream-close-text-output-record stream))
    recording-p))

(defmethod stream-add-output-record
    ((stream standard-output-recording-stream) record)
  (add-output-record record (stream-current-output-record stream)))

(defmethod stream-replay ((stream standard-output-recording-stream)
                          &optional (region (sheet-visible-region stream)))
  (replay (stream-output-history stream) stream region))

(defun output-record-ancestor-p (ancestor child)
  (loop for record = child then parent
     for parent = (output-record-parent record)
     when (eq parent nil) do (return nil)
     when (eq parent ancestor) do (return t)))

(defmethod erase-output-record (record (stream standard-output-recording-stream)
                                &optional (errorp t))
  (with-output-recording-options (stream :record nil)
    (let ((region (rounded-bounding-rectangle record))
          (parent (output-record-parent record)))
      (cond
        ((output-record-ancestor-p (stream-output-history stream) record)
         (delete-output-record record parent))
        (errorp
         (error "~S is not contained in ~S." record stream)))
      (with-bounding-rectangle* (x1 y1 x2 y2) region
        (draw-rectangle* stream x1 y1 x2 y2 :ink +background-ink+)
        (stream-replay stream region)))))

;;; 16.4.3. Text Output Recording
(defmethod stream-text-output-record
    ((stream standard-output-recording-stream) text-style)
  (declare (ignore text-style))
  (or (stream-current-text-output-record stream)
      (multiple-value-bind (cx cy) (stream-cursor-position stream)
        (setf (stream-current-text-output-record stream)
              (make-instance 'standard-text-displayed-output-record
                             :x-position cx :y-position cy :stream stream)))))

(defmethod stream-close-text-output-record ((stream standard-output-recording-stream))
  (when-let ((record (stream-current-text-output-record stream)))
    (setf (stream-current-text-output-record stream) nil)
    (when (stream-recording-p stream)
      (stream-add-output-record stream record))
    (when (stream-drawing-p stream)
      (with-output-recording-options (stream :record nil)
        (with-identity-transformation (stream)
          (replay-output-record record stream))))
    t))

(defmethod stream-add-character-output ((stream standard-output-recording-stream)
                                        character text-style width height baseline)
  (add-character-output-to-text-record (stream-text-output-record stream text-style)
                                       character text-style width height baseline))

(defmethod stream-add-string-output ((stream standard-output-recording-stream)
                                     string start end text-style
                                     width height baseline)
  (add-string-output-to-text-record (stream-text-output-record stream text-style)
                                    string start end text-style
                                    width height baseline))

(defun stream-add-record-output (stream record width height base-x base-y)
  (add-object-to-text-record (stream-text-output-record stream nil) record))


;;; 16.4.4. Output Recording Utilities

(defmethod invoke-with-output-recording-options
  ((stream output-recording-stream) continuation record draw)
  "Calls CONTINUATION on STREAM enabling or disabling recording and drawing
according to the flags RECORD and DRAW."
  (letf (((stream-recording-p stream) record)
         ((stream-drawing-p stream) draw))
    (funcall continuation stream)))

(defmethod invoke-with-new-output-record
    ((stream output-recording-stream) continuation record-type
     &rest initargs &key parent)
  (with-keywords-removed (initargs (:parent))
    (stream-close-text-output-record stream)
    (let ((new-record (apply #'make-instance record-type initargs)))
      (letf (((stream-current-output-record stream) new-record))
        ;; Should we switch on recording? -- APD
        (funcall continuation stream new-record)
        (stream-close-text-output-record stream))
      (if parent
          (add-output-record new-record parent)
          (stream-add-output-record stream new-record))
      new-record)))

(defmethod invoke-with-output-to-output-record
    ((stream output-recording-stream) continuation record-type
     &rest initargs)
  (with-pristine-viewport (stream)
    (with-cursor-off ((stream-text-cursor stream))
      (let ((new-record (apply #'make-instance record-type initargs)))
        (with-output-recording-options (stream :record t :draw nil)
          (letf (((stream-current-text-output-record stream) nil)
                 ((stream-current-output-record stream) new-record))
            (funcall continuation stream new-record)
            (stream-close-text-output-record stream)))
        new-record))))

(defmethod invoke-with-output-to-pixmap
    ((sheet output-recording-stream) cont &key width height)
  (unless (and width height)
    ;; What to do when only width or height are given?  And what's the meaning
    ;; of medium-var? -- rudi 2005-09-05
    ;;
    ;; We default WIDTH or HEIGHT to provided values. The output is clipped to a
    ;; rectactangle [0 0 (or width max-x) (height max-y)]. We record the output
    ;; only to learn about dimensions - it is not replayed because the medium
    ;; can't be expected to work with this protocol. To produce the output we
    ;; invoke the continuation again. -- jd 2022-03-16
    (if (output-recording-stream-p sheet)
        (with-bounding-rectangle* (:x2 max-x :y2 max-y)
            (invoke-with-output-to-output-record sheet
                                                 (lambda (sheet record)
                                                   (declare (ignore record))
                                                   (funcall cont sheet))
                                                 'standard-sequence-output-record)
          (setf width (or width max-x)
                height (or height max-y)))
        (error "WITH-OUTPUT-TO-PIXMAP: please provide :WIDTH and :HEIGHT.")))
  (let* ((port (port sheet))
         (pixmap (allocate-pixmap sheet width height))
         (pixmap-medium (make-medium port sheet))
         (drawing-plane (make-rectangle* 0 0 width height)))
    (degraft-medium pixmap-medium port sheet)
    (letf (((medium-drawable pixmap-medium) pixmap)
           ((medium-clipping-region pixmap-medium) drawing-plane)
           ((medium-background pixmap-medium) +transparent-ink+))
      (medium-clear-area pixmap-medium 0 0 width height)
      (funcall cont pixmap-medium)
      pixmap)))

(defmethod invoke-with-clipping-region
    ((sheet output-recording-stream) continuation region)
  (declare (ignore continuation))
  (if (stream-recording-p sheet)
      (with-sheet-medium (medium sheet)
        (let* ((tr (medium-transformation medium))
               (clip (transform-region tr region)))
          (with-new-output-record (sheet 'clipping-output-record record
                                         :clipping-region clip)
            (call-next-method)
            (setf (rectangle-edges* record)
                  (bounding-rectangle*
                   (region-intersection (sheet-region sheet) clip))))))
      (call-next-method)))

;;; FIXME: Change things so the rectangle below is only drawn in response
;;;        to explicit repaint requests from the user, not exposes from X.
;;; FIXME: Use DRAW-DESIGN, that is fix DRAW-DESIGN.
(defmethod handle-repaint ((stream output-recording-stream) region)
  (unless (region-equal region +nowhere+) ; ignore repaint requests for +nowhere+
    (let ((region (if (region-equal region +everywhere+)
                      ;; fallback to the sheet's region for +everwhere+.
                      (sheet-region stream)
                      (bounding-rectangle region))))
      (stream-replay stream region))))

(defmethod scroll-extent :around ((stream output-recording-stream) x y)
  (declare (ignore x y))
  (when (stream-drawing-p stream)
    (call-next-method)))
