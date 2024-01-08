;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;  (c) Copyright 2023 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Text formatting utilities.
;;;

(in-package #:clim-internals)

;;; Mixin is used to store text-style and ink when filling-output it is invoked.

(defclass filling-output-mixin (gs-ink-mixin gs-text-style-mixin)
  ((lbs :accessor line-break-strategy :initarg :line-break-strategy
        :documentation "T for a default word wrap or a list of break characters.")
   (alb :accessor after-line-break :initarg :after-line-break
        :documentation "Function accepting stream to call after the line break."))
  (:default-initargs :line-break-strategy t :after-line-break nil))

(defgeneric invoke-with-filling-output (stream continuation fresh-line-fn
                                        &key fill-width break-characters)
  (:method ((stream filling-output-mixin) continuation fresh-line-fn
            &key
              (fill-width (bounding-rectangle-max-x (stream-page-region stream)))
              break-characters)
    (with-temporary-margins (stream :right `(:absolute ,fill-width))
      (letf (((stream-end-of-line-action stream) :wrap*)
             ((line-break-strategy stream) break-characters)
             ((after-line-break stream) fresh-line-fn))
        (when (stream-start-line-p stream)
          (funcall fresh-line-fn stream nil))
        (funcall continuation stream)))))

(defmacro filling-output ((stream &rest args
                                  &key
                                  fill-width
                                  break-characters
                                  after-line-break
                                  (after-line-break-composed t)
                                  (after-line-break-initially nil)
                                  (after-line-break-subsequent t))
                          &body body)
  (declare (ignore fill-width break-characters))
  (with-stream-designator (stream '*standard-output*)
    (with-keywords-removed (args (:after-line-break
                                  :after-line-break-composed
                                  :after-line-break-initially
                                  :after-line-break-subsequent))
      (with-gensyms (continuation old-fresh-line-fn fresh-line-fn
                                  initial-indent ink text-style)
        (once-only (after-line-break
                    after-line-break-composed
                    after-line-break-initially
                    after-line-break-subsequent)
          `(let ((,initial-indent (stream-cursor-initial-position ,stream))
                 (,old-fresh-line-fn (after-line-break ,stream))
                 (,ink (medium-ink ,stream))
                 (,text-style (medium-text-style ,stream)))
             (flet ((,continuation (,stream)
                      ,@body)
                    (,fresh-line-fn (,stream soft-newline-p)
                      (when (and ,after-line-break-composed ,old-fresh-line-fn)
                        (funcall ,old-fresh-line-fn ,stream soft-newline-p))
                      (when (null ,after-line-break)
                        (return-from ,fresh-line-fn))
                      (multiple-value-bind (cx cy) (stream-cursor-position ,stream)
                        (setf (stream-cursor-position ,stream) (values ,initial-indent cy))
                        (when (or (and ,after-line-break-initially (null soft-newline-p))
                                  (and ,after-line-break-subsequent soft-newline-p))
                          (with-end-of-line-action (,stream :allow) ; prevent infinite recursion
                            (with-drawing-options (,stream :ink ,ink :text-style ,text-style)
                              (etypecase ,after-line-break
                                (string   (write-string ,after-line-break ,stream))
                                (function (funcall ,after-line-break ,stream soft-newline-p))))))
                        ;; When after-line-break goes beyond the
                        ;; previous position we advance the cursor.
                        (multiple-value-bind (nx ny) (stream-cursor-position ,stream)
                          (stream-set-cursor-position ,stream (max cx nx) (max cy ny))))))
               (declare (dynamic-extent #',continuation #',fresh-line-fn))
               (invoke-with-filling-output ,stream #',continuation #',fresh-line-fn ,@args))))))))

(defgeneric invoke-with-indenting-output
    (stream continuation &key indent move-cursor)
  (:method (stream continuation &key indent (move-cursor t))
    (let ((left-margin (copy-list (getf (stream-text-margins stream) :left)))
          (line-beginning (stream-cursor-initial-position stream)))
      (setf (second left-margin)
            (+ (parse-space stream (second left-margin) :horizontal)
               (parse-space stream indent :horizontal)))
      (with-temporary-margins (stream :left left-margin :move-cursor move-cursor)
        (flet ((fix-cursor-position (from-value to-value)
                 ;; We purposefully bypass the protocol to adjust
                 ;; cursor-position. Roundabout way with accessors is
                 ;; possible but obfuscates the intent. -- jd 2019-03-07
                 (when (= (stream-cursor-position stream) from-value)
                   (setf (slot-value (stream-text-cursor stream) 'x) to-value))))
          (fix-cursor-position line-beginning (second left-margin))
          (unwind-protect (funcall continuation stream)
            (fix-cursor-position (second left-margin) line-beginning)))))))

(defmacro indenting-output ((stream indentation &rest args &key move-cursor) &body body)
  (declare (ignore move-cursor))
  (with-stream-designator (stream '*standard-output*)
    (with-gensyms (continuation)
      `(flet ((,continuation (,stream) ,@body))
         (declare (dynamic-extent #',continuation))
         (invoke-with-indenting-output ,stream #',continuation :indent ,indentation ,@args)))))

;;; This fallback implementation is meant to work on any sheet with the output
;;; protocol.
(defmethod invoke-with-room-for-graphics (cont stream
                                          &key (first-quadrant t)
                                               (width 100)
                                               (height 100)
                                               (move-cursor t)
                                               (record-type nil))
  (declare (ignore record-type))
  (with-sheet-medium (medium stream)
    (multiple-value-bind (cx cy) (transform-position (medium-transformation medium) 0 0)
      (multiple-value-bind (cy* transformation)
          (if (not first-quadrant)
              (values cy +identity-transformation+)
              (values (+ cy height)
                      (make-scaling-transformation 1 -1)))
        (letf (((medium-transformation medium)
                (compose-transformation-with-translation transformation cx cy*)))
          (funcall cont stream)))
      (if move-cursor
          (values (+ cx width) cy)
          (values cx cy)))))

(defmethod invoke-with-room-for-graphics (cont (stream extended-output-stream)
                                          &key (first-quadrant t)
                                            (width (stream-character-width stream #\M))
                                            (height (stream-cursor-height stream))
                                            (move-cursor t)
                                            (record-type nil))
  (declare (ignore record-type))
  (with-sheet-medium (medium stream)
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (multiple-value-bind (cy* transformation)
          (if (not first-quadrant)
              (values cy +identity-transformation+)
              (values (+ cy (stream-baseline stream))
                      (make-scaling-transformation 1 -1)))
        (letf (((medium-transformation medium)
                (compose-transformation-with-translation transformation cx cy*)))
          (funcall cont stream)))
      (maxf (stream-cursor-height stream) height)
      (setf (stream-cursor-position stream)
            (if move-cursor
                (values (+ cx width) cy)
                (values cx cy))))))


(defmethod invoke-with-room-for-graphics
    (cont (stream output-recording-stream)
     &key (first-quadrant t) width height (move-cursor t)
       (record-type 'standard-sequence-output-record))
  (with-sheet-medium (medium stream)
    (multiple-value-bind (cx cy) (stream-cursor-position stream)
      (multiple-value-bind (cy* transformation)
          (if (not first-quadrant)
              (values cy +identity-transformation+)
              (values (+ cy (stream-baseline stream))
                      (make-scaling-transformation 1 -1)))
        (letf (((medium-transformation medium)
                (compose-transformation-with-translation transformation cx cy*)))
          (let ((record (with-new-output-record (stream record-type)
                          (funcall cont stream))))
            (with-bounding-rectangle* (:x2 x2 :y2 y2) record
              (orf width (- x2 cx))
              (orf height (- y2 cy))))))
      (maxf (stream-cursor-height stream) height)
      (setf (stream-cursor-position stream)
            (if move-cursor
                (values (+ cx width) cy)
                (values cx cy))))))

;;; FIXME: think about merging behavior by using WITH-LOCAL-COORDINATES and
;;; WITH-FIRST-QUADRANT-COORDINATES which both work on both mediums and
;;; streams. Also write a documentation chapter describing behavior and
;;; providing some examples.
;;;
;;; ----------------------------------------------------------------------------
;;; Complicated, underspecified...
;;;
;;; From examining old Genera documentation, I believe that
;;; with-room-for-graphics is supposed to set the medium transformation to
;;; give the desired coordinate system; i.e., it doesn't preserve any
;;; rotation, scaling or translation in the current medium transformation.

(defmacro with-room-for-graphics ((&optional (stream t)
                                   &rest arguments
                                   &key (first-quadrant t)
                                     width
                                     height
                                     (move-cursor t)
                                     (record-type ''standard-sequence-output-record))
                                  &body body)
  (declare (ignore first-quadrant width height move-cursor record-type))
  (let ((cont (gensym "CONT.")))
    (with-stream-designator (stream '*standard-output*)
      `(labels ((,cont (,stream)
                  ,@body))
         (declare (dynamic-extent #',cont))
         (invoke-with-room-for-graphics #',cont ,stream ,@arguments)))))


;;; formatting functions
(defun format-textual-list (sequence printer
                            &key stream separator conjunction
                              suppress-separator-before-conjunction
                              suppress-space-after-conjunction)
  "Outputs the SEQUENCE of items as a \"textual list\" into
STREAM. PRINTER is a function of an item and a stream. Between each
two items the string SEPARATOR is placed. If the string CONJUCTION is
supplied, it is placed before the last item.

SUPPRESS-SEPARATOR-BEFORE-CONJUNCTION and
SUPPRESS-SPACE-AFTER-CONJUNCTION are non-standard."
  (orf stream *standard-output*)
  (orf separator ", ")
  (let* ((length (length sequence))
         (n-rest length))
    (map-repeated-sequence nil 1
                           (lambda (item)
                             (funcall printer item stream)
                             (decf n-rest)
                             (cond ((> n-rest 1)
                                    (princ separator stream))
                                   ((= n-rest 1)
                                    (if conjunction
                                        (progn
                                          (unless suppress-separator-before-conjunction
                                            (princ separator stream))
                                          (princ conjunction stream)
                                          (unless suppress-space-after-conjunction
                                            (princ #\space stream)))
                                        (princ separator stream)))))
                           sequence)))
