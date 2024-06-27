;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2002,2003 Timothy Moore <tmoore@common-lisp.net>
;;;  (c) copyright 2008 Troels Henriksen <thenriksen@common-lisp.net>
;;;  (c) copyright 2019 admich <andrea.demichele@gmail.com>
;;;  (c) copyright 2019-2021 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;  (c) copyright 2024 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Frame pointer documentation computation and updating.
;;;

(in-package #:clim-internals)

(defconstant +button-documentation+ '((#.+pointer-left-button+ "L")
                                      (#.+pointer-middle-button+ "M")
                                      (#.+pointer-right-button+ "R")
                                      (#.+pointer-wheel-up+ "WheelUp")
                                      (#.+pointer-wheel-down+ "WheelDown")
                                      (#.+pointer-wheel-left+ "WheelLeft")
                                      (#.+pointer-wheel-right+ "WheelRight")))

(defconstant +modifier-documentation+
  '((#.+shift-key+ "sh" "Shift")
    (#.+control-key+ "c" "Control")
    (#.+meta-key+ "m" "Meta")
    (#.+super-key+ "s" "Super")
    (#.+hyper-key+ "h" "Hyper")))

;;; Give a coherent order to sets of modifier combinations.  Multi-key combos
;;; come after single keys.

(defun cmp-modifiers (a b)
  (let ((cnt-a (logcount a))
        (cnt-b (logcount b)))
    (cond ((eql cnt-a cnt-b)
           (< a b))
          (t (< cnt-a cnt-b)))))

(defun print-modifiers (stream modifiers style)
  (if (zerop modifiers)
      (when (eq style :long)
        (write-string "<nothing>" stream))
      (loop with trailing = nil
            for (bit short long) in +modifier-documentation+
            for arguments = (if (eq style :short) short long)
            when (logtest bit modifiers)
              do (format stream "~:[~;-~]~A" trailing arguments)
                 (setq trailing t))))

;;; We update the existing cached record to maintain equality when we compare
;;; the old and the new pointer documentation state.
(defun pointer-documentation-blank-area (sheet event)
  (when-let ((pd-stream *pointer-documentation-output*))
    (let ((record (%pointer-documentation-blank-area pd-stream))
          (x (device-event-x event))
          (y (device-event-y event)))
      (if record
          (setf (presentation-type record)
                `((blank-area :sheet ,sheet :region ,(make-point x y))
                  :description "Blank Area")
                (presentation-object record) event
                (rectangle-edges* record) (bounding-rectangle* sheet))
          (setf record (make-blank-area-presentation sheet x y event)
                (%pointer-documentation-blank-area pd-stream) record))
      record)))

(defun compute-pointer-documentation-state (frame input-context stream event)
  (let* ((current-modifier (event-modifier-state event))
         (x (device-event-x event))
         (y (device-event-y event))
         (result '())
         (other-modifiers '())
         (known-size nil))
    (labels ((presentation-size (presentation)
               (or known-size
                   (multiple-value-bind (min-x min-y max-x max-y)
                       (output-record-hit-detection-rectangle* presentation)
                     (* (- max-x min-x) (- max-y min-y)))))
             (consider-gesture (translator presentation context button size)
               (let* ((existing (assoc-value result button))
                      (existing-context (third existing))
                      (existing-size (fourth existing)))
                 (when (or (not existing)
                           (and (eq context existing-context)
                                (< size existing-size)))
                   (setf (assoc-value result button)
                         (list presentation translator context size)))))
             (consider-translator (translator presentation context)
               (let ((gesture (gestures-for-pointer-documentation (gesture translator))))
                   (loop with size = (presentation-size presentation)
                         for (nil button modifier) in gesture
                         do (if (or (eql modifier t) (eql modifier current-modifier))
                                (consider-gesture translator presentation context
                                                  button size)
                                (pushnew modifier other-modifiers)))))
             (map-translators (history)
               (map-applicable-translators
                #'consider-translator history
                input-context frame stream x y :for-menu :for-documentation)))
      (map-translators (stream-output-history stream))
      (setf known-size most-positive-fixnum)
      (map-translators (pointer-documentation-blank-area stream event)))
    (list current-modifier (sort result #'< :key #'car) other-modifiers)))

(defun print-pointer-documentation (pstream stream state event)
  (unless state
    (return-from print-pointer-documentation nil))
  (destructuring-bind (current-modifier new-translators other-modifiers) state
    (let ((x (device-event-x event))
          (y (device-event-y event)))
      (when new-translators
        (loop for (button presentation translator context) in new-translators
              for name = (cadr (assoc button +button-documentation+))
              for first-one = t then nil
              do (progn
                   (unless first-one
                     (write-string "; " pstream))
                   (unless (zerop current-modifier)
                     (print-modifiers pstream current-modifier :short)
                     (write-string "-" pstream))
                   (format pstream "~A: " name)
                   (document-presentation-translator translator
                                                     presentation
                                                     (input-context-type context)
                                                     *application-frame*
                                                     event
                                                     stream
                                                     x y
                                                     :stream pstream
                                                     :documentation-type
                                                     :pointer))
              finally (when new-translators
                        (write-char #\. pstream))))
      (when other-modifiers
        (setf other-modifiers (sort other-modifiers #'cmp-modifiers))
        (terpri pstream)
        (write-string "To see other commands, press "	pstream)
        (loop for modifier-tail on other-modifiers
              for (modifier) = modifier-tail
              for count from 0
              do (progn
                   (if (null (cdr modifier-tail))
                       (progn
                         (when (> count 1)
                           (write-char #\, pstream))
                         (when (> count 0)
                           (write-string " or " pstream)))
                       (when (> count 0)
                         (write-string ", " pstream)))
                   (print-modifiers pstream modifier :long)))
        (write-char #\. pstream)))))

(defun update-pointer-documentation (frame input-context stream event)
  (when-let ((pd-stream *pointer-documentation-output*))
    (let ((old-state (pointer-documentation-state pd-stream))
          (new-state (compute-pointer-documentation-state frame input-context stream event)))
      (unless (equal old-state new-state)
        (setf (pointer-documentation-state pd-stream) new-state)
        (with-output-buffered (pd-stream)
          (window-clear pd-stream)
          (print-pointer-documentation pd-stream stream new-state event))
        (dispatch-repaint pd-stream +everywhere+)))))


