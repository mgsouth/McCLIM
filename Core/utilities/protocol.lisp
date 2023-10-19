;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Low level protocol (with extensions).
;;;

(in-package "CLIM-INTERNALS")

;;; B The CLIM-SYS Package

;;; B.1 Resources
(pledge :macro defresource (name parameters &key constructor initializer deinitializer matcher initial-copies))
(pledge :macro using-resource ((variable name &rest parameters) &body body))
(declfun allocate-resource (name &rest parameters))
(declfun deallocate-resource (name object))
(declfun clear-resource (name))
(declfun name-resource (fun name))

;;; B.2 Multiprocessing
(pledge :variable *multiprocessing-p*)
(declfun make-process (fun &key name))
(declfun destroy-process (process))
(declfun current-process nil)
(declfun all-processes nil)
(declfun processp (object))
(declfun process-name (process))
(declfun process-state (process))
(declfun process-whostate (process))
(declfun process-wait (reason predicate))
(declfun process-wait-with-timeout (reason timeout predicate))
(declfun process-yield nil)
(declfun process-interrupt (process fun))
(declfun disable-process (process))
(declfun enable-process (process))
(declfun restart-process (process))
(pledge :macro without-scheduling (&body body))
(declfun atomic-incf (reference))
(declfun atomic-decf (reference))
(declfun make-condition-variable nil)
(declfun condition-wait (cv lock &optional timeout))
(declfun condition-notify (cv))

;;; B.3 Locks
(declfun make-lock (&optional name))
(pledge :macro with-lock-held ((place &optional state) &body body))
(declfun make-recursive-lock (&optional name))
(pledge :macro with-recursive-lock-held ((place &optional state) &body body))

;;; B.4 Multiple Value setf
(pledge :macro defgeneric* (name lambda-list &body options))
(pledge :macro defmethod* (name lambda-list &body body))

;;; C Encapsulating Streams
(define-protocol-class encapsulating-stream ()) ; initargs (:stream)
(pledge :class standard-encapsulating-stream (encapsulating-stream))
(defgeneric encapsulating-stream-stream (encapsulating-stream)
  (:documentation "The stream encapsulated by an encapsulating stream"))


;;; Extra operators
(defgeneric equals (a b)
  (:method (a b)
    (equalp a b)))


;;; KLUDGE both PORT and SHEET are referenced through the codebase so we need to
;;; define protocol classes early.

(define-protocol-class sheet nil)
(define-protocol-class port nil)
