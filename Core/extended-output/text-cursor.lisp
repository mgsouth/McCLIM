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

;;; Standard-Text-Cursor class
(defclass standard-text-cursor (cursor)
  ((sheet :initarg :sheet :accessor cursor-sheet)
   (x :initarg :x-position :accessor cursor-position-x)
   (y :initarg :y-position :accessor cursor-position-y)
   (dx :initarg :x-offset :accessor cursor-offset-x)
   (dy :initarg :y-offset :accessor cursor-offset-y)
   (ex :initarg :x-extent :accessor cursor-extent-x)
   (ey :initarg :y-extent :accessor cursor-extent-y)
   ;; XXX what does "cursor is active" mean?
   ;; It means that the sheet (stream) updates the cursor, though currently the
   ;; cursor appears to be always updated after stream text operations. -- moore
   (cursor-active :accessor cursor-active)
   (cursor-state  :accessor cursor-state)))

(defmethod equals ((a standard-text-cursor) (b standard-text-cursor))
  (and (coordinate= (slot-value a 'x) (slot-value b 'x))
       (coordinate= (slot-value a 'y) (slot-value b 'y))
       (coordinate= (cursor-offset-x a) (cursor-offset-x b))
       (coordinate= (cursor-offset-y a) (cursor-offset-y b))
       (coordinate= (cursor-extent-x a) (cursor-extent-x b))
       (coordinate= (cursor-extent-y a) (cursor-extent-y b))))

(defmethod initialize-instance :after
    ((object standard-text-cursor) &key (visibility :on))
  (setf (cursor-visibility object) visibility)
  (setf (cursor-position object) (values 0 0))
  (setf (cursor-offset object) (values 0 0))
  (setf (cursor-extent object) (values 0 0)))

(defmethod bounding-rectangle* ((cursor standard-text-cursor))
  (with-slots (x y dx dy ex ey) cursor
    (values x y (+ x dx ex) (+ y dy ey))))

(defmethod print-object ((cursor standard-text-cursor) stream)
  (with-slots (x y) cursor
    (print-unreadable-object (cursor stream :type t :identity t)
      (format stream "~D ~D" x y))))

(defmethod cursor-focus ((cursor standard-text-cursor))
  (when-let* ((sheet (cursor-sheet cursor))
              (port (port sheet)))
    (eq sheet (port-keyboard-input-focus port))))

(defmethod draw-design (sheet (cursor standard-text-cursor) &rest args)
  (when (cursor-state cursor)
    (with-bounding-rectangle* (x1 y1 x2 y2) cursor
      (let ((ink (if (and (cursor-focus cursor)
                          (cursor-active cursor))
                     +foreground-ink+ +dark-grey+)))
        (apply #'draw-rectangle* sheet x1 y1 x2 y2
               (append args (list :filled t :ink ink)))))))

(defmethod cursor-visibility ((cursor standard-text-cursor))
  (if (cursor-active cursor)
      (if (cursor-state cursor)
          :on
          :off)
      nil))

(defmethod (setf cursor-visibility) (nv (cursor standard-text-cursor))
  (multiple-value-bind (active state)
      (ecase nv
        ((:on t) (values t t))
        (:off    (values t nil))
        ((nil)   (values nil nil)))
    (setf (cursor-state cursor)  state
          (cursor-active cursor) active)))

(defmethod cursor-position ((cursor standard-text-cursor))
  (with-slots (x y) cursor
    (values x y)))

(defmethod* (setf cursor-position) (nx ny (cursor standard-text-cursor))
  (with-slots (x y) cursor
    (setf (values x y) (values (or nx x) (or ny y)))))

(defmethod cursor-offset ((cursor standard-text-cursor))
  (with-slots (dx dy) cursor
    (values dx dy)))

(defmethod* (setf cursor-offset) (nx ny (cursor standard-text-cursor))
  (with-slots (dx dy) cursor
    (setf (values dx dy) (values (or nx dx) (or ny dy)))))

(defmethod cursor-extent ((cursor standard-text-cursor))
  (with-slots (ex ey) cursor
    (values ex ey)))

(defmethod* (setf cursor-extent) (nx ny (cursor standard-text-cursor))
  (with-slots (ex ey) cursor
    (setf (values ex ey) (values (or nx ex) (or ny ey)))))

(defun cursor-width (cursor)
  (abs (+ (cursor-offset-x cursor) (cursor-extent-x cursor))))

(defun cursor-height (cursor)
  (abs (+ (cursor-offset-y cursor) (cursor-extent-y cursor))))

(defun cursor-size (cursor)
  (values (cursor-width cursor) (cursor-height cursor)))

(defun update-cursor (target source)
  (setf (cursor-position target) (cursor-position source)
        (cursor-offset target) (cursor-offset source)
        (cursor-extent target) (cursor-extent source))
  target)

;;; This macro is used to ensure that the cursor is restored to its old state
;;; after the operation. -- jd 2024-01-05
(defmacro with-cursor-off ((cursor) &body body)
  `(letf (((cursor-visibility ,cursor) nil)
          ((cursor-position ,cursor) (values 0 0))
          ((cursor-offset ,cursor)   (values 0 0))
          ((cursor-extent ,cursor)   (values 0 0)))
     ,@body))
