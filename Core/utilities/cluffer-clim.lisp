;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020-2024 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Cluffer features a "standard line" that implements line editing as a gap
;;; buffer. Because it uses CHANGE-CLASS to modify the line class, it is not
;;; possible to subclass it. This file implements a line without this drawback.
;;; This code is derived from Cluffer's standard line implementation.

(defpackage "CLUFFER-CLIM"
  (:use "COMMON-LISP")
  (:export
   "LINE"
   "CURSOR"
   "CURSOR-STICKINESS"))
(in-package "CLUFFER-CLIM")

(defclass line (cluffer:line)
  ((%contents     :initarg  :contents
                  :accessor contents)
   (%cursors      :initarg  :cursors
                  :type     list
                  :accessor cursors
                  :initform '())
   ;; No writer since the first line always remains the first line
   ;; since even splitting the first line at position 0 will insert
   ;; the new line after the first line.
   (%first-line-p :initarg  :first-line-p
                  :reader   cluffer:first-line-p
                  :initform t)
   (%last-line-p  :initarg  :last-line-p
                  :accessor last-line-p
                  :reader   cluffer:last-line-p
                  :initform t)
   ;;; The items of an open line are stored in a gap buffer.
   (%gap-start   :initform 0
                 :initarg :gap-start
                 :accessor gap-start)
   (%gap-end     :initform 1
                 :initarg :gap-end
                 :accessor gap-end)
   (%open-line-p :initform nil
                 :initarg :open-line-p
                 :reader open-line-p
                 :accessor %open-line-p))
  (:default-initargs :contents (vector)))

(defun ensure-opened-line (line)
  (unless (open-line-p line)
    (let* ((contents (contents line))
           (item-count (length contents))
           (new-length (max 32 item-count))
           (new-contents (make-array new-length
                                     :element-type (array-element-type contents))))
      (replace new-contents contents :start1 (- new-length item-count) :start2 0)
      (setf (contents line) new-contents
            (gap-start line) 0
            (gap-end line) (- new-length item-count)
            (%open-line-p line) t)
      nil)))

(defun ensure-closed-line (line)
  (when (open-line-p line)
    (let* ((item-count (cluffer:item-count line))
           (contents (contents line))
           (new-contents (make-array item-count)))
      (replace new-contents contents :start1 0 :start2 0 :end2 (gap-start line))
      (replace new-contents contents :start1 (gap-start line) :start2 (gap-end line))
      (setf (contents line) new-contents
            (%open-line-p line) nil)
      nil)))

(defclass cursor (cluffer:cursor)
  ((%line
    :initform nil
    :accessor line
    :reader cluffer:line
    :reader cluffer:cursor-attached-p)
   (%cursor-position
    :accessor cluffer:cursor-position)
   (%cursor-stickiness
    :initarg :stickiness
    :type (member :lsticky :rsticky)
    :accessor cursor-stickiness))
  (:default-initargs :stickiness :rsticky))

(defmethod initialize-instance :after
    ((cursor cursor)
     &key (line nil line-supplied-p)
          (cursor-position 0 cursor-position-supplied-p))
  (cond (line-supplied-p
         (cluffer:attach-cursor cursor line cursor-position))
        (cursor-position-supplied-p
         (error "~@<Cannot supply ~S without also supplying ~S.~@:>"
                :cursor-position :line))))

(defmethod cluffer:attach-cursor ((cursor cursor) (line line)
                                  &optional (position 0))
  (push cursor (cursors line))
  (setf (line cursor) line)
  (setf (cluffer:cursor-position cursor) position)
  nil)

(defmethod cluffer:detach-cursor ((cursor cursor))
  (setf (cursors (line cursor))
        (remove cursor (cursors (line cursor))))
  (setf (line cursor) nil))


(defmethod cluffer:item-count ((line line))
  (if (open-line-p line)
      (- (length (contents line)) (- (gap-end line) (gap-start line)))
      (length (contents line))))

(defmethod cluffer:items ((line line) &key (start 0) (end nil))
  (ensure-closed-line line)
  (if (and (= start 0) (null end))
      (contents line)
      (subseq (contents line) start end)))

(defmethod cluffer:insert-item-at-position ((line line) item position)
  (ensure-opened-line line)
  (let ((contents (contents line)))
    (cond ((= (gap-start line) (gap-end line))
           (let* ((new-length (* 2 (length contents)))
                  (diff (- new-length (length contents)))
                  (new-contents (make-array new-length)))
             (replace new-contents contents
                      :start2 0 :start1 0 :end2 position)
             (replace new-contents contents
                      :start2 position :start1 (+ position diff))
             (setf (gap-start line) position)
             (setf (gap-end line) (+ position diff))
             (setf (contents line) new-contents)))
          ((< position (gap-start line))
           (decf (gap-end line) (- (gap-start line) position))
           (replace contents contents :start2 position :end2 (gap-start line)
                                      :start1 (gap-end line))
           (setf (gap-start line) position))
          ((> position (gap-start line))
           (replace contents contents :start2 (gap-end line)
                                      :start1 (gap-start line) :end1 position)
           (incf (gap-end line) (- position (gap-start line)))
           (setf (gap-start line) position))
          (t
           nil))
    (setf (aref (contents line) (gap-start line)) item)
    (incf (gap-start line))
    (loop for cursor in (cursors line)
          do (when (or (> (cluffer:cursor-position cursor) position)
                       (and (= (cluffer:cursor-position cursor) position)
                            (eql (cursor-stickiness cursor) :rsticky)))
               (incf (cluffer:cursor-position cursor)))))
  nil)

(defmethod cluffer:delete-item-at-position ((line line) position)
  (ensure-opened-line line)
  (let ((contents (contents line)))
    (cond ((< position (gap-start line))
           (decf (gap-end line) (- (gap-start line) position))
           (replace contents contents
                    :start2 position :end2 (gap-start line)
                    :start1 (gap-end line))
           (setf (gap-start line) position))
          ((> position (gap-start line))
           (replace contents contents
                    :start2 (gap-end line)
                    :start1 (gap-start line) :end1 position)
           (incf (gap-end line) (- position (gap-start line)))
           (setf (gap-start line) position))
          (t
           nil))
    (incf (gap-end line))
    (when (and (> (length contents) 32)
               (> (- (gap-end line) (gap-start line))
                  (* 3/4 (length contents))))
      (let* ((new-length (floor (length contents) 2))
             (diff (- (length contents) new-length))
             (new-contents (make-array new-length)))
        (replace new-contents contents
                 :start2 0 :start1 0 :end2 (gap-start line))
        (replace new-contents contents
                 :start2 (gap-end line) :start1 (- (gap-end line) diff))
        (decf (gap-end line) diff)
        (setf (contents line) new-contents)))
    (loop for cursor in (cursors line)
          do (when (> (cluffer:cursor-position cursor) position)
               (decf (cluffer:cursor-position cursor)))))
  nil)

;;; No need to open the line.
(defmethod cluffer:item-at-position ((line line) position)
  (if (open-line-p line)
      (aref (contents line)
            (if (< position (gap-start line))
                position
                (+ position (- (gap-end line) (gap-start line)))))
      (aref (contents line) position)))

(defmethod cluffer-internal:line-split-line ((line line) position)
  (ensure-closed-line line)
  (let* ((contents (contents line))
         (new-contents (subseq contents position))
         (last-line-p (last-line-p line)) ; are we inserting after the last line?
         (new-line (make-instance (class-of line) :cursors      '()
                                                  :contents     new-contents
                                                  :first-line-p nil
                                                  :last-line-p  last-line-p
                                                  :open-line-p  nil)))
    (setf (contents line) (subseq contents 0 position))
    (setf (cursors new-line)
          (loop for cursor in (cursors line)
                when (or (and (eq (cursor-stickiness cursor) :rsticky)
                              (>= (cluffer:cursor-position cursor) position))
                         (and (eq (cursor-stickiness cursor) :lsticky)
                              (> (cluffer:cursor-position cursor) position)))
                  collect cursor))
    (loop for cursor in (cursors new-line)
          do (setf (line cursor) new-line)
             (decf (cluffer:cursor-position cursor) position))
    (setf (cursors line)
          (set-difference (cursors line) (cursors new-line)))
    ;; If we inserted the new line after the former last line, that
    ;; line is no longer the last line.
    (when last-line-p
      (setf (last-line-p line) nil))
    new-line))

(defmethod cluffer-internal:line-join-line ((line1 line) (line2 line))
  (ensure-closed-line line1)
  (ensure-closed-line line2)
  (loop with length = (length (contents line1))
          initially
             (setf (contents line1)
                   (concatenate 'vector (contents line1) (contents line2)))
        for cursor in (cursors line2)
        do (setf (line cursor) line1)
           (incf (cluffer:cursor-position cursor) length)
           (push cursor (cursors line1)))
  ;; If we are joining the former next-to-last and last lines, the
  ;; "surviving" line, LINE1, is now the last line.
  (when (last-line-p line2)
    (setf (last-line-p line1) t))
  nil)
