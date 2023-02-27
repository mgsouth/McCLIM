;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020-2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; An implementation of internal buffers. Includes a kill buffer.

(in-package #:clim-internals)

(defun make-cluffer ()
  (let ((line (make-instance 'cluffer-standard-line:open-line)))
    (make-instance 'cluffer-standard-buffer:buffer :initial-line line)))

;;; Kill buffer
(defun make-kluffer ()
  (let* ((line (make-instance 'cluffer-standard-line:open-line))
         (cursor (make-instance 'cluffer-standard-line:right-sticky-cursor))
         (buffer (make-instance 'cluffer-standard-buffer:buffer :initial-line line)))
    (cluffer:attach-cursor cursor line)
    (values buffer cursor)))

(defun print-kluffer (cursor offset)
  (format *debug-io* "---------------------~%")
  (map-over-lines (lambda (p)
                    (cond ((and (zerop offset)
                                (cursor= p cursor))
                           (format *debug-io* "= kill: ~a~%" (line-string p)))
                          ((cursor= p cursor)
                           (format *debug-io* "< kill: ~a~%" (line-string p)))
                          ((= (mod (+ (cluffer:line-number cursor) offset)
                                   (cluffer:line-count (cluffer:buffer cursor)))
                              (cluffer:line-number p))
                           (format *debug-io* "> kill: ~a~%" (line-string p)))
                          ((format *debug-io* "  kill: ~a~%" (line-string p)))))
                  (cluffer:buffer cursor)))

;;; Selections (known as regions) may represent a pointer selection, last
;;; yank, an annotation etc. Selections may overlap and span multiple lines.

(defclass buffer-selection ()
  ((lcursor :initarg :lcursor :reader lcursor)
   (rcursor :initarg :rcursor :reader rcursor)
   (%anchor :initarg :%anchor :accessor %anchor)))

(defun make-buffer-selection (anchor)
  (let ((c1 (make-instance 'edward-lsticky-cursor))
        (c2 (make-instance 'edward-rsticky-cursor)))
    (smooth-set-position c1 anchor)
    (smooth-set-position c2 anchor)
    (make-instance 'buffer-selection :%anchor c1 :lcursor c1 :rcursor c2)))

(defun move-buffer-selection (selection position &optional extension)
  (smooth-set-position (lcursor selection) position)
  (smooth-set-position (rcursor selection) position)
  (when extension
    (extend-buffer-selection selection extension)))

(defun extend-buffer-selection (selection position)
  (let ((anchor (%anchor selection))
        (lcursor (lcursor selection))
        (rcursor (rcursor selection)))
    (if (cursor<= position anchor)
        (progn
          (smooth-set-position rcursor anchor)
          (smooth-set-position lcursor position)
          (setf (%anchor selection) rcursor))
        (progn
          (smooth-set-position lcursor anchor)
          (smooth-set-position rcursor position)
          (setf (%anchor selection) lcursor)))))

(defun kill-buffer-selection (selection)
  (let ((lcursor (lcursor selection))
        (rcursor (rcursor selection)))
    (when (cluffer:cursor-attached-p lcursor)
      (cluffer:detach-cursor lcursor))
    (when (cluffer:cursor-attached-p rcursor)
      (cluffer:detach-cursor rcursor))))

;;; Cluffer "smooth" utilities - CLIM spec operates on positions treating the
;;; input buffer as a vector. On the other hand Cluffer keeps each line as a
;;; separate entity, so we need to make things transparent and allow smooth
;;; transitioning between lines and addressing positions with an integer.

(defun cursor-previous-line (cursor)
  (let ((buf (cluffer:buffer cursor))
        (pos (1- (cluffer:line-number cursor))))
    (cluffer:find-line buf pos)))

(defun cursor-next-line (cursor)
  (let ((buf (cluffer:buffer cursor))
        (pos (1+ (cluffer:line-number cursor))))
    (cluffer:find-line buf pos)))

(defun cursor-linear-position (cursor)
  (loop with buffer = (cluffer:buffer cursor)
        with newlines = (cluffer:line-number cursor)
        with position = (+ newlines (cluffer:cursor-position cursor))
        for linum from 0 below newlines
        for line = (cluffer:find-line buffer linum)
        for count = (cluffer:item-count line)
        do (incf position count)
        finally (return (values position linum count))))

(defun (setf cursor-linear-position) (new-position cursor)
  (loop with buffer = (cluffer:buffer cursor)
        with position = 0
        for linum from 0 below (cluffer:line-count buffer)
        for line = (cluffer:find-line buffer linum)
        when (<= new-position (+ position (cluffer:item-count line))) do
          (cluffer:detach-cursor cursor)
          (cluffer:attach-cursor cursor line (- new-position position))
          (return-from cursor-linear-position
            (cluffer:cursor-position cursor))
        do (incf position (1+ (cluffer:item-count line)))
        finally (error "~s points beyond the buffer!" new-position)))

;;; "Smooth" operations glide over line boundaries as if we had a linear buffer.

(defun smooth-peek-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         nil)
        ((cluffer:end-of-line-p cursor)
         #\newline)
        (t
         (cluffer:item-after-cursor cursor))))

(defun smooth-forward-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         (beep))
        ((cluffer:end-of-line-p cursor)
         (let ((next (cursor-next-line cursor)))
           (cluffer:detach-cursor cursor)
           (cluffer:attach-cursor cursor next)
           #\newline))
        (t
         (cluffer:forward-item cursor)
         (cluffer:item-before-cursor cursor))))

(defun smooth-backward-item (cursor)
  (cond ((cluffer:beginning-of-buffer-p cursor)
         (beep))
        ((cluffer:beginning-of-line-p cursor)
         (let ((prev (cursor-previous-line cursor)))
           (cluffer:detach-cursor cursor)
           (cluffer:attach-cursor cursor prev)
           (cluffer:end-of-line cursor)
           #\newline))
        (t
         (cluffer:backward-item cursor)
         (cluffer:item-after-cursor cursor))))

(defun smooth-insert-item (cursor item)
  (if (char= item #\newline)
      (cluffer:split-line cursor)
      (cluffer:insert-item cursor item)))

(defun smooth-delete-item (cursor)
  (cond ((cluffer:end-of-buffer-p cursor)
         (beep)
         nil)
        ((cluffer:end-of-line-p cursor)
         (cluffer:join-line cursor)
         #\newline)
        (t
         (let ((item (cluffer:item-after-cursor cursor)))
           (cluffer:delete-item cursor)
           item))))

(defun smooth-erase-item (cursor)
  (cond ((cluffer:beginning-of-buffer-p cursor)
         (beep)
         nil)
        ((cluffer:beginning-of-line-p cursor)
         (cluffer:join-line (cursor-previous-line cursor))
         #\newline)
        (t
         (let ((item (cluffer:item-before-cursor cursor)))
           (cluffer:erase-item cursor)
           item))))

(defun smooth-set-position (cursor destination &optional position)
  (etypecase destination
    (cluffer:line
     (if (cluffer:cursor-attached-p cursor)
         (unless (eq destination (cluffer:line cursor))
           (cluffer:detach-cursor cursor)
           (cluffer:attach-cursor cursor destination position))
         (cluffer:attach-cursor cursor destination position)))
    (cluffer:cursor
     (smooth-set-position cursor
                          (cluffer:line destination)
                          (cluffer:cursor-position destination)))
    (integer
     (setf (cursor-linear-position cursor) destination)))
  (when position
    (setf (cluffer:cursor-position cursor) position)))

(defun smooth-clean-buffer (buffer cursor)
  (smooth-beg-of-buffer buffer cursor)
  (handler-case (loop (smooth-delete-line cursor))
    (cluffer:end-of-buffer ()))
  (assert (= 1 (cluffer:line-count buffer)))
  (assert (= 0 (cluffer:item-count (cluffer:line cursor)))))

(defun smooth-beg-of-buffer (buffer cursor)
  (symbol-macrolet ((line0 (cluffer:find-line buffer 0)))
    (unless (zerop (cluffer:line-number cursor))
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor line0)))
  (cluffer:beginning-of-line cursor))

(defun smooth-end-of-buffer (buffer cursor)
  (let ((cline (cluffer:line cursor))
        (bline (cluffer:find-line buffer (1- (cluffer:line-count buffer)))))
    (unless (eq cline bline)
      (cluffer:detach-cursor cursor)
      (cluffer:attach-cursor cursor bline)))
  (cluffer:end-of-line cursor))

(defun smooth-move-line (buffer cursor offset)
  (let* ((lnum (+ (cluffer:line-number cursor) offset))
         (cpos (cluffer:cursor-position cursor))
         (lmax (1- (cluffer:line-count buffer)))
         (next (cluffer:find-line buffer (clamp lnum 0 lmax))))
    (cluffer:detach-cursor cursor)
    (handler-case (cluffer:attach-cursor cursor next cpos)
      (cluffer:end-of-line ()
        (cluffer:end-of-line cursor)))))

(defun smooth-warp-line (buffer cursor offset)
  (let* ((lnum (+ (cluffer:line-number cursor) offset))
         (cpos (cluffer:cursor-position cursor))
         (lcnt (cluffer:line-count buffer))
         (next (cluffer:find-line buffer (mod lnum lcnt))))
    (cluffer:detach-cursor cursor)
    (handler-case (cluffer:attach-cursor cursor next cpos)
      (cluffer:end-of-line ()
        (cluffer:end-of-line cursor)))))

(defun smooth-jump-line (buffer cursor lnum)
  (let* ((lmax (1- (cluffer:line-count buffer)))
         (next (cluffer:find-line buffer (clamp lnum 0 lmax))))
    (cluffer:detach-cursor cursor)
    (cluffer:attach-cursor cursor next 0)))

(defun smooth-delete-line (cursor)
  (if (cluffer:end-of-line-p cursor)
      (cluffer:join-line cursor)
      (handler-case (loop (cluffer:delete-item cursor))
        (cluffer:end-of-line ()))))

(defun smooth-kill-line (cursor)
  (prog1 (if (cluffer:end-of-line-p cursor)
             (list #\newline)
             (copy-seq (cluffer:items cursor :start (cluffer:cursor-position cursor))))
    (smooth-delete-line cursor)))

(defun smooth-insert-input (cursor input)
  ;; This "insert" splits the line on a newline character.
  (map nil (curry #'smooth-insert-item cursor) input))

(defun smooth-insert-items (cursor items)
  ;; This "insert" preserves a newline character. Render goes crazy here :-).
  (map nil (curry #'cluffer:insert-item cursor) items))

(defun smooth-insert-line (cursor items)
  ;; This "insert" ignores a newline character.
  (do-sequence (item items)
    (unless (eql item #\newline)
      (cluffer:insert-item cursor item))))

(defun smooth-replace-input (selection items)
  (loop with lcursor = (lcursor selection)
        with rcursor = (rcursor selection)
          initially (assert (cursor<= lcursor rcursor))
        while (cursor< lcursor rcursor)
        do (smooth-delete-item lcursor)
        finally (smooth-insert-input rcursor items)))

(defun smooth-replace-line (selection items)
  (loop with lcursor = (lcursor selection)
        with rcursor = (rcursor selection)
          initially (assert (cursor<= lcursor rcursor))
        while (cursor< lcursor rcursor)
        do (smooth-delete-item lcursor)
        finally (smooth-insert-line rcursor items)))

(defun smooth-kill-object (cursor object merge)
  ;; The cursor is always located at the end of the line.
  (ecase merge
    ((nil)
     (cluffer:end-of-line cursor)
     (unless (cluffer:beginning-of-buffer-p cursor)
       (cluffer:split-line cursor)))
    (:front
     (cluffer:beginning-of-line cursor))
    (:back
     (cluffer:end-of-line cursor)))
  (smooth-insert-items cursor object)
  #+ (or) (print-kluffer cursor 0))

(defun smooth-yank-kill (cursor &optional (offset 0))
  (unless (zerop offset)
    (smooth-warp-line (cluffer:buffer cursor) cursor offset)
    (cluffer:end-of-line cursor))
  (cluffer:items cursor))

(defun smooth-yank-next (cursor)
  (smooth-yank-kill cursor -1))

(defun smooth-yank-prev (cursor)
  (smooth-yank-kill cursor +1))

;;; This DWIM operator compares line and cursor positions. When a cursor is
;;; compared with a line then 0 means "attached to a line". [-1 0 +1]
(defun cursor-compare (c1 c2)
  (let ((l1 (cluffer:line-number c1))
        (l2 (cluffer:line-number c2)))
    (cond ((< l1 l2) -1)
          ((> l1 l2) +1)
          ((or (typep c1 'cluffer:line)
               (typep c2 'cluffer:line))
           0)
          ((let ((p1 (cluffer:cursor-position c1))
                 (p2 (cluffer:cursor-position c2)))
             (cond ((< p1 p2) -1)
                   ((> p1 p2) +1)
                   (t          0)))))))

(macrolet ((defcmp (name cmp val)
             `(defun ,name (c1 c2)
                (,cmp (cursor-compare c1 c2) ,val))))
  (defcmp cursor< = -1)
  (defcmp cursor> = +1)
  (defcmp cursor= =  0)
  (defcmp cursor<= /= +1)
  (defcmp cursor>= /= -1))

;;; Operations on cluffer's buffer and line instances.

(defun map-over-lines (function buffer)
  (loop with length = (cluffer:line-count buffer)
        for lineno from 0 below length
        for line = (cluffer:find-line buffer lineno)
        do (funcall function line)))

(defun line-string (line)
  (with-output-to-string (str)
    (loop for item across (cluffer:items line)
          if (characterp item)
            do (princ item str)
          else
            do (format str "@~a" item))))

;;; FIXME while this does not cons excessively (we accept an adjustable string
;;; with a fill pointer as an argument), the operation time in the case of a
;;; dirty buffer is linear to the size of the input. The CLUFFER:UPDATE protocol
;;; is much faster but we need to come up with a scheme to remember line numbers
;;; in the string to be able to synchronize. This is good enough for now.
(defun buffer-string (buffer &optional string)
  (with-output-to-string (stream string)
    (flet ((add-line (line)
             (princ (line-string line) stream)
             (unless (cluffer:last-line-p line)
               (terpri stream))))
      (declare (dynamic-extent (function add-line)))
      (map-over-lines #'add-line buffer))))

(defun buffer-timestamp (buffer)
  (cluffer-standard-buffer::current-time buffer))
