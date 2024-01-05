;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2002 by Alexey Dejneka <adejneka@comail.ru>
;;;  (c) Copyright 2023 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Page layout abstraction
;;;
;;; Page layout may have numerous properties which arrange things on a stream
;;; with having a broader picture in mind. Text on a page may have alignment and
;;; direction, margins, columns, paragraph settings and much more. This file is
;;; a beacon of the abstraction which may be used to specify these things.
;;;

(in-package #:clim-internals)

(defvar +default-margins+
  `(:left   (:absolute 0)
    :top    (:absolute 0)
    :right  (:relative 0)
    :bottom (:relative 0)))

(defclass standard-page-layout (edward-sheet-mixin)
  (;; Margins are used to compute the page dimensions.
   (margins :initarg :margins :reader stream-text-margins :type margin-spec)
   (last-page-region :initform +nowhere+ :accessor last-page-region))
  (:default-initargs :margins +default-margins+))

(defmethod shared-initialize :after ((instance standard-page-layout)
                                     slot-names &key text-margins text-margin)
  (declare (ignore slot-names))
  (let* ((right-margin (and text-margin `(:right (:absolute ,text-margin))))
         (defaults (normalize-margin-spec right-margin +default-margins+)))
    (setf (slot-value instance 'margins)
          (normalize-margin-spec text-margins defaults))))

(defmethod (setf stream-text-margins) (margins (self standard-page-layout))
  (let* ((old (stream-text-margins self))
         (new (normalize-margin-spec margins old)))
    (unless (equal old new)
      (setf (slot-value self 'margins) new)
      (recompute-page-region self))))

(defmethod stream-page-region :before ((sheet standard-page-layout))
  (unless (region-equal (last-page-region sheet) (window-viewport sheet))
    (setf (last-page-region sheet) (window-viewport sheet))
    (recompute-page-region sheet)))

(defun recompute-page-region (stream)
  (flet ((fallback-dimensions ()
           (let* ((text-style (stream-text-style stream))
                  (chw (text-style-width  text-style stream))
                  (chh (text-style-height text-style stream)))
             (make-rectangle* 0 0 (* 80 chw) (* 43 chh)))))
    (let ((region (window-viewport stream))
          (cached (stream-page-region stream)))
      (with-bounding-rectangle* (x1 y1 x2 y2)
          (if (region-equal region +everywhere+)
              (fallback-dimensions)
              region)
        (macrolet ((thunk (margin edge sign orientation)
                     `(if (eql (first ,margin) :absolute)
                          (parse-space stream (second ,margin) ,orientation)
                          (,sign ,edge (parse-space stream (second ,margin) ,orientation)))))
          (destructuring-bind (&key left top right bottom) (stream-text-margins stream)
            (setf (rectangle-edges* cached)
                  (values (thunk left   x1 + :horizontal)
                          (thunk top    y1 + :vertical)
                          (thunk right  x2 - :horizontal)
                          (thunk bottom y2 - :vertical)))
            cached))))))

(defgeneric invoke-with-temporary-page (stream continuation &key margins move-cursor)
  (:method ((stream standard-page-layout) continuation &key margins (move-cursor t))
    (flet ((do-it ()
             (letf (((stream-text-margins stream) margins))
               (funcall continuation stream))))
      (if move-cursor
          (do-it)
          (multiple-value-bind (cx cy) (stream-cursor-position stream)
            (unwind-protect (do-it)
              (setf (stream-cursor-position stream) (values cx cy))))))))

(defmacro with-temporary-margins
    ((stream &rest args
             &key (move-cursor t) (left nil lp) (right nil rp) (top nil tp) (bottom nil bp))
     &body body)
  (declare (ignore move-cursor))
  (with-stream-designator (stream '*standard-output*)
    (with-keywords-removed (args (:left :right :top :bottom))
      (with-gensyms (stream-arg continuation margins)
        `(flet ((,continuation (,stream-arg)
                  (declare (ignore ,stream-arg))
                  ,@body))
           (declare (dynamic-extent #',continuation))
           (let (,margins)
             ,@(collect (margin)
                 (when lp (margin `(setf (getf ,margins :left) ,left)))
                 (when rp (margin `(setf (getf ,margins :right) ,right)))
                 (when tp (margin `(setf (getf ,margins :top) ,top)))
                 (when bp (margin `(setf (getf ,margins :bottom) ,bottom)))
                 (margin))
             (invoke-with-temporary-page ,stream #',continuation :margins ,margins ,@args)))))))

(defgeneric invoke-with-pristine-viewport (sheet cont)
  (:method ((sheet standard-page-layout) cont)
    (let ((cursor (stream-text-cursor sheet)))
      (letf (((stream-text-margins sheet) '(:left   (:absolute 0)
                                            :top    (:absolute 0)
                                            :right  (:relative 0)
                                            :bottom (:relative 0)))
             ((cursor-position cursor) (stream-cursor-initial-position sheet))
             ((cursor-baseline cursor) (values 0 0))
             ((cursor-size cursor) (values 0 0)))
        (funcall cont sheet)))))

(defmacro with-pristine-viewport ((stream) &body body)
  (let ((cont (gensym)))
    `(flet ((,cont (,stream)
              (declare (ignorable ,stream))
              ,@body))
       (declare (dynamic-extent (function ,cont)))
       (invoke-with-pristine-viewport ,stream (function ,cont)))))


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
