;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2023 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; The implementation of displaying and drawing of the "text" buffer. Here we
;;; implement things like the text direction, margins and line wrapping. This is
;;; a common abstraction used by SEOS and Gadgets. We take a loose definition of
;;; the word "text" to mean "a sequence of things" (not necessarily a string).

(in-package #:climi)

(deftype h-direction () `(member :left-to-right :right-to-left))
(deftype v-direction () `(member :top-to-bottom :bottom-to-top))
(deftype *-direction () `(or h-direction v-direction))

(defclass edward-sheet-mixin ()
  ((text-cursor :accessor stream-text-cursor)
   (text-buffer :accessor stream-text-buffer)
   ;;
   (page-region :initarg :page-region :accessor stream-page-region)
   (line-direction :initarg :line-direction :accessor stream-line-direction)
   (page-direction :initarg :page-direction :accessor stream-page-direction)
   (end-of-line-action :initarg :end-of-line-action :accessor stream-end-of-line-action)
   (end-of-page-action :initarg :end-of-page-action :accessor stream-end-of-page-action))
  (:default-initargs :page-region (make-bounding-rectangle 0 0 100 100)
                     :line-direction :left-to-right
                     :page-direction :top-to-bottom
                     :end-of-line-action :wrap*
                     :end-of-page-action :scroll))

(defmethod initialize-instance :after ((self edward-sheet-mixin) &rest args)
  (declare (ignore args))
  (let ((buffer nil) ;; not yet
        (cursor (make-instance 'standard-text-cursor :sheet self)))
    (setf (stream-text-buffer self) buffer)
    (setf (stream-text-cursor self) cursor)))

(defun stream-cursor-initial-position (sheet)
  (with-bounding-rectangle* (x1 y1 x2 y2) (stream-page-region sheet)
    (let ((x0 x1) (y0 y1))
      (ecase (stream-line-direction sheet)
        (:left-to-right (setf x0 x1))
        (:right-to-left (setf x0 x2))
        (:top-to-bottom (setf y0 y1))
        (:bottom-to-top (setf y0 y2)))
      (ecase (stream-page-direction sheet)
        (:top-to-bottom (setf y0 y1))
        (:bottom-to-top (setf y0 y2))
        (:left-to-right (setf x0 x1))
        (:right-to-left (setf x0 x2)))
      (values x0 y0))))

(defun stream-cursor-final-position (sheet)
  (with-bounding-rectangle* (x1 y1 x2 y2) (stream-page-region sheet)
    (let ((xn x2) (yn y2))
      (ecase (stream-line-direction sheet)
        (:left-to-right (setf xn x2))
        (:right-to-left (setf xn x1))
        (:top-to-bottom (setf yn y2))
        (:bottom-to-top (setf yn y1)))
      (ecase (stream-page-direction sheet)
        (:top-to-bottom (setf yn y2))
        (:bottom-to-top (setf yn y1))
        (:left-to-right (setf xn x2))
        (:right-to-left (setf xn x1)))
      (values xn yn))))

(defun stream-line-margin (sheet)
  (ecase (stream-line-direction sheet)
    ((:left-to-right :right-to-left)
     (bounding-rectangle-width (stream-page-region sheet)))
    ((:top-to-bottom :bottom-to-top)
     (bounding-rectangle-height (stream-page-region sheet)))))

(defun stream-text-offset (sheet cursor)
  (multiple-value-bind (cx cy) (cursor-position cursor)
    (with-bounding-rectangle* (x1 y1 x2 y2) (stream-page-region sheet)
      (ecase (stream-line-direction sheet)
        (:left-to-right (- cx x1))
        (:right-to-left (- x2 cx))
        (:top-to-bottom (- cy y1))
        (:bottom-to-top (- y2 cy))))))

;;; FIXME we probably should plug UAX-14 here.
(defun stream-text-break (sheet cursor text start end)
  (orf start 0)
  (orf end (length text))
  (when (>= start end)
    (return-from stream-text-break nil))
  (let ((margin (stream-line-margin sheet))
        (offset (stream-text-offset sheet cursor))
        (direction (stream-line-direction sheet))
        (end-of-line (stream-end-of-line-action sheet)))
    (labels ((size (text start end)
               (multiple-value-bind (sum-w sum-h)
                   (text-metrics sheet text :start start :end end)
                 (ecase direction
                   ((:left-to-right :right-to-left) sum-w)
                   ((:top-to-bottom :bottom-to-top) sum-h))))
             (break-opportunity-p (position)
               (ecase end-of-line
                 (:wrap  t)
                 (:wrap* (let ((elt (aref text (1- position))))
                           (if (not (characterp elt))
                               t
                               (char= elt #\space))))))
             (break-before-position-p (position)
               (> (+ offset (size text start position)) margin))
             (compute-next-break ()
               (loop with emergencyop = (1+ start)
                     with opportunity = t
                     for position from (1+ start) upto end do
                       (cond ((break-before-position-p position)
                              (return-from compute-next-break
                                (values opportunity emergencyop)))
                             ((break-opportunity-p position)
                              (setf opportunity position))
                             (t
                              (setf emergencyop position)))
                     finally (return nil))))
      (multiple-value-bind (position emergency) (compute-next-break)
        (if (eq position t) ;; Emergency break.
            (if (zerop offset) emergency start)
            position)))))

;;; To measure the text line metrics CLIM II defines the function TEXT-SIZE
;;; which returns values:
;;;
;;; - WIDTH, HEIGHT - total dimensions of the record. Not to be confused with
;;; the bounding rectangle, "a" and "A" have the same height under this metric
;;; - CURSOR-DX, CURSOR-DY - the final position of the cursor; CURSOR-DY is
;;; specified to be 0 unless there is a line break
;;; - HORIZONTAL-BASELINE - the horizontal baseline of the string
;;;
;;; That specification has a few problems. Most notably it assumes that the line
;;; and the page directions are :LEFT-TO-RIGHT and :TOP-TO-BOTTOM. HORIZONTAL
;;; baseline helps to understand where is the origin in horizontal lines, but
;;; there's no such indicator for vertical lines. We change this specification
;;; to address the multidirectional page needs in a mostly compatible manner:
;;;
;;; - WIDTH, HEIGHT - total dimensions of the object (bearings included)
;;; - CURSOR-DX, CURSOR-DY - advance size in horizontal / vertical line
;;;
;;; The drawing origin is assumed to be at (0, 0) and CURSOR-{DX,DY} indicate
;;; the offset from the origin to the right-bottom corner of the line. These
;;; values may be used to compute the baseline when needed:
;;;
;;; - HBASELINE = (- HEIGHT CURSOR-DY)
;;; - VBASELINE = (- WIDTH  CURSOR-DX)
;;;
;;; Note that this definition is mostly compatible with TEXT-SIZE, but to avoid
;;; issues witht unprepared backends we introduce a separate function.
(defgeneric text-metrics (sheet object &key &allow-other-keys))

(defmethod text-metrics (sheet item &key)
  (declare (ignore sheet))
  ;; Here we may decide on padding in the future.
  (with-bounding-rectangle* (:x2 x2 :y2 y2 :width w :height h) item
    (values w h x2 y2)))

(defmethod text-metrics (sheet (record output-record) &key)
  (declare (ignore sheet))
  ;; Here we may decide on padding in the future.
  (with-bounding-rectangle* (:x2 x2 :y2 y2 :width w :height h) record
    (multiple-value-bind (y0 x0) (output-record-baseline record)
      (values w h (- x2 x0) (- y2 y0)))))

(defmethod text-metrics (sheet (record string) &rest args)
  (multiple-value-bind (width height cursor-dx cursor-dy hbase)
      (apply #'text-size sheet record args)
    (declare (ignore cursor-dy))
    (values width height cursor-dx (- height hbase))))

;;; The compound line size of multiple elements. This is tricky, because the
;;; embedded line may have a different direction from the parent. The page
;;; direction is irrelevant because we always return the same maximized values.
(defmethod text-metrics (sheet (object vector) &key start end line-direction)
  (orf start 0) (orf end (length object))
  (when (>= start end)
    (return-from text-metrics (values 0 0 0 0)))
  (orf line-direction :left-to-right)
  (let ((sum-w 0) (sum-h 0)
        (max-l 0) (max-t 0)
        (max-r 0) (max-b 0)
        (record-0 (aref object start))
        (record-1 (aref object (1- end)))
        r0-before r0-above
        r1-before r1-above)
    (flet ((add-record (record)
             (multiple-value-bind (width height after below)
                 (text-metrics sheet record)
               (let ((hbase (- height below))
                     (vbase (- width after)))
                 (incf sum-w width) (incf sum-h height)
                 (maxf max-l hbase) (maxf max-t vbase)
                 (maxf max-r after) (maxf max-b below)
                 (values hbase vbase)))))
      (multiple-value-setq (r0-before r0-above)
        (add-record record-0))
      (loop for idx from (1+ start) below (1- end)
            do (add-record (aref object idx)))
      (multiple-value-setq (r1-before r1-above)
        (add-record record-1)))
    (let ((ln-before max-l) (ln-above  max-t)
          (ln-after  max-r) (ln-below  max-b))
      (ecase line-direction
        (:left-to-right
         (setf ln-before r0-before)
         (setf ln-after (- sum-w ln-before)))
        (:right-to-left
         (setf ln-before r1-before)
         (setf ln-after (- sum-w ln-before)))
        (:top-to-bottom
         (setf ln-above r0-above)
         (setf ln-below (- sum-h ln-above)))
        (:bottom-to-top
         (setf ln-above r1-above)
         (setf ln-below (- sum-h ln-above))))
      (values sum-w sum-h ln-after ln-below))))


;;; This function computes the cursor change after inserting the element while
;;; taking into account margins and the line and the page directions.
;;;
;;; - RECORD-ORIGIN - the pen position for drawing the record
;;; - CURSOR-FINISH - the final cursor position after inserting the element
;;; - CURSOR-OFFSET - new values of the horizontal and the vertical baseline
;;; - CURSOR-SIZE - the new cursor size (width and height)
;;;
;;; Moreover this function returns two flags signaling whether the object
;;; exceeds page margins:
;;;
;;; - END-OF-LINE-P - the record will exceed the line end
;;; - END-OF-PAGE-P - the record will exceed the page end
;;;
(defun stream-cursor-motion (sheet cursor record &rest args)
  (nest
   (let ((line-direction (stream-line-direction sheet))
         (page-direction (stream-page-direction sheet))
         (page-region (stream-page-region sheet))))
   (with-bounding-rectangle* (px1 py1 px2 py2) page-region)
   (multiple-value-bind (cx cy) (cursor-position cursor))
   (multiple-value-bind (by bx) (cursor-baseline cursor))
   (multiple-value-bind (ax ay) (cursor-size cursor))
   (multiple-value-bind (w h r b) (apply #'text-metrics sheet record args))
   (let ((l (- w r))
         (a (- h b))
         (x0 cx) (y0 cy)
         (eol-p nil) (eop-p nil))
     ;; The line direction determines the cursor advancement.
     (ecase line-direction
       (:left-to-right (setf x0 (+ cx l)) (incf cx w) (setf eol-p (> cx px2)))
       (:right-to-left (setf x0 (- cx r)) (decf cx w) (setf eol-p (< cx px1)))
       (:top-to-bottom (setf y0 (+ cy a)) (incf cy h) (setf eol-p (> cy py2)))
       (:bottom-to-top (setf y0 (- cy b)) (decf cy h) (setf eol-p (< cy py1))))
     ;; The page direction determines the baseline alignment.
     (ecase page-direction
       (:top-to-bottom
        (maxf by a)             (setf y0 (+ cy by))
        (maxf ay (+ by b))      (setf eop-p (> (+ cy ay) py2)))
       (:bottom-to-top
        (minf by (- b))         (setf y0 (+ cy by))
        (maxf ay (+ (- by) a))  (setf eop-p (< (- cy ay) py1)))
       (:left-to-right
        (maxf bx l)             (setf x0 (+ cx bx))
        (maxf ax (+ bx r))      (setf eop-p (> (+ cx w) px2)))
       (:right-to-left
        (minf bx (- r))         (setf x0 (+ cx bx))
        (maxf ax (+ (- bx) l))  (setf eop-p (> (- cx w) px1))))
     (values x0 y0 cx cy bx by ax ay eol-p eop-p))))
