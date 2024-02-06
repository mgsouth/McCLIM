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
;;; Machinery for creating, querying and modifying output records.
;;;

;;; Output Record geometry:
;;;
;;; OUTPUT-RECORD is a subclass of BOUNDING-RECTANGLE and its coordinates are
;;; specified to be relative to the stream. OUTPUT-RECORD must handle initargs
;;; :X-POSITION and :Y-POSITION. The following additional accessors related to
;;; the record's geometry are specified:
;;;
;;;   OUTPUT-RECORD-POSITION: upper-left corner of the bounding rectangle
;;;   OUTPUT-RECORD-{START,END}-CURSOR-POSITION: "text" and "updating" records
;;;
;;; CLIM does not specify the concept of a baseline in output records, but other
;;; parts of the specification require it. McCLIM implementation details follow:
;;;
;;; Initargs :{X,Y}-POSITION specify the output record origin and may be inside
;;; and/or outside of the record's bounding rectangle. Both baselines are
;;; implicit from the record origin and its bounding rectangle.
;;;
;;;     OUTPUT-RECORD-ORIGIN: accessor that specifies the origin of the record
;;;     OUTPUT-RECORD-OFFSET: reader that specifies offsets for line drawing
;;;
;;; Unlike sheets, output records are specified to have positions relative to
;;; the stream, not to the parent. Or, if you like, the transformation of the
;;; output record to its parent is always +IDENTITY-TRANSFORMATION+. This is
;;; useful, because output records that are part of the stream's output history
;;; may be directly used as region designators without additional cost. On the
;;; other hand that makes the operation of moving the output record expensive,
;;; because it must be propagated to its children.
;;;
;;; The text protocol allows for a sequence of elements on a common baseline.
;;; The text-line output record arranges elements on a line and updates the
;;; cursor. The incremental redisplay takes that even further and treats the
;;; output record history as a buffer, where some elements maintain a spatial
;;; distance between each other.
;;;
;;; To do that we utilize functions OUTPUT-RECORD-{START,END}-CURSOR-POSITION.
;;; Instead of expensive operation of changing the output record position, the
;;; formatting output record replays its children with offsets that depend on
;;; the current cursor position.
;;;
;;; Most notably REPLAY-OUTPUT-RECORD and MAP-OVER-OUTPUT-RECORDS-* are called
;;; with arguments :{X,Y}-OFFSET to ensure the correct drawing placement. This
;;; also means that such records should not rely on bounding rectangles of its
;;; children when computing their own bounding rectangle.

;;; TODO records:
;;;
;;; - How should (SETF OUTPUT-RECORD-POSITION) affect the bounding rectangle of
;;;   the parent? Now its bounding rectangle is accurately recomputed, but it is
;;;   very inefficient for table formatting. It seems that CLIM is supposed to
;;;   keep a "large enough" rectangle and to shrink it to the correct size only
;;;   when the layout is complete by calling TREE-RECOMPUTE-EXTENT.
;;;
;;; - Computation of the bounding rectangle of lines/polygons ignores
;;;   LINE-STYLE-CAP-SHAPE.
;;;
;;; - Rounding of coordinates.

(in-package #:clim-internals)

;;; These generic functions need to be implemented for all the basic
;;; displayed-output-records, so they are defined in this file.
;;;
;;; MATCH-OUTPUT-RECORDS and FIND-CHILD-OUTPUT-RECORD, as defined in the CLIM
;;; spec, are pretty silly.  How does incremental redisplay know what keyword
;;; arguments to supply to FIND-CHILD-OUTPUT-RECORD?  Through a gf specialized
;;; on the type of the record it needs to match... why not define the search
;;; function and the predicate on two records then!
;;;
;;; These gf's use :MOST-SPECIFIC-LAST because one of the least specific methods
;;; will check the bounding boxes of the records, which should cause an early
;;; out most of the time.
;;;
;;; We'll implement MATCH-OUTPUT-RECORDS and FIND-CHILD-OUTPUT-RECORD, but we
;;; won't actually use them.  Instead, output-record-equal will match two
;;; records, and find-child-record-equal will search for the equivalent record.

(defgeneric match-output-records-1 (record &key)
  (:method-combination and :most-specific-last))

(defgeneric output-record-equal (record1 record2)
  (:method-combination and :most-specific-last))

;;; Fallback ethods so that something's always applicable.

(defmethod match-output-records ((record t) &rest args)
  (apply #'match-output-records-1 record args))

(defmethod output-record-equal and (record1 record2)
  (declare (ignore record1 record2))
  'maybe)

(defmethod output-record-equal :around (record1 record2)
  (cond ((eq record1 record2)
         ;; Some unusual record -- like a Goatee screen line -- might
         ;; exist in two trees at once
         t)
        ((eq (class-of record1) (class-of record2))
         (let ((result (call-next-method)))
           (if (eq result 'maybe)
               nil
               result)))
        (t nil)))

;;; The code for MATCH-OUTPUT-RECORDS-1 and OUTPUT-RECORD-EQUAL methods are very
;;; similar, hence this macro.  In order to exploit the similarities, it's
;;; necessary to treat the slots of the second record like variables, so for
;;; convenience the macro will use WITH-SLOTS on the second record.
(defmacro defrecord-predicate (record-type slots &body body)
  "Each element of SLOTS is either a symbol naming a slot
or (SLOT-NAME SLOT-P)."
  (multiple-value-bind (slot-names key-args key-arg-alist)
      (loop for slot-spec in slots
            for (name slot-p) = (if (atom slot-spec)
                                    (list slot-spec t)
                                    slot-spec)
            for suppliedp = (gensym (format nil "~A-~A" name '#:p))
            when slot-p collect name into slot-names
            collect `(,name nil ,suppliedp) into key-args
            collect (cons name suppliedp) into key-arg-alist
            finally (return (values slot-names key-args key-arg-alist)))
    `(progn
       (defmethod output-record-equal and ((record ,record-type)
                                           (record2 ,record-type))
         (macrolet ((if-supplied ((var &optional (type t)) &body supplied-body)
                      (declare (ignore type))
                      (when (find var ',slot-names :test #'eq)
                        `(progn ,@supplied-body))))
           (with-slots ,slot-names record2
             ,@body)))
       (defmethod match-output-records-1 and ((record ,record-type)
                                              &key ,@key-args)
         (macrolet ((if-supplied ((var &optional (type t)) &body supplied-body)
                      (let ((supplied-var (or (cdr (assoc var ',key-arg-alist))
                                              (error "Unknown argument ~S" var))))
                        `(or (null ,supplied-var)
                             ,(if (eq type t)
                                  `(progn ,@supplied-body)
                                  `(if (typep ,var ',type)
                                       (progn ,@supplied-body)
                                       (error 'type-error
                                              :datum ,var
                                              :expected-type ',type)))))))
           ,@body)))))

;;; Forward definition
(defclass stream-output-history-mixin ()
  ((stream :initarg :stream :reader output-history-stream)))

(defun find-output-record-sheet (record)
  "Walks up the parents of RECORD, searching for an output history from which
the associated sheet can be determined."
  (typecase record
    (stream-output-history-mixin
     (output-history-stream record))
    (basic-output-record
     (find-output-record-sheet (output-record-parent record)))))


;;; Text output records and updating output records maintain the cursor start
;;; and end positions. This is a mixin that implements this. -- jd 2024-01-09

(defclass updating-cursor-mixin ()
  ((cursor  :initform (make-instance 'standard-text-cursor) :reader start-cursor)
   (final-x :accessor output-record-end-position-x)
   (final-y :accessor output-record-end-position-y)))

(macrolet ((def-accessor (name jump)
             `(progn
                (defmethod ,name (stream)
                  (,jump (start-cursor stream)))
                (defmethod (setf ,name) (val stream)
                  (setf (,jump (start-cursor stream)) val)))))
  (def-accessor output-record-start-position-x cursor-position-x)
  (def-accessor output-record-start-position-y cursor-position-y))

(defmethod output-record-start-cursor-position ((record updating-cursor-mixin))
  (cursor-position (start-cursor record)))

(defmethod* (setf output-record-start-cursor-position) (x y (record updating-cursor-mixin))
  (setf (cursor-position (start-cursor record)) (values x y)))

(defmethod output-record-end-cursor-position ((self updating-cursor-mixin))
  (values (output-record-end-position-x self)
          (output-record-end-position-y self)))

(defmethod* (setf output-record-end-cursor-position) (x y (self updating-cursor-mixin))
  (setf (values (output-record-end-position-x self)
                (output-record-end-position-y self))
        (values x y)))

(defun move-output-record (self dx dy)
  ;(assert (not (output-record-fixed-position record)))
  (incf (output-record-start-position-x self) dx)
  (incf (output-record-start-position-y self) dy)
  (incf (output-record-end-position-x self) dx)
  (incf (output-record-end-position-y self) dy))

(defmethod* (setf output-record-position) :before (nx ny (self updating-cursor-mixin))
  (multiple-value-bind (ox oy) (output-record-position self)
    (move-output-record self (- nx ox) (- ny oy))))

(defmacro tracking-cursor ((stream record) &body body)
  `(progn
     (update-cursor (start-cursor ,record) (stream-text-cursor ,stream))
     (multiple-value-prog1 (progn ,@body)
       (setf (output-record-end-cursor-position ,record)
             (cursor-position (stream-text-cursor ,stream))))))

;;; XXX Add to values we test, obviously.
;;;
;;; Well, maybe not.  The goal is to support output records that have moved
;;; but that are otherwise clean. I.e., some previous part of the output has
;;; changed (lines added or deleted, for example). If the stream cursor
;;; position is different, I'm not sure now that the code for the updating
;;; output record needs to be rerun; I think we could use only the difference
;;; in cursor position to move the record. Any other graphics state change --
;;; like a different foreground color -- should probably be handled by the
;;; programmer forcing all new output.

(defun state-matches-stream-p (record stream)
  (or (output-record-fixed-position record)
      ;; Note: We don't match the y coordinate.
      (= (cursor-position (start-cursor record))
         (cursor-position (stream-text-cursor stream)))))


;;; 16.2 Output Records: BASIC-OUTPUT-RECORD

(defclass basic-output-record (standard-bounding-rectangle output-record)
  ((parent :initform nil :accessor output-record-parent)
   (x :initarg :x-position :initform 0.0d0)
   (y :initarg :y-position :initform 0.0d0))
  (:documentation "Implementation class for the Basic Output Record Protocol."))

(defmethod initialize-instance :after ((record basic-output-record)
                                       &key (x-position 0.0d0 x-position-p)
                                            (y-position 0.0d0 y-position-p)
                                            (parent nil))
  (when (or x-position-p y-position-p)
    (setf (rectangle-edges* record)
          (values x-position y-position x-position y-position)))
  (when parent
    (add-output-record record parent)))

(defun output-record-origin (record)
  (with-slots (x y) record
    (values x y)))

(defun set-output-record-origin (self nx ny)
  (with-slots (x y) self
    (and nx (setf x nx))
    (and ny (setf y ny))))

(defun set-output-record-origin* (self nx ny)
  (multiple-value-bind (ox oy) (output-record-origin self)
    (let ((dx (- nx ox))
          (dy (- ny oy)))
      (multiple-value-bind (x1 y1) (output-record-position self)
        (setf (output-record-position self) (values (+ x1 dx) (+ y1 dy)))))))

(defmacro with-output-record-offset ((dx dy ox oy nx ny self) &body body)
  `(multiple-value-bind (,ox ,oy) (output-record-position ,self)
     (let ((,dx (- ,nx ,ox))
           (,dy (- ,ny ,oy)))
       ,@body
       (values ,nx ,ny))))

(defmethod* (setf output-record-position) :before (nx ny (self basic-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-bind (x0 y0) (output-record-origin self)
      (set-output-record-origin self (+ x0 dx) (+ y0 dy)))))

;;; 16.2.1. The Basic Output Record Protocol

(defmethod output-record-position ((record basic-output-record))
  (bounding-rectangle-position record))

(defmethod* (setf output-record-position) (nx ny (self basic-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (with-standard-rectangle* (x1 y1 x2 y2) self
      (setf (rectangle-edges* self)
            (values (+ x1 dx) (+ y1 dy) (+ x2 dx) (+ y2 dy))))))

(defmethod* (setf output-record-position)
    :around (nx ny (record basic-output-record))
  (with-bounding-rectangle* (min-x min-y max-x max-y) record
    (call-next-method)
    (when-let ((parent (output-record-parent record)))
      (unless (and (typep parent 'compound-output-record)
                   (slot-value parent 'in-moving-p)) ; XXX
        (recompute-extent-for-changed-child parent record
                                            min-x min-y max-x max-y)))
    (values nx ny)))

(defmethod output-record-start-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-start-cursor-position)
    (x y (record basic-output-record))
  (values x y))

(defmethod output-record-end-cursor-position ((record basic-output-record))
  (values nil nil))

(defmethod* (setf output-record-end-cursor-position)
    (x y (record basic-output-record))
  (values x y))

(defmethod output-record-hit-detection-rectangle* ((record basic-output-record))
  (bounding-rectangle* record))

(defmethod output-record-refined-position-test ((record basic-output-record) x y)
  (declare (ignore x y))
  t)

(defmethod note-output-record-lost-sheet ((record basic-output-record) sheet)
  (declare (ignore record sheet))
  (values))

(defmethod note-output-record-got-sheet ((record basic-output-record) sheet)
  (declare (ignore record sheet))
  (values))

(defmethod output-record-children ((record basic-output-record))
  nil)

(defmethod add-output-record (child (record basic-output-record))
  (declare (ignore child))
  (error "Cannot add a child to ~S." record))

(defmethod delete-output-record (child (record basic-output-record)
                                 &optional (errorp t))
  (declare (ignore child))
  (when errorp (error "Cannot delete a child from ~S." record)))

(defmethod delete-output-record :before (child (record basic-output-record)
                                         &optional (errorp t))
  (declare (ignore errorp))
  (when-let ((sheet (find-output-record-sheet record)))
    (note-output-record-lost-sheet child sheet)))

(defmethod clear-output-record ((record basic-output-record))
  (error "Cannot clear ~S." record))

;;; Design sub-protocol (is this all?)

(defmethod make-design-from-output-record ((self basic-output-record))
  self)

(defmethod draw-design (medium (self basic-output-record) &rest drawing-options)
  (with-medium-options (medium drawing-options)
    (replay-output-record self medium)))


;;; 16.2 Output Records: COMPOUND-OUTPUT-RECORD

(defclass compound-output-record (basic-output-record)
  ((in-moving-p :initform nil :documentation "Is set while changing the position."))
  (:documentation "Implementation class for output records with children."))

(defmethod* (setf output-record-position)
    :before (nx ny (self compound-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (letf (((slot-value self 'in-moving-p) t))
      (map-over-output-records
       (lambda (child)
         (multiple-value-bind (x y) (output-record-position child)
           (setf (output-record-position child)
                 (values (+ x dx) (+ y dy)))))
       self))))

(defmethod replay-output-record ((record compound-output-record) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (unless region
    (setf region (sheet-visible-region stream)))
  (with-drawing-options (stream :clipping-region region)
    (map-over-output-records-overlapping-region
     #'replay-output-record record region x-offset y-offset
     stream region x-offset y-offset)))

(defmethod note-output-record-lost-sheet :after ((record compound-output-record) sheet)
  (map-over-output-records #'note-output-record-lost-sheet record 0 0 sheet))

(defmethod note-output-record-got-sheet :after ((record compound-output-record) sheet)
  (map-over-output-records #'note-output-record-got-sheet record 0 0 sheet))

(defmethod add-output-record :before (child (record compound-output-record))
  (let ((parent (output-record-parent child)))
    (cond (parent
           (restart-case
               (error "~S already has a parent ~S." child parent)
             (delete ()
               :report "Delete from the old parent."
               (delete-output-record child parent))))
          ((eq record child)
           (error "~S is being added to itself." record))
          ((eq (output-record-parent record) child)
           (error "child ~S is being added to its own child ~S."
                  child record)))))

(defmethod add-output-record :after (child (record compound-output-record))
  (recompute-extent-for-new-child record child)
  (when (eq record (output-record-parent child))
    (when-let ((sheet (find-output-record-sheet record)))
      (note-output-record-got-sheet child sheet))))

(defmethod delete-output-record :after (child (record compound-output-record)
                                        &optional (errorp t))
  (declare (ignore errorp))
  (with-bounding-rectangle* (x1 y1 x2 y2) child
    (recompute-extent-for-changed-child record child x1 y1 x2 y2)))

(defmethod clear-output-record :before ((record compound-output-record))
  (when-let ((sheet (find-output-record-sheet record)))
    (map-over-output-records #'note-output-record-lost-sheet record 0 0 sheet)))

(defmethod clear-output-record :around ((record compound-output-record))
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* record)
    (call-next-method)
    (when-let ((parent (output-record-parent record)))
      (recompute-extent-for-changed-child parent record x1 y1 x2 y2))))

(defmethod clear-output-record :after ((record compound-output-record))
  (with-slots (x y) record
    (setf (rectangle-edges* record) (values x y x y))))

(defmethod recompute-extent-for-new-child
    ((record compound-output-record) child)
  (unless (null-bounding-rectangle-p child)
    (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
      (cond
        ((null-bounding-rectangle-p record)
         (setf (rectangle-edges* record) (bounding-rectangle* child)))
        ((not (null-bounding-rectangle-p child))
         (assert (not (null-bounding-rectangle-p record))) ; important.
         (with-bounding-rectangle* (x1-child y1-child x2-child y2-child)
             child
           (setf (rectangle-edges* record)
                 (values (min old-x1 x1-child) (min old-y1 y1-child)
                         (max old-x2 x2-child) (max old-y2 y2-child))))))
      (when-let ((parent (output-record-parent record)))
        (recompute-extent-for-changed-child
         parent record old-x1 old-y1 old-x2 old-y2))))
  record)

(defun %tree-recompute-extent* (record)
  (check-type record compound-output-record)
  ;; Internal helper function
  (if (zerop (output-record-count record)) ; no children
      (with-slots (x y) record
        (values x y x y))
      (let ((new-x1 0)
            (new-y1 0)
            (new-x2 0)
            (new-y2 0)
            (first-time t))
        (flet ((do-child (child)
                 (cond ((null-bounding-rectangle-p child))
                       (first-time
                        (multiple-value-setq (new-x1 new-y1 new-x2 new-y2)
                          (bounding-rectangle* child))
                        (setq first-time nil))
                       (t
                        (with-bounding-rectangle* (cx1 cy1 cx2 cy2) child
                          (minf new-x1 cx1)
                          (minf new-y1 cy1)
                          (maxf new-x2 cx2)
                          (maxf new-y2 cy2))))))
          (declare (dynamic-extent #'do-child))
          (map-over-output-records #'do-child record))
        (values new-x1 new-y1 new-x2 new-y2))))

(defmethod recompute-extent-for-changed-child
    ((record compound-output-record) changed-child
     old-min-x old-min-y old-max-x old-max-y)
  (with-bounding-rectangle* (ox1 oy1 ox2 oy2) record
    (with-bounding-rectangle* (cx1 cy1 cx2 cy2) changed-child
      ;; If record is currently empty, use the child's bbox directly. Else..
      ;; Does the new rectangle of the child contain the original rectangle?  If
      ;; so, we can use min/max to grow record's current rectangle.  If not, the
      ;; child has shrunk, and we need to fully recompute.
      (multiple-value-bind (nx1 ny1 nx2 ny2)
          (cond
            ;; The child has been deleted; who knows what the new bounding box
            ;; might be. This case shouldn't be really necessary.
            ((not (output-record-parent changed-child))
             (%tree-recompute-extent* record))
            ;; 1) Only one child of record, and we already have the bounds.
            ;; 2) Our record occupied no space so the new child is the rectangle.
            ((or (eql (output-record-count record) 1)
                 (null-bounding-rectangle-p record))
             (values cx1 cy1 cx2 cy2))
            ;; In the following cases, we can grow the new bounding rectangle
            ;; from its previous state:
            ((or
              ;; If the child was originally empty, it could not have affected
              ;; previous computation of our bounding rectangle.  This is
              ;; hackish for reasons similar to the above.
              (and (= old-min-x old-max-x) (= old-min-y old-max-y))
              ;; For each edge of the original child bounds, if it was within
              ;; its respective edge of the old parent bounding rectangle, or if
              ;; it has not changed:
              (and (or (> old-min-x ox1) (= old-min-x cx1))
                   (or (> old-min-y oy1) (= old-min-y cy1))
                   (or (< old-max-x ox2) (= old-max-x cx2))
                   (or (< old-max-y oy2) (= old-max-y cy2)))
              ;; New child bounds contain old child bounds, so use min/max to
              ;; extend the already-calculated rectangle.
              (and (<= cx1 old-min-x) (<= cy1 old-min-y)
                   (>= cx2 old-max-x) (>= cy2 old-max-y)))
             (values (min cx1 ox1) (min cy1 oy1)
                     (max cx2 ox2) (max cy2 oy2)))
            ;; No shortcuts - we must compute a new bounding box from those of
            ;; all our children. We want to avoid this - in worst cases, such as
            ;; a toplevel output history, there may exist thousands of children.
            ;; Without the above optimizations, construction becomes O(N^2) due
            ;; to the bounding rectangle calculation.
            (t
             (%tree-recompute-extent* record)))
        (with-slots (x y) record
          (setf x nx1 y ny1)
          (setf (rectangle-edges* record) (values nx1 ny1 nx2 ny2))
          (when-let ((parent (output-record-parent record)))
            (unless (and (= nx1 ox1) (= ny1 oy1)
                         (= nx2 ox2) (= ny2 oy2))
              (recompute-extent-for-changed-child parent record
                                                  ox1 oy1 ox2 oy2)))))))
  record)

(defun tree-recompute-extent-aux (record &aux new-x1 new-y1 new-x2 new-y2 changedp)
  (when (or (null (typep record 'compound-output-record))
            (zerop (output-record-count record)))
    (return-from tree-recompute-extent-aux
      (bounding-rectangle* record)))
  (flet ((do-child (child)
           (if (null changedp)
               (progn
                 (multiple-value-setq (new-x1 new-y1 new-x2 new-y2)
                   (tree-recompute-extent-aux child))
                 (setq changedp t))
               (multiple-value-bind (cx1 cy1 cx2 cy2)
                   (tree-recompute-extent-aux child)
                 (minf new-x1 cx1) (minf new-y1 cy1)
                 (maxf new-x2 cx2) (maxf new-y2 cy2)))))
    (declare (dynamic-extent #'do-child))
    (map-over-output-records #'do-child record))
  (with-slots (x y) record
    (setf x new-x1 y new-y1)
    (setf (rectangle-edges* record)
          (values new-x1 new-y1 new-x2 new-y2))))

(defmethod tree-recompute-extent ((record compound-output-record))
  (tree-recompute-extent-aux record)
  record)

(defmethod tree-recompute-extent :around ((record compound-output-record))
  (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) record
    (call-next-method)
    (with-bounding-rectangle* (x1 y1 x2 y2) record
      (when-let ((parent (output-record-parent record)))
        (unless (and (= old-x1 x1) (= old-y1 y1)
                     (= old-x2 x2) (= old-y2 y2))
          (recompute-extent-for-changed-child parent record
                                              old-x1 old-y1
                                              old-x2 old-y2)))))
  record)


;;; 16.3.1. Standard output record classes: sequence output recording

(defclass standard-sequence-output-record (compound-output-record)
  ((children :initform (make-array 8 :adjustable t :fill-pointer 0)
             :reader output-record-children)))

(defmethod add-output-record (child (record standard-sequence-output-record))
  (vector-push-extend child (output-record-children record))
  (setf (output-record-parent child) record))

(defmethod delete-output-record (child (record standard-sequence-output-record)
                                 &optional (errorp t))
  (with-slots (children) record
    (if-let ((pos (position child children :test #'eq)))
      (progn
        (setq children (replace children children
                                :start1 pos
                                :start2 (1+ pos)))
        (decf (fill-pointer children))
        (setf (output-record-parent child) nil))
      (when errorp
        (error "~S is not a child of ~S" child record)))))

(defmethod clear-output-record ((record standard-sequence-output-record))
  (let ((children (output-record-children record)))
    (map 'nil (lambda (child) (setf (output-record-parent child) nil))
         children)
    (fill children nil)
    (setf (fill-pointer children) 0)))

(defmethod output-record-count ((record standard-sequence-output-record))
  (length (output-record-children record)))

(defmethod map-over-output-records-1
    (function (record standard-sequence-output-record) function-args)
  "Applies FUNCTION to all children in the order they were added."
  (let ((function (alexandria:ensure-function function)))
    (if function-args
        (loop for child across (output-record-children record)
              do (apply function child function-args))
        (loop for child across (output-record-children record)
              do (funcall function child)))))

(defmethod map-over-output-records-containing-position
    (function (record standard-sequence-output-record) x y
     &optional (x-offset 0) (y-offset 0) &rest function-args)
  (decf x x-offset)
  (decf y y-offset)
  (let ((function (alexandria:ensure-function function)))
    (loop with children = (output-record-children record)
          for i from (1- (length children)) downto 0
          for child = (aref children i)
          when (and (multiple-value-bind (min-x min-y max-x max-y)
                        (output-record-hit-detection-rectangle* child)
                      (and (<= min-x x max-x) (<= min-y y max-y)))
                    (output-record-refined-position-test child x y))
            do (apply function child function-args))))

(defmethod map-over-output-records-overlapping-region
    (function (record standard-sequence-output-record) region
     &optional (x-offset 0) (y-offset 0) &rest function-args)
  (loop with function = (alexandria:ensure-function function)
        with transf = (make-translation-transformation x-offset y-offset)
        with region = (untransform-region transf region)
        for child across (output-record-children record)
        when (region-intersects-region-p region child)
          do (apply function child function-args)))


;;; 16.3.1. Standard output record classes: tree output recording
(defclass tree-output-record-entry ()
     ((record :initarg :record :reader tree-output-record-entry-record)
      (cached-rectangle :initform nil
                        :accessor tree-output-record-entry-cached-rectangle)
      (inserted-nr :initarg :inserted-nr
                   :accessor tree-output-record-entry-inserted-nr)))

(defvar %infinite-rectangle%
  (rectangles:make-rectangle)
  "This constant should be used to map over all tree output records.")

(defun make-tree-output-record-entry (record inserted-nr)
  (make-instance 'tree-output-record-entry
                 :record record
                 :inserted-nr inserted-nr))

(defun %record-to-spatial-tree-rectangle (record)
  (with-bounding-rectangle* (x1 y1 x2 y2) record
    (rectangles:make-rectangle :lows `(,x1 ,y1) :highs `(,x2 ,y2))))

(defun %output-record-entry-to-spatial-tree-rectangle (r)
  (when (null (tree-output-record-entry-cached-rectangle r))
    (let* ((record (tree-output-record-entry-record r)))
      (setf (tree-output-record-entry-cached-rectangle r)
            (%record-to-spatial-tree-rectangle record))))
  (tree-output-record-entry-cached-rectangle r))

(defun %make-tree-output-record-tree ()
  (spatial-trees:make-spatial-tree :r
                        :rectfun #'%output-record-entry-to-spatial-tree-rectangle))

(defclass standard-tree-output-record (compound-output-record)
  ((children-tree :initform (%make-tree-output-record-tree)
                  :accessor %tree-record-children)
   (children-hash :initform (make-hash-table :test #'eql)
                  :reader %tree-record-children-cache)
   (child-count :initform 0)
   (last-insertion-nr :initform 0 :accessor last-insertion-nr)))

(defun %entry-in-children-cache (record child)
  (gethash child (%tree-record-children-cache record)))

(defun (setf %entry-in-children-cache) (new-val record child)
  (setf (gethash child (%tree-record-children-cache record)) new-val))

(defun %remove-entry-from-children-cache (record child)
  (remhash child (%tree-record-children-cache record)))

(defun %refresh-entry-in-children-cache (record child)
  (let ((rtree (%tree-record-children record))
        (entry (%entry-in-children-cache record child)))
    (spatial-trees:delete entry rtree)
    (setf (tree-output-record-entry-cached-rectangle entry) nil)
    (spatial-trees:insert entry rtree)))

(defmethod output-record-children ((record standard-tree-output-record))
  (map 'list #'tree-output-record-entry-record
       (spatial-trees:search %infinite-rectangle%
                             (%tree-record-children record))))

(defmethod add-output-record (child (record standard-tree-output-record))
  (let ((entry (make-tree-output-record-entry
                child (incf (last-insertion-nr record)))))
    (spatial-trees:insert entry (%tree-record-children record))
    (setf (output-record-parent child) record)
    (setf (%entry-in-children-cache record child) entry))
  (incf (slot-value record 'child-count))
  (values))

(defmethod delete-output-record
    (child (record standard-tree-output-record) &optional (errorp t))
  (if-let ((entry (find child (spatial-trees:search
                               (%entry-in-children-cache record child)
                               (%tree-record-children record))
                        :key #'tree-output-record-entry-record)))
    (progn
      (decf (slot-value record 'child-count))
      (spatial-trees:delete entry (%tree-record-children record))
      (%remove-entry-from-children-cache record child)
      (setf (output-record-parent child) nil))
    (when errorp
      (error "~S is not a child of ~S" child record))))

(defmethod* (setf output-record-position) :after
  (nx ny (record standard-tree-output-record))
  (declare (ignore nx ny))
  (dolist (child (output-record-children record))
    (%refresh-entry-in-children-cache record child)))

(defmethod clear-output-record ((record standard-tree-output-record))
  (map nil (lambda (child)
             (setf (output-record-parent child) nil)
             (%remove-entry-from-children-cache record child))
       (output-record-children record))
  (setf (slot-value record 'child-count) 0)
  (setf (last-insertion-nr record) 0)
  (setf (%tree-record-children record) (%make-tree-output-record-tree)))

(defmethod output-record-count ((record standard-tree-output-record))
  (slot-value record 'child-count))

(defun map-over-tree-output-records
    (function record rectangle sort-order function-args)
  (dolist (child (sort (spatial-trees:search rectangle
                                             (%tree-record-children record))
                       (ecase sort-order
                         (:most-recent-first #'>)
                         (:most-recent-last #'<))
                       :key #'tree-output-record-entry-inserted-nr))
    (apply function (tree-output-record-entry-record child) function-args)))

(defmethod map-over-output-records-1
    (function (record standard-tree-output-record) args)
  (map-over-tree-output-records
   function record %infinite-rectangle% :most-recent-last args))

(defmethod map-over-output-records-containing-position
    (function (record standard-tree-output-record) x y
     &optional (x-offset 0) (y-offset 0) &rest function-args)
  (decf x x-offset)
  (decf y y-offset)
  (flet ((refined-test-function (record)
           (when (output-record-refined-position-test record x y)
             (apply function record function-args))))
    (declare (dynamic-extent #'refined-test-function))
    (let ((rectangle (rectangles:make-rectangle :lows `(,x ,y) :highs `(,x ,y))))
      (map-over-tree-output-records
       #'refined-test-function record rectangle :most-recent-first nil))))

(defmethod map-over-output-records-overlapping-region
    (function (record standard-tree-output-record) region
     &optional (x-offset 0) (y-offset 0) &rest function-args)
  (typecase region
    (everywhere-region (map-over-output-records-1 function record function-args))
    (nowhere-region nil)
    (otherwise (map-over-tree-output-records
                (lambda (child)
                  (when (region-intersects-region-p
                         (with-bounding-rectangle* (x1 y1 x2 y2) child
                           (make-rectangle* (+ x1 x-offset)
                                            (+ y1 y-offset)
                                            (+ x2 x-offset)
                                            (+ y2 y-offset)))
                         region)
                    (apply function child function-args)))
                record
                (%record-to-spatial-tree-rectangle (bounding-rectangle region))
                :most-recent-last
                '()))))

(defmethod recompute-extent-for-changed-child :before
    ((record standard-tree-output-record) child
     old-min-x old-min-y old-max-x old-max-y)
  (declare (ignore old-min-x old-min-y old-max-x old-max-y))
  (when (eql record (output-record-parent child))
    (%refresh-entry-in-children-cache record child)))


;;; Output Records: DISPLAYED-OUTPUT-RECORD graphical state mixins
(defmethod replay-output-record :around
    ((record gs-ink-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :ink (graphics-state-ink record))
    (call-next-method)))

(defmethod* (setf output-record-position) :before
  (nx ny (self gs-ink-mixin))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (let ((tr (make-translation-transformation dx dy)))
      (with-slots (ink) self
        (setf (graphics-state-ink self)
              (transform-region tr ink))))))

(defrecord-predicate gs-ink-mixin (ink)
  (if-supplied (ink)
    (design-equalp (slot-value record 'ink) ink)))

(defmethod replay-output-record :around
    ((record gs-line-style-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :line-style (graphics-state-line-style record))
    (call-next-method)))

(defrecord-predicate gs-line-style-mixin (line-style)
  (if-supplied (line-style)
    (line-style-equalp (slot-value record 'line-style) line-style)))

(defmethod replay-output-record :around
    ((record gs-text-style-mixin) stream &optional region x-offset y-offset)
  (declare (ignore region x-offset y-offset))
  (with-drawing-options (stream :text-style (graphics-state-text-style record))
    (call-next-method)))

(defrecord-predicate gs-text-style-mixin (text-style)
  (if-supplied (text-style)
    (text-style-equalp (slot-value record 'text-style) text-style)))

(defmethod replay-output-record :around
    ((record gs-transformation-mixin) stream &optional region (x-offset 0) (y-offset 0))
  (with-translation (stream x-offset y-offset)
    (with-drawing-options (stream :transformation (graphics-state-transformation record))
      (call-next-method record stream region 0 0))))

(defmethod* (setf output-record-position) :around
  (nx ny (self gs-transformation-mixin))
  (with-output-record-offset (dx dy ox oy nx ny self)
    ;; We don't call the next method, because that'd apply the transformation
    ;; twice. -- jd 2024-01-024
    (progn ;; multiple-value-prog1 (call-next-method)
      (setf #1=(graphics-state-transformation self)
            (compose-transformation-with-translation #1# dx dy)))))

(defrecord-predicate gs-transformation-mixin (transformation)
  (if-supplied (transformation)
    (transformation-equal (graphics-state-transformation record) transformation)))


;;; 16.3.2. Graphics Displayed Output Records

(defclass standard-displayed-output-record
    (gs-ink-mixin basic-output-record displayed-output-record)
  ((ink :reader displayed-output-record-ink)
   (stream :initarg :stream))
  (:documentation "Implementation class for DISPLAYED-OUTPUT-RECORD.")
  (:default-initargs :stream nil))

(defclass standard-graphics-displayed-output-record
    (standard-displayed-output-record
     graphics-displayed-output-record)
  ())

(defmethod match-output-records-1 and
  ((record standard-displayed-output-record)
   &key (x1 nil x1-p) (y1 nil y1-p)
   (x2 nil x2-p) (y2 nil y2-p)
   (bounding-rectangle nil bounding-rectangle-p))
  (if bounding-rectangle-p
      (region-equal record bounding-rectangle)
      (multiple-value-bind (my-x1 my-y1 my-x2 my-y2)
          (bounding-rectangle* record)
        (macrolet ((coordinate=-or-lose (key mine)
                     `(if (typep ,key 'coordinate)
                          (coordinate= ,mine ,key)
                          (error 'type-error
                                 :datum ,key
                                 :expected-type 'coordinate))))
          (and (or (null x1-p)
                   (coordinate=-or-lose x1 my-x1))
               (or (null y1-p)
                   (coordinate=-or-lose y1 my-y1))
               (or (null x2-p)
                   (coordinate=-or-lose x2 my-x2))
               (or (null y2-p)
                   (coordinate=-or-lose y2 my-y2)))))))

(defmethod output-record-equal and ((record standard-displayed-output-record)
                                    (record2 standard-displayed-output-record))
  (region-equal record record2))

(defmethod output-record-count ((record displayed-output-record))
  0)

(defmethod map-over-output-records-1
    (function (record displayed-output-record) function-args)
  (declare (ignore function function-args))
  nil)

(defmethod map-over-output-records-containing-position
    (function (record displayed-output-record) x y
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function x y x-offset y-offset function-args))
  nil)

(defmethod map-over-output-records-overlapping-region
    (function (record displayed-output-record) region
     &optional (x-offset 0) (y-offset 0)
     &rest function-args)
  (declare (ignore function region x-offset y-offset function-args))
  nil)


;;; 16.3.2. Graphics Displayed Output Records: drawing classes (utilities)

(defmacro generate-medium-recording-body (class-name args)
  (let ((arg-list (alexandria:mappend
                   (lambda (arg)
                     (destructuring-bind (name &optional (recording-form nil formp)
                                                         (storep t))
                         (alexandria:ensure-list arg)
                       (when storep
                         `(,(alexandria:make-keyword name)
                           ,(if formp
                                recording-form
                                name)))))
                   args)))
    `(progn
       (when (stream-recording-p stream)
         (let ((record (make-instance ',class-name :stream stream ,@arg-list)))
           (stream-add-output-record stream record)))
       (when (stream-drawing-p stream)
         (call-next-method)))))

;;; DEF-GRECORDING: This is the central interface through which recording is
;;; implemented for drawing functions. The body provided is used to compute the
;;; bounding rectangle of the rendered output. DEF-GRECORDING will define a
;;; class for the output record, with slots corresponding to the drawing
;;; function arguments. It also defines an INITIALIZE-INSTANCE method computing
;;; the bounding rectangle of the record. It defines a method for the medium
;;; drawing function specialized on output-recording-stream, which is
;;; responsible for creating the output record and adding it to the stream
;;; history. It also defines a REPLAY-OUTPUT-RECORD method, which calls the
;;; medium drawing function based on the recorded slots.
;;;
;;; The macro lambda list of DEF-GRECORDING is loosely based on that
;;; of DEFCLASS with a few differences:
;;;
;;; * The name can either be just a symbol or a list of a symbol followed by
;;;   keyword arguments which control what aspects should be generated: class,
;;;   medium-fn and replay-fn.
;;;
;;; * Instead of slot specifications, a list of argument descriptions is
;;;   supplied which is used to defined slots as well as arguments.  An argument
;;;   is either a symbol or a list of the form (NAME INITFORM STOREP) where
;;;   INITFORM computes the value to store in the output record and STOREP
;;;   controls whether a slot for the argument should be present in the output
;;;   record at all.
;;;
;;; * DEFCLASS options are not accepted, but a body is, as described above.
(defmacro def-grecording (name-and-options (&rest mixins) (&rest args)
                          &body body)
  (destructuring-bind (name &key (class t) (medium-fn t) (replay-fn t))
      (alexandria:ensure-list name-and-options)
    (let* ((method-name (symbol-concat '#:medium- name '*))
           (class-name (symbol-concat name '#:-output-record))
           (medium (gensym "MEDIUM"))
           (arg-names (mapcar #'alexandria:ensure-car args))
           (slot-names (alexandria:mappend
                        (lambda (arg)
                          (destructuring-bind (name &optional form (storep t))
                              (alexandria:ensure-list arg)
                            (declare (ignore form))
                            (when storep `(,name))))
                        args))
           (slots `((stream :initarg :stream)
                    ,@(loop for slot-name in slot-names
                            for initarg = (alexandria:make-keyword slot-name)
                            collect `(,slot-name :initarg ,initarg)))))
      `(progn
         ,@(when class
             `((defclass ,class-name (,@mixins standard-graphics-displayed-output-record)
                 ,slots)
               (defmethod initialize-instance :after ((graphic ,class-name) &key)
                 (with-slots (stream ink line-style text-style ,@slot-names)
                     graphic
                   (let ((medium (sheet-medium stream)))
                     (setf (rectangle-edges* graphic)
                           (progn ,@body)))))
               (defmethod reinitialize-instance :after ((graphic ,class-name) &key)
                 (with-slots (stream ink line-style text-style ,@slot-names)
                     graphic
                   (let ((medium (sheet-medium stream)))
                     (setf (rectangle-edges* graphic)
                           (progn ,@body)))))))
         ,@(when medium-fn
             `((defmethod ,method-name :around ((stream output-recording-stream) ,@arg-names)
                 (generate-medium-recording-body ,class-name ,args))))
         ,@(when replay-fn
             `((defmethod replay-output-record ((record ,class-name) stream
                                                &optional (region +everywhere+)
                                                  (x-offset 0) (y-offset 0))
                 (declare (ignore region))
                 (with-slots (,@slot-names) record
                   (let ((,medium (sheet-medium stream)))
                     ;; The medium graphics state is set up in :around methods.
                     (with-translation (,medium x-offset y-offset)
                       (,method-name ,medium ,@arg-names)))))))))))

(defun fix-line-style-unit (graphic medium)
  (let* ((line-style (graphics-state-line-style graphic))
         (thickness (line-style-effective-thickness line-style medium)))
    (unless (eq (line-style-unit line-style) :normal)
      (let ((dashes (line-style-effective-dashes line-style medium)))
        (setf (slot-value graphic 'line-style)
              (make-line-style :thickness thickness
                               :joint-shape (line-style-joint-shape line-style)
                               :cap-shape (line-style-cap-shape line-style)
                               :dashes dashes))))
    thickness))

;;; Helper function
(defun normalize-coords (dx dy &optional unit)
  (let ((norm (sqrt (+ (* dx dx) (* dy dy)))))
    (cond ((= norm 0.0d0)
           (values 0.0d0 0.0d0))
          (unit
           (let ((scale (/ unit norm)))
             (values (* dx scale) (* dy scale))))
          (t (values (/ dx norm) (/ dy norm))))))

(defun polygon-record-bounding-rectangle
    (coord-seq closed filled line-style border miter-limit)
  (cond (filled
         (coord-seq-bounds coord-seq 0))
        ((eq (line-style-joint-shape line-style) :round)
         (coord-seq-bounds coord-seq border))
        (t (let* ((x1 (elt coord-seq 0))
                  (y1 (elt coord-seq 1))
                  (min-x x1)
                  (min-y y1)
                  (max-x x1)
                  (max-y y1)
                  (len (length coord-seq)))
             (unless closed
               (setq min-x (- x1 border)  min-y (- y1 border)
                     max-x (+ x1 border)  max-y (+ y1 border)))
             ;; Setup for iterating over the coordinate vector.  If
             ;; the polygon is closed, deal with the extra segment.
             (multiple-value-bind (initial-xp initial-yp
                                   final-xn final-yn
                                   initial-index final-index)
                 (if closed
                     (values (elt coord-seq (- len 2))
                             (elt coord-seq (- len 1))
                             x1 y1
                             0 (- len 2))
                     (values x1 y1
                             (elt coord-seq (- len 2))
                             (elt coord-seq (- len 1))
                             2 (- len 4)))
               (ecase (line-style-joint-shape line-style)
                 (:miter
                  ;; FIXME: Remove successive positively proportional segments
                  (loop with sin-limit = (sin (* 0.5 miter-limit))
                        and xn and yn
                        for i from initial-index to final-index by 2
                        for xp = initial-xp then x
                        for yp = initial-yp then y
                        for x = (elt coord-seq i)
                        for y = (elt coord-seq (1+ i))
                        do (setf (values xn yn)
                                 (if (eql i final-index)
                                     (values final-xn final-yn)
                                     (values (elt coord-seq (+ i 2))
                                             (elt coord-seq (+ i 3)))))
                           (multiple-value-bind (ex1 ey1)
                               (normalize-coords (- x xp) (- y yp))
                             (multiple-value-bind (ex2 ey2)
                                 (normalize-coords (- x xn) (- y yn))
                               (let ((cos-a)
                                     (sin-a/2))
                                 (cond ((or (and (zerop ex1) (zerop ey2)) ; axis-aligned right angle
                                            (and (zerop ey1) (zerop ex2)))
                                        (minf min-x (- x border))
                                        (minf min-y (- y border))
                                        (maxf max-x (+ x border))
                                        (maxf max-y (+ y border)))
                                       ((progn
                                          (setf cos-a (+ (* ex1 ex2) (* ey1 ey2))
                                                sin-a/2 (sqrt (* 0.5 (max 0 (- 1.0f0 cos-a)))))
                                          (< sin-a/2 sin-limit)) ; almost straight, any direction
                                        (let ((nx (* border (max (abs ey1) (abs ey2))))
                                              (ny (* border (max (abs ex1) (abs ex2)))))
                                          (minf min-x (- x nx))
                                          (minf min-y (- y ny))
                                          (maxf max-x (+ x nx))
                                          (maxf max-y (+ y ny))))
                                       (t ; general case
                                        (let ((length (/ border sin-a/2)))
                                          (multiple-value-bind (dx dy)
                                              (normalize-coords (+ ex1 ex2)
                                                                (+ ey1 ey2)
                                                                length)
                                            (minf min-x (+ x dx))
                                            (minf min-y (+ y dy))
                                            (maxf max-x (+ x dx))
                                            (maxf max-y (+ y dy)))))))))))
                 ((:bevel :none)
                  (loop with xn and yn
                        for i from initial-index to final-index by 2
                        for xp = initial-xp then x
                        for yp = initial-yp then y
                        for x = (elt coord-seq i)
                        for y = (elt coord-seq (1+ i))
                        do (setf (values xn yn)
                                 (if (eql i final-index)
                                     (values final-xn final-yn)
                                     (values (elt coord-seq (+ i 2))
                                             (elt coord-seq (+ i 3)))))
                           (multiple-value-bind (ex1 ey1)
                               (normalize-coords (- x xp) (- y yp))
                             (multiple-value-bind (ex2 ey2)
                                 (normalize-coords (- x xn) (- y yn))
                               (let ((nx (* border (max (abs ey1) (abs ey2))))
                                     (ny (* border (max (abs ex1) (abs ex2)))))
                                 (minf min-x (- x nx))
                                 (minf min-y (- y ny))
                                 (maxf max-x (+ x nx))
                                 (maxf max-y (+ y ny))))))))
               (unless closed
                 (multiple-value-bind (x y)
                     (values (elt coord-seq (- len 2))
                             (elt coord-seq (- len 1)))
                   (minf min-x (- x border))
                   (minf min-y (- y border))
                   (maxf max-x (+ x border))
                   (maxf max-y (+ y border)))))
             (values min-x min-y max-x max-y)))))

(defun bezigon-record-bounding-rectangle (coord-seq closed filled border)
  (declare (ignore closed))
  (if filled
      (setf border 0)
      (setf border (ceiling border)))
  (let* ((min-x (elt coord-seq 0))
         (min-y (elt coord-seq 1))
         (max-x min-x)
         (max-y min-y))
    (map-over-bezigon-segments*
     (lambda (x0 y0 x1 y1 x2 y2 x3 y3)
       (multiple-value-bind (x1 x2) (cubic-bezier-dimension-min-max x0 x1 x2 x3)
         (minf min-x x1)
         (maxf max-x x2))
       (multiple-value-bind (y1 y2) (cubic-bezier-dimension-min-max y0 y1 y2 y3)
         (minf min-y y1)
         (maxf max-y y2)))
     coord-seq 4)
    (values (floor (- min-x border))
            (floor (- min-y border))
            (ceiling (+ max-x border))
            (ceiling (+ max-y border)))))

(declaim (inline %enclosing-transform-polygon))
(defun %enclosing-transform-polygon (transformation x1 y1 x2 y2)
  (let (min-x min-y max-x max-y)
    (setf (values min-x min-y) (transform-position transformation x1 y1)
          (values max-x max-y) (values min-x min-y))
    (flet ((do-point (x y)
             (with-transformed-position (transformation x y)
               (minf min-x x) (maxf max-x x)
               (minf min-y y) (maxf max-y y))))
      (do-point x1 y2)
      (do-point x2 y1)
      (do-point x2 y2))
    (values min-x min-y max-x max-y)))

(defclass coord-seq-mixin ()
  ((coord-seq :accessor coord-seq :initarg :coord-seq))
  (:documentation "Mixin class that implements methods for records that contain
   sequences of coordinates."))

(defun coord-seq-bounds (coord-seq border)
  (setf border (ceiling border))
  (let* ((min-x (elt coord-seq 0))
         (min-y (elt coord-seq 1))
         (max-x min-x)
         (max-y min-y))
    (do-sequence ((x y) coord-seq)
      (minf min-x x)
      (minf min-y y)
      (maxf max-x x)
      (maxf max-y y))
    (values (floor (- min-x border))
            (floor (- min-y border))
            (ceiling (+ max-x border))
            (ceiling (+ max-y border)))))

;;; record must be a standard-rectangle

(defmethod* (setf output-record-position) :around (nx ny (self coord-seq-mixin))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-prog1 (call-next-method)
      (let ((coords (slot-value self 'coord-seq))
            (odd nil))
        (map-into coords
                  (lambda (val)
                    (prog1 (if odd
                               (incf val dy)
                               (incf val dx))
                      (setf odd (not odd))))
                  coords)))))

(defmethod match-output-records-1 and ((record coord-seq-mixin)
                                       &key (coord-seq nil coord-seq-p))
  (or (null coord-seq-p)
      (let ((my-coord-seq (slot-value record 'coord-seq)))
        (sequence= my-coord-seq coord-seq #'coordinate=))))


;;; 16.3.2. Graphics Displayed Output Records: drawing classes (grecording)

(def-grecording draw-point (gs-line-style-mixin)
    (point-x point-y)
  (let ((border (/ (fix-line-style-unit graphic medium) 2)))
    (with-transformed-position ((medium-transformation medium) point-x point-y)
      (setf (slot-value graphic 'point-x) point-x
            (slot-value graphic 'point-y) point-y)
      (values (- point-x border)
              (- point-y border)
              (+ point-x border)
              (+ point-y border)))))

(defmethod* (setf output-record-position) :around
  (nx ny (self draw-point-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-prog1 (call-next-method)
      (with-slots (point-x point-y) self
        (incf point-x dx)
        (incf point-y dy)))))

(defrecord-predicate draw-point-output-record (point-x point-y)
  (and (if-supplied (point-x coordinate)
         (coordinate= (slot-value record 'point-x) point-x))
       (if-supplied (point-y coordinate)
         (coordinate= (slot-value record 'point-y) point-y))))

;;; Initialize the output record with a copy of COORD-SEQ, as the replaying code
;;; will modify it to be positioned relative to the output-record's position and
;;; making a temporary is (arguably) less bad than untransforming the coords
;;; back to how they were.
(def-grecording draw-points (coord-seq-mixin gs-line-style-mixin)
    ((coord-seq (copy-sequence-into-vector coord-seq)))
  (let* ((transformed-coord-seq (transform-positions (medium-transformation medium) coord-seq))
         (border (/ (fix-line-style-unit graphic medium) 2)))
    (setf (slot-value graphic 'coord-seq) transformed-coord-seq)
    (coord-seq-bounds transformed-coord-seq border)))

(def-grecording draw-line (gs-line-style-mixin)
    (point-x1 point-y1 point-x2 point-y2)
  (let* ((transform (medium-transformation medium))
         (border (/ (fix-line-style-unit graphic medium) 2)))
    (with-transformed-position (transform point-x1 point-y1)
      (with-transformed-position (transform point-x2 point-y2)
        (setf (slot-value graphic 'point-x1) point-x1
              (slot-value graphic 'point-y1) point-y1
              (slot-value graphic 'point-x2) point-x2
              (slot-value graphic 'point-y2) point-y2)
        (values (- (min point-x1 point-x2) border)
                (- (min point-y1 point-y2) border)
                (+ (max point-x1 point-x2) border)
                (+ (max point-y1 point-y2) border))))))

(defmethod* (setf output-record-position) :around
  (nx ny (self draw-line-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-prog1 (call-next-method)
      (with-slots (point-x1 point-y1 point-x2 point-y2) self
        (incf point-x1 dx)
        (incf point-y1 dy)
        (incf point-x2 dx)
        (incf point-y2 dy)))))

(defrecord-predicate draw-line-output-record (point-x1 point-y1
                                              point-x2 point-y2)
  (and (if-supplied (point-x1 coordinate)
         (coordinate= (slot-value record 'point-x1) point-x1))
       (if-supplied (point-y1 coordinate)
         (coordinate= (slot-value record 'point-y1) point-y1))
       (if-supplied (point-x2 coordinate)
         (coordinate= (slot-value record 'point-x2) point-x2))
       (if-supplied (point-y2 coordinate)
         (coordinate= (slot-value record 'point-y2) point-y2))))

;;; Regarding COORD-SEQ, see comment for DRAW-POINTS.
(def-grecording draw-lines (coord-seq-mixin gs-line-style-mixin)
    ((coord-seq (copy-sequence-into-vector coord-seq)))
  (let* ((transformation (medium-transformation medium))
         (transformed-coord-seq (transform-positions transformation coord-seq))
         (border (/ (fix-line-style-unit graphic medium) 2)))
    (setf coord-seq transformed-coord-seq)
    (coord-seq-bounds transformed-coord-seq border)))

;;; (setf output-record-position) and predicates for draw-lines-output-record
;;; are taken care of by methods on superclasses.

;;; Regarding COORD-SEQ, see comment for DRAW-POINTS.
(def-grecording draw-polygon (coord-seq-mixin gs-line-style-mixin)
    ((coord-seq (copy-sequence-into-vector coord-seq))
     closed filled)
  (let* ((transform (medium-transformation medium))
         (transformed-coord-seq (transform-positions transform coord-seq))
         (border (unless filled
                   (/ (fix-line-style-unit graphic medium) 2))))
    (setf coord-seq transformed-coord-seq)
    (polygon-record-bounding-rectangle transformed-coord-seq
                                       closed filled line-style border
                                       (medium-miter-limit medium))))

(defrecord-predicate draw-polygon-output-record (closed filled)
  (and (if-supplied (closed)
         (eql (slot-value record 'closed) closed))
       (if-supplied (filled)
         (eql (slot-value record 'filled) filled))))

(def-grecording draw-bezigon (coord-seq-mixin gs-line-style-mixin)
    ((coord-seq (copy-sequence-into-vector coord-seq))
     closed filled)
  (let* ((transform (medium-transformation medium))
         (transformed-coord-seq (transform-positions transform coord-seq))
         (border (unless filled
                   (/ (fix-line-style-unit graphic medium) 2))))
    (setf coord-seq transformed-coord-seq)
    (bezigon-record-bounding-rectangle transformed-coord-seq closed filled border)))

(defrecord-predicate draw-bezigon-output-record (filled)
  (if-supplied (filled)
    (eql (slot-value record 'filled) filled)))

(def-grecording (draw-rectangle :medium-fn nil) (gs-line-style-mixin)
    (left top right bottom filled)
  (let* ((transform (medium-transformation medium))
         (pre-coords (expand-rectangle-coords left top right bottom))
         (coords (transform-positions transform pre-coords))
         (border (unless filled
                   (/ (fix-line-style-unit graphic medium) 2))))
    (setf (values left top) (transform-position transform left top))
    (setf (values right bottom) (transform-position transform right bottom))
    (polygon-record-bounding-rectangle coords t filled line-style border
                                       (medium-miter-limit medium))))

(defmethod medium-draw-rectangle* :around ((stream output-recording-stream)
                                           left top right bottom filled)
  (let ((tr (medium-transformation stream)))
    (if (rectilinear-transformation-p tr)
        (generate-medium-recording-body draw-rectangle-output-record
                                        (left top right bottom filled))
        (medium-draw-polygon* stream
                              (expand-rectangle-coords left top right bottom)
                              t
                              filled))))

(def-grecording (draw-rectangles :medium-fn nil) (coord-seq-mixin gs-line-style-mixin)
    (coord-seq filled)
  (let* ((transform (medium-transformation medium))
         (border (unless filled
                   (/ (fix-line-style-unit graphic medium) 2))))
    (let ((transformed-coord-seq
            (map-repeated-sequence 'vector 2
                                   (lambda (x y)
                                     (with-transformed-position (transform x y)
                                       (values x y)))
                                   coord-seq)))
      (polygon-record-bounding-rectangle transformed-coord-seq
                                         t filled line-style border
                                         (medium-miter-limit medium)))))

(defmethod medium-draw-rectangles* :around ((stream output-recording-stream)
                                            coord-seq filled)
  (let ((tr (medium-transformation stream)))
    (if (rectilinear-transformation-p tr)
        (generate-medium-recording-body
         draw-rectangles-output-record
         ((coord-seq (copy-sequence-into-vector coord-seq)) filled))
        (do-sequence ((left top right bottom) coord-seq)
          (medium-draw-polygon* stream (vector left top
                                               left bottom
                                               right bottom
                                               right top)
                                t filled)))))

(defmethod medium-clear-area :around ((medium output-recording-stream) left top right bottom)
  (declare (ignore left top right bottom))
  (when (stream-drawing-p medium)
    (call-next-method)))

(defmethod* (setf output-record-position) :around
  (nx ny (self draw-rectangle-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-prog1 (call-next-method)
      (with-slots (left top right bottom) self
        (incf left dx)
        (incf top dy)
        (incf right dx)
        (incf bottom dy)))))

(defrecord-predicate draw-rectangle-output-record (left top right bottom filled)
  (and (if-supplied (left coordinate)
         (coordinate= (slot-value record 'left) left))
       (if-supplied (top coordinate)
         (coordinate= (slot-value record 'top) top))
       (if-supplied (right coordinate)
         (coordinate= (slot-value record 'right) right))
       (if-supplied (bottom coordinate)
         (coordinate= (slot-value record 'bottom) bottom))
       (if-supplied (filled)
         (eql (slot-value record 'filled) filled))))

(def-grecording draw-ellipse (gs-line-style-mixin)
    (center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
     start-angle end-angle filled)
  (let ((transform (medium-transformation medium)))
    (setf (values center-x center-y)
          (transform-position transform center-x center-y))
    (setf (values radius-1-dx radius-1-dy)
          (transform-distance transform radius-1-dx radius-1-dy))
    (setf (values radius-2-dx radius-2-dy)
          (transform-distance transform radius-2-dx radius-2-dy))
    ;; We untransform-angle below, as the ellipse angles go counter-clockwise
    ;; in screen coordinates, whereas our transformations rotate clockwise in
    ;; the default coorinate system. -Hefner
    (setf start-angle (untransform-angle transform start-angle))
    (setf end-angle   (untransform-angle transform end-angle))
    (when (reflection-transformation-p transform)
      (rotatef start-angle end-angle))
    (multiple-value-setq (start-angle end-angle)
      (normalize-angle* start-angle end-angle))
    (multiple-value-bind (min-x min-y max-x max-y)
        (ellipse-bounding-rectangle*
         center-x center-y radius-1-dx radius-1-dy radius-2-dx radius-2-dy
         start-angle end-angle filled)
      (if filled
          (values min-x min-y max-x max-y)
          (let ((border (/ (fix-line-style-unit graphic medium) 2)))
            (values (floor (- min-x border))
                    (floor (- min-y border))
                    (ceiling (+ max-x border))
                    (ceiling (+ max-y border))))))))

(defmethod* (setf output-record-position) :around
    (nx ny (self draw-ellipse-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-prog1 (call-next-method)
      (with-slots (center-x center-y) self
        (incf center-x dx)
        (incf center-y dy)))))

(defrecord-predicate draw-ellipse-output-record (center-x center-y filled)
  (and (if-supplied (center-x coordinate)
                    (coordinate= (slot-value record 'center-x) center-x))
       (if-supplied (center-y coordinate)
                    (coordinate= (slot-value record 'center-y) center-y))
       (if-supplied (filled)
                    (eql (slot-value record 'filled) filled))))

(def-grecording draw-text (gs-text-style-mixin gs-transformation-mixin)
    ((string (create-string string start end))
     x y ;; collapses onto OUTPUT-RECORD-ORIGIN (important)
     (start 0) (end nil)
     align-x align-y
     toward-x toward-y transform-glyphs)
  ;; FIXME Text direction.
  (let* ((transformation (medium-transformation medium))
         (text-style (graphics-state-text-style graphic)))
    (multiple-value-bind (sw sh dx dy)
        (text-metrics medium string :text-style text-style)
      (case align-x
        (:left   (setf dx sw))
        (:right  (setf dx 0))
        (:center (setf dx (/ sw 2))))
      (case align-y
        (:top    (setf dy sh))
        (:bottom (setf dy 0))
        (:center (setf dy (/ sh 2))))
      (%enclosing-transform-polygon transformation
                                    (- (+ x dx) sw)
                                    (- (+ y dy) sh)
                                    (+ x dx)
                                    (+ y dy)))))

#+ (or) ;; debugging
(defmethod replay-output-record :after
    ((record basic-output-record) stream
     &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region))
  (with-translation (stream x-offset y-offset)
    (let ((rect (copy-bounding-rectangle record))
          (ink1 (compose-in +blue+ (make-opacity .1)))
          (ink2 +black+)
          (ink3 (compose-in +black+ (make-opacity .5))))
      (draw-design stream rect :filled t :ink ink1)
      (draw-design stream rect :filled nil :ink ink2 :line-thickness .5)
      (with-bounding-rectangle* (:center-x cx :center-y cy) rect
        (draw-point* stream cx cy :line-thickness 10 :ink ink3)))))

(defrecord-predicate draw-text-output-record
    (string start end
     x y align-x align-y toward-x toward-y transform-glyphs)
  ;; Compare position first because it is cheap and an update is most
  ;; likely to change the position.
  (and (if-supplied (x coordinate)
         (coordinate= (slot-value record 'x) x))
       (if-supplied (y coordinate)
         (coordinate= (slot-value record 'y) y))
       (if-supplied (string)
         (let ((start1 0)
               (start2 0)
               (end1 nil)
               (end2 nil))
           (if-supplied (start)
             (setf start1 (slot-value record 'start)
                   start2 start))
           (if-supplied (end)
             (setf end1 (slot-value record 'end)
                   end2 end))
           (string= (slot-value record 'string) string
                    :start1 start1 :end1 end1
                    :start2 start2 :end2 end2)))
       (if-supplied (align-x)
         (eq (slot-value record 'align-x) align-x))
       (if-supplied (align-y)
         (eq (slot-value record 'align-y) align-y))
       (if-supplied (toward-x coordinate)
         (coordinate= (slot-value record 'toward-x) toward-x))
       (if-supplied (toward-y coordinate)
         (coordinate= (slot-value record 'toward-y) toward-y))
       (if-supplied (transform-glyphs)
         (eq (slot-value record 'transform-glyphs) transform-glyphs))))

(def-grecording draw-pattern ()
    (pattern x y)
  (let* ((x2 (+ x (pattern-width pattern)))
         (y2 (+ y (pattern-height pattern)))
         (transf (medium-transformation medium))
         (coords (expand-rectangle-coords x y x2 y2)))
    (multiple-value-bind (x1 y1 x2 y2)
        (coord-seq-bounds (transform-positions transf coords) 0)
      (setf x x1 y y1)
      (values x1 y1 x2 y2))))


;;; 16.3.3. Text Displayed Output Record

(defclass standard-text-displayed-output-record (updating-cursor-mixin
                                                 text-displayed-output-record
                                                 standard-displayed-output-record)
  (;; Stream is used to query record dimensions and drawing options.
   (stream :initarg :stream)
   ;; All objects making the output record.
   (objects :initform (make-array 0 :adjustable t :fill-pointer t))))

(defmethod initialize-instance :after
    ((self standard-text-displayed-output-record) &key stream)
  (update-cursor (start-cursor self) (stream-text-cursor stream))
  (setf (output-record-end-cursor-position self)
        (stream-cursor-position stream)))

;;; Forget match-output-records-1 for standard-text-displayed-output-record; it
;;; doesn't make much sense because these records have state that is not
;;; initialized via initargs.

(defmethod output-record-equal and ((r1 standard-text-displayed-output-record)
                                    (r2 standard-text-displayed-output-record))
  (and (equals (start-cursor r1) (start-cursor r2))
       (eql (slot-value r1 'objects) (slot-value r2 'objects))))

(defmethod print-object ((self standard-text-displayed-output-record) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (multiple-value-bind (x y) (cursor-position (start-cursor self))
      (format stream "[~D ~D] (~D objects)" x y (slot-value self 'objects)))))

;;; Before implementing a dynamic reflow we must implement the repaint
;;; queue. Otherwise we'll get nasty race conditions. -- jd 2023-12-29
(defmethod replay-output-record ((self standard-text-displayed-output-record) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region))
  (let ((cursor (stream-text-cursor stream)))
    (update-cursor cursor (start-cursor self))
    (stream-increment-cursor-position stream x-offset y-offset)
    (with-identity-transformation (stream)
      (loop for record across (slot-value self 'objects)
            do (multiple-value-bind (x0 y0) (output-record-origin record)
                 (multiple-value-bind (dx dy fx fy bx by ex ey)
                     (stream-cursor-motion stream cursor record)
                   (setf (cursor-offset cursor) (values bx by))
                   (setf (cursor-extent cursor) (values ex ey))
                   (replay-output-record record stream nil (- dx x0) (- dy y0))
                   (setf (cursor-position cursor) (values fx fy))))))))

(defun update-output-record-cursor (self object)
  (let* ((stream (slot-value self 'stream))
         (start-cursor (start-cursor self))
         (sheet-cursor (stream-text-cursor stream)))
    (multiple-value-bind (x0 y0 fx fy bx by ex ey)
        (stream-cursor-motion stream sheet-cursor object)
      (declare (ignore x0 y0))
      (setf (output-record-end-cursor-position self) (values fx fy)
            (cursor-position sheet-cursor) (values fx fy)
            (cursor-offset start-cursor) (values bx by)
            (cursor-offset sheet-cursor) (values bx by)
            (cursor-extent start-cursor) (values ex ey)
            (cursor-extent sheet-cursor) (values ex ey)))))

;;; Recomputes the cursor motion from scratch.
(defun compute-output-record-cursor (self)
  (let ((stream (slot-value self 'stream))
        (cursor (start-cursor self)))
    (multiple-value-bind (x0 y0) (output-record-start-cursor-position self)
      (reset-stream-cursor stream cursor)
      (setf (cursor-position cursor) (values x0 y0))
      (update-cursor (stream-text-cursor stream) cursor)))
  (with-slots (objects) self
    (loop for object across objects do
      (update-output-record-cursor self object))))

(defmethod tree-recompute-extent ((self standard-text-displayed-output-record))
  (nest
   (let ((start-cursor (start-cursor self))
         (stream (slot-value self 'stream))))
   (multiple-value-bind (cx cy) (cursor-position start-cursor))
   (multiple-value-bind (fx fy) (output-record-end-cursor-position self))
   (multiple-value-bind (sw sh) (cursor-size start-cursor))
   (let (x1 y1 x2 y2)
     (ecase (stream-line-direction stream)
       (:left-to-right (setf x1 cx x2 fx))
       (:right-to-left (setf x2 cx x1 fx))
       (:top-to-bottom (setf y1 cy y2 fy))
       (:bottom-to-top (setf y2 cy y1 fy)))
     (ecase (stream-page-direction stream)
       (:top-to-bottom (setf y1 cy y2 (+ fy sh)))
       (:bottom-to-top (setf y2 cy y1 (- fy sh)))
       (:left-to-right (setf x1 cx x2 (+ fx sw)))
       (:right-to-left (setf x2 cx x1 (- fx sw))))
     (setf (rectangle-edges* self)
           (values (coordinate x1) (coordinate y1)
                   (coordinate x2) (coordinate y2)))))
  self)

(defun add-object-to-text-record (self object)
  (with-slots (objects) self
    (vector-push-extend object objects)
    (update-output-record-cursor self object)
    (tree-recompute-extent self)))

;;; The newline character is not part of the text output record, however it may
;;; influence the cursor (but only when there is no other content on the line).
(defun add-newline-output-to-text-record (self)
  (when (zerop (length (slot-value self 'objects)))
    (let* ((stream (slot-value self 'stream))
           (start-cursor (start-cursor self))
           (sheet-cursor (stream-text-cursor stream)))
      (multiple-value-bind (x0 y0 fx fy bx by ex ey)
          (stream-cursor-motion stream sheet-cursor "M")
        (declare (ignore x0 y0 fx fy))
        (setf (cursor-offset start-cursor) (values bx by)
              (cursor-offset sheet-cursor) (values bx by)
              (cursor-extent start-cursor) (values ex ey)
              (cursor-extent sheet-cursor) (values ex ey)))
      (tree-recompute-extent self))))

(defmethod add-character-output-to-text-record
    ((self standard-text-displayed-output-record)
     character text-style char-width line-height new-baseline)
  (add-string-output-to-text-record self (string character)
                                    0 1 text-style
                                    char-width line-height new-baseline))

(defmethod add-string-output-to-text-record
    ((self standard-text-displayed-output-record)
     string start end text-style width height baseline)
  (orf end (length string))
  (with-slots (objects stream) self
    (let ((last-object (unless (emptyp objects)
                         (last-elt objects)))
          (ink (medium-ink stream)))
      (if (and (typep last-object 'draw-text-output-record)
               (match-output-records last-object
                                     :text-style text-style :ink ink))
          (with-slots ((record-string string)) last-object
            (append-string record-string string start end)
            (reinitialize-instance last-object)
            (compute-output-record-cursor self))
          (let* ((string (create-string string start end))
                 (record (make-instance 'draw-text-output-record
                                        :stream stream
                                        :string string
                                        :x 0 :y 0
                                        :start 0 :end nil
                                        :align-x :left :align-y :baseline
                                        :toward-x nil :toward-y nil
                                        :transform-glyphs nil
                                        :ink ink :text-style text-style)))
            (vector-push-extend record objects)
            (update-output-record-cursor self record))))
    (tree-recompute-extent self)))

(defmethod text-displayed-output-record-string
    ((record standard-text-displayed-output-record))
  (flet ((to-string (object)
           (typecase object
             (character (string object))
             (string object)
             (otherwise "@"))))
    (with-slots (objects) record
      (cond ((= 0 (length objects))
             "")
            ((= 1 (length objects))
             (to-string (elt objects 0)))
            (t
             (with-output-to-string (result)
               (loop for object across objects
                     do (write-string (to-string object) result))))))))


(defclass clipping-output-record (standard-tree-output-record)
  ((clipping-region :initarg :clipping-region :type region
                    :accessor graphics-state-clip)))

(defmethod replay-output-record ((record clipping-output-record) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (declare (ignore region))
  (let* ((translation (make-translation-transformation x-offset y-offset))
         (transf-clip (transform-region translation (graphics-state-clip record))))
    (with-clipping-region (stream transf-clip)
      (call-next-method))))

(defmethod* (setf output-record-position) :around
  (nx ny (self clipping-output-record))
  (with-output-record-offset (dx dy ox oy nx ny self)
    (multiple-value-prog1 (call-next-method)
      (let ((tr (make-translation-transformation dx dy)))
        (setf (graphics-state-clip self)
              (transform-region tr (graphics-state-clip self)))))))

(defmethod output-record-refined-position-test
    ((record clipping-output-record) x y)
  (region-contains-position-p (graphics-state-clip record) x y))


;;; Baseline

(defun output-record-baseline (record)
  (multiple-value-bind (bx by) (output-record-offset record)
    (values by bx)))

(defun output-record-offset-x (record)
  (nth-value 0 (output-record-offset record)))

(defun output-record-offset-y (record)
  (nth-value 1 (output-record-offset record)))

(defmethod output-record-offset ((record output-record))
  "Fall- fall back method :-)"
  (with-bounding-rectangle* (:height height) record
    (values 0 height nil)))

(defmethod output-record-offset ((record basic-output-record))
  "Fall back method"
  (multiple-value-bind (x0 y0) (output-record-origin record)
    (values x0 y0)))

(defmethod output-record-offset ((self standard-text-displayed-output-record))
  (multiple-value-bind (x y) (cursor-offset (start-cursor self))
    (values x y t)))

(defmethod output-record-offset ((record compound-output-record))
  (map-over-output-records (lambda (sub-record)
                             (multiple-value-bind (bx by definitive)
                                 (output-record-offset sub-record)
                               (when definitive
                                 (return-from output-record-offset
                                   (values bx by t)))))
                           record)
  (call-next-method))


;;; The underlying mechanism for caching output records based on their
;;; :unique-id. It is mixed to updating streams and output records.

;;; The map from unique values to output records. Unfortunately the :ID-TEST is
;;; specified in the child updating output records, not in the record that holds
;;; the cache! So, the map lookup code jumps through some hoops to use a hash
;;; table if the child id tests allow that and if there enough records in the
;;; map to make that worthwhile.

(defclass updating-output-map-mixin ()
  ((id-map :accessor id-map :initform nil)
   (id-counter :accessor id-counter
               :documentation "The counter used to assign unique ids to
                updating output records without one.")
   (tester-function :accessor tester-function :initform 'none
                    :documentation "The function used to lookup
  updating output records in this map if unique; otherwise, :mismatch.")
   (element-count :accessor element-count :initform 0)))

;;; Complete guess...
(defparameter *updating-map-threshold* 10
  "The limit at which the id map in an updating output record switches to a
  hash table.")

;;; ((eq map-test-func :mismatch)
;;;   nil)
(defun function-matches-p (map func)
  (let ((map-test-func (tester-function map)))
    (cond ((eq map-test-func func)
           t)
          ((and (symbolp map-test-func) (symbolp func)) ; not eq
           nil)
          ((and (symbolp map-test-func) (fboundp map-test-func))
           (eq (symbol-function map-test-func) func))
          ((and (symbolp func) (fboundp func))
           (eq map-test-func (symbol-function func)))
          (t nil))))

(defun ensure-test (map test)
  (unless (function-matches-p map test)
    (explode-map-hash map)
    (setf (tester-function map) :mismatch)))

(defgeneric clear-map (map)
  (:method ((map updating-output-map-mixin))
    (setf (id-map map) nil)
    (setf (id-counter map) 0)
    (setf (element-count map) 0)))

;;; Perhaps these should be generic functions, but in the name of premature
;;; optimization they're not :)
(defun get-from-map (map value test)
  (when (eq (tester-function map) 'none)
    (return-from get-from-map nil))
  (ensure-test map test)
  (let ((map (id-map map)))
    (if (hash-table-p map)
        (gethash value map)
        (cdr (assoc value map :test test)))))

(defun maybe-convert-to-hash (map)
  (let ((test (tester-function map)))
    (when (and (not (eq test :mismatch))
               (> (element-count map) *updating-map-threshold*)
               (or (case test
                     ((eq eql equal equalp) t))
                   (eq test #'eq)
                   (eq test #'eql)
                   (eq test #'equal)
                   (eq test #'equalp)))
      (let ((new-map (make-hash-table :test test)))
        (loop
           for (key . value) in (id-map map)
           do (setf (gethash key new-map) value))
        (setf (id-map map) new-map)))))

(defun explode-map-hash (map)
  (let ((hash-map (id-map map)))
    (when (hash-table-p hash-map)
      (loop
         for key being each hash-key of hash-map using (hash-value record)
         collect (cons key record) into alist
         finally (setf (id-map map) alist)))))

(defun add-to-map (map record value test replace)
  (if (eq (tester-function map) 'none)
      (setf (tester-function map) test)
      (ensure-test map test))
  (let ((val-map (id-map map)))
    (if (hash-table-p val-map)
        (multiple-value-bind (existing-value in-table)
            (if replace
                (gethash value val-map)
                (values nil nil))
          (declare (ignore existing-value))
          (setf (gethash value val-map) record)
          (unless in-table
            (incf (element-count map))))
        (let ((val-cons (if replace
                            (assoc value val-map :test test)
                            nil)))
          (if val-cons
              (setf (cdr val-cons) record)
              (progn
                (setf (id-map map) (acons value record val-map))
                (incf (element-count map))
                (maybe-convert-to-hash map)))))))

(defun delete-from-map (map value test)
  (ensure-test map test)
  (let ((val-map (id-map map))
        (deleted nil))
    (if (hash-table-p val-map)
        (setf deleted (remhash value val-map))
        (setf (values (id-map map) deleted)
              (delete-1 value val-map :test test :key #'car)))
    (when deleted
      (decf (element-count map)))))

;;; Reset the ID counter so that updating output records without explicit IDs
;;; can be assigned one during a run of the code. I'm not sure about using
;;; reinitialize-instance for this...
(defmethod shared-initialize :after
    ((obj updating-output-map-mixin) slot-names &key)
  (declare (ignore slot-names))
  (setf (id-counter obj) 0))



;;; Updating output record.
(defclass standard-updating-output-record (updating-cursor-mixin
                                           updating-output-map-mixin
                                           standard-sequence-output-record
                                           updating-output-record)
  ((unique-id
    :reader output-record-unique-id
    :initarg :unique-id)
   (id-test
    :reader output-record-id-test
    :initarg :id-test
    :initform #'eql)
   (cache-value
    :reader output-record-cache-value
    :initarg :cache-value)
   (cache-test
    :reader output-record-cache-test
    :initarg :cache-test
    :initform #'eql)
   (fixed-position
    :reader output-record-fixed-position
    :initarg :fixed-position :initform nil)
   (displayer
    :accessor output-record-displayer
    :initarg :displayer)
   (old-children
    :accessor old-children
    :documentation "Contains the output record tree for the current display.")
   (output-record-dirty
    :accessor output-record-dirty
    :initform :updating
    :documentation ":updating :updated :clean :moved")
   (parent-cache
    :accessor parent-cache :initarg :parent-cache
    :documentation "The parent cache in which this updating output record is
                    stored.")
   (stream
    :accessor updating-output-stream :initarg :stream :initform nil
    :documentation "Capture the screen in order to restrict update to visible
                    records")
   (parent-updating-output
    :accessor parent-updating-output
    :initarg :parent-updating-output :initform nil
    :documentation "A backlink to the updating-output-parent above this one in the tree.")
   ;; Results of (setf output-record-position) while updating
   (old-bounds
    :accessor old-bounds
    :initform (make-bounding-rectangle 0.0d0 0.0d0 0.0d0 0.0d0)
    :documentation "Holds the old bounds of an updating output record if that
                    can no longer be determined from the old-children.")
   ;; on-screen state?
   ))

(defmethod print-object ((obj standard-updating-output-record) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-standard-rectangle* (x1 y1 x2 y2) obj
      (format stream "X ~S:~S Y ~S:~S " x1 x2 y1 y2))
    (when (slot-boundp obj 'unique-id)
      (let ((*print-length* 10)
            (*print-level* 3))
        (format stream " ~S" (output-record-unique-id obj))))))

(defgeneric sub-record (record)
  (:method ((record standard-updating-output-record))
    (let ((children (output-record-children record)))
      (if (zerop (length children))
          nil
          (aref children 0)))))

;;; Prevent deleted output records from coming back from the dead.
(defmethod delete-output-record :after
    ((child standard-updating-output-record) record &optional errorp)
  (declare (ignore record errorp))
  (let ((pcache (parent-cache child)))
    (delete-from-map pcache
                     (output-record-unique-id child)
                     (output-record-id-test child))))

;; XXX Update :{X,Y}-OFFSET? -- jd 2024-01-12
#+ (or)
(defmethod replay-output-record ((record updating-output-record) stream
                                 &optional region (x-offset 0) (y-offset 0))
  (unless region
    (setf region (sheet-visible-region stream)))
  (with-drawing-options (stream :clipping-region region)
    (map-over-output-records-overlapping-region
     #'replay-output-record record region x-offset y-offset
     stream region x-offset y-offset)))


;;; Helper functions

;;; Function for visiting updating-output records in a tree.
(defgeneric map-over-updating-output (function root use-old-records)
  (:method (function (record standard-updating-output-record) use-old-records)
    (funcall function record)
    (let ((children (cond (use-old-records
                           (when (slot-boundp record 'old-children)
                             (old-children record)))
                          (t (sub-record record)))))
      (when children
        (map-over-updating-output function children use-old-records))))
  (:method (function (record compound-output-record) use-old-records)
    (flet ((mapper (r) (map-over-updating-output function r use-old-records)))
      (declare (dynamic-extent #'mapper))
      (map-over-output-records #'mapper record)))
  (:method (function record use-old-records)
    (declare (ignore function record use-old-records))
    nil))

;;; Notification protocol for the record change propagation. The function
;;; NOTE-OUTPUT-RECORD-CHILD-CHANGED propagates changes upwards.

(defmethod propagate-output-record-changes-p
    (record child mode old-position old-bounding-rectangle)
  (declare (ignore mode old-position))
  (and record
       (or (null old-bounding-rectangle)
           (not (region-equal child old-bounding-rectangle)))))

(defmethod propagate-output-record-changes
    (record child mode &optional old-position old-bounding-rectangle
                                 difference-set check-overlapping)
  (declare (ignore old-position))
  (ecase mode
    (:none
     nil)
    (:add
     (recompute-extent-for-new-child record child))
    (:delete
     (with-bounding-rectangle* (x1 y1 x2 y2) child
       (recompute-extent-for-changed-child record child x1 y1 x2 y2)))
    ((:change :move :clear)
     (with-bounding-rectangle* (x1 y1 x2 y2) old-bounding-rectangle
       (recompute-extent-for-changed-child record child x1 y1 x2 y2))))
  (values difference-set check-overlapping))

(defmethod note-output-record-child-changed
    (record child mode old-position old-bounding-rectangle stream
     &key difference-set check-overlapping)
  (if (propagate-output-record-changes-p
       record child mode old-position old-bounding-rectangle)
      (let ((old-bbox (copy-bounding-rectangle record)))
        (multiple-value-bind (difference-set check-overlapping)
            (propagate-output-record-changes
             record child mode old-position old-bounding-rectangle
             difference-set check-overlapping)
          (note-output-record-child-changed
           (output-record-parent record) record :change nil old-bbox stream
           :difference-set difference-set
           :check-overlapping check-overlapping)))
      (destructuring-bind (erases moves draws erases* moves*) difference-set
        (incremental-redisplay stream nil erases moves draws erases* moves*))))


;;; 16.3.4. Top-Level Output Records

(defclass standard-sequence-output-history
    (standard-sequence-output-record stream-output-history-mixin)
  ())

(defclass standard-tree-output-history
    (standard-tree-output-record stream-output-history-mixin)
  ())
