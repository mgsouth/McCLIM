;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2023 Daniel Kochmaski <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; BOUND a building block for all kinds of space requirements. It specifies the
;;; default, the minimum and the maximum value of the dimension.
;;;
;;; The full space requirement depends on the object's measure, padding and
;;; margins. CLIM specification defines only a the measure, but padding and
;;; margins creep into the pane specifications.
;;;
(in-package "CLIM-INTERNALS")

(defconstant +fill+
  (expt 10 (floor (log most-positive-fixnum 10))))

(defun fillp (val)
  (= val +fill+))

;;; To be implemented later.
(defmacro amb (&rest elements) `(or ,@elements))

(defclass bound ()
  ((val :initarg :val :reader bound-val)
   (min :initarg :min :reader bound-min)
   (max :initarg :max :reader bound-max))
  (:default-initargs :val 0 :min 0 :max +fill+))

(defvar +null-bound+ (make-instance 'bound))

(defun null-bound-p (bound)
  (eql bound +null-bound+))

(defmethod print-object ((object bound) stream)
  (flet ((fix (val) (if (fillp val) '+fill+ val)))
    (print-unreadable-object (object stream :type nil :identity nil)
      (format stream "~a [~a ~a]"
              (fix (bound-val object))
              (fix (bound-min object))
              (fix (bound-max object))))))

(defun make-bound (val &optional (min 0) (max +fill+))
  (clampf min 0 (min max +fill+))
  (clampf max min +fill+)
  (clampf val min max)
  (if (and (zerop val) (zerop min) (fillp max))
      +null-bound+
      (make-instance 'bound :val val :min min :max max)))

(defun ensure-bound (val)
  (typecase val
    (bound val)
    (null +null-bound+)
    (list (apply #'make-bound val))
    (otherwise (make-bound val))))

(defun bound-combine (function b1 b2)
  (make-bound (funcall function (bound-val b1) (bound-val b2))
              (funcall function (bound-min b1) (bound-min b2))
              (funcall function (bound-max b1) (bound-max b2))))

(defun bound-combine* (function b1 b2)
  (funcall function b1 b2))

(defun bound-equal (b1 b2)
  (and (= (bound-val b1) (bound-val b2))
       (= (bound-min b1) (bound-min b2))
       (= (bound-max b1) (bound-max b2))))

(defun maximize-bound (b1 b2)
  (let ((min (min (bound-min b1) (bound-min b2)))
        (max (max (bound-max b1) (bound-max b2)))
        (val (amb (bound-val b1) (bound-val b2))))
    (make-bound val min max)))

(defun minimize-bound (b1 b2)
  (let ((min (max (bound-min b1) (bound-min b2)))
        (max (min (bound-max b1) (bound-max b2)))
        (val (amb (bound-val b1) (bound-val b2))))
    (maxf max min)
    (minf min max)
    (clampf val min max)
    (make-bound val min max)))


;;; The size requirement has horizontal bound and vertical bound. It may be used
;;; to specify the measure of the object or spacing between respecitve siblings.
(defclass size-req ()
  ((ws :initarg :ws :reader size-ws)
   (hs :initarg :hs :reader size-hs))
  (:default-initargs :ws +null-bound+ :hs +null-bound+))

(defvar +null-size-req+
  (make-instance 'size-req))

(defun null-size-p (size)
  (eql size +null-size-req+))

(defmethod print-object ((object size-req) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "ws: ~s hs: ~s" (size-ws object) (size-hs object))))

(defun make-size-req (width &optional (height width))
  (setf width  (ensure-bound width))
  (setf height (ensure-bound height))
  (if (and (null-bound-p width)
           (null-bound-p height))
      +null-size-req+
      (make-instance 'size-req :ws width :hs height)))

(defun ensure-size-req (value)
  (typecase value
    (size-req value)
    (null +null-size-req+)
    (list (apply #'make-size-req value))
    (real (make-size-req value))
    (otherwise
     (with-bounding-rectangle* (:width w :height h) value
       (make-size-req w h)))))

(defun size-req-combine (fun b1 b2)
  (setf b1 (ensure-size-req b1))
  (setf b2 (ensure-size-req b2))
  (make-size-req (bound-combine fun (size-ws b1) (size-ws b2))
                 (bound-combine fun (size-hs b1) (size-hs b2))))

(defun size-req-combine* (fun b1 b2)
  (setf b1 (ensure-size-req b1))
  (setf b2 (ensure-size-req b2))
  (make-size-req (bound-combine* fun (size-ws b1) (size-ws b2))
                 (bound-combine* fun (size-hs b1) (size-hs b2))))

(defun size-equal (b1 b2)
  (setf b1 (ensure-size-req b1))
  (setf b2 (ensure-size-req b2))
  (and (bound-equal (size-ws b1) (size-ws b2))
       (bound-equal (size-hs b1) (size-hs b2))))

(defmethod bounding-rectangle* ((object size-req))
  (values 0 0
          (bound-val (size-ws object))
          (bound-val (size-hs object))))


;;; The fill requirement is used to describe padding and margins which "add" the
;;; requirement to appropriate edge of the "inner" object.
(defclass fill-req ()
  ((x1 :initarg :x1 :reader fill-x1)
   (y1 :initarg :y1 :reader fill-y1)
   (x2 :initarg :x2 :reader fill-x2)
   (y2 :initarg :y2 :reader fill-y2))
  (:default-initargs :x1 +null-bound+ :y1 +null-bound+
                     :x2 +null-bound+ :y2 +null-bound+))

(defvar +null-fill-req+ (make-instance 'fill-req))

(defun null-fill-p (fill)
  (eql fill +null-fill-req+))

(defmethod print-object ((object fill-req) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (if (null-fill-p object)
        (format stream "none")
        (format stream "x1: ~s y1: ~s x2: ~s y2: ~s"
                (fill-x1 object) (fill-y1 object) (fill-x2 object) (fill-y2 object)))))

(defun make-fill-req (x1 &optional (y1 x1) (x2 x1) (y2 y1))
  (setf x1 (ensure-bound x1)
        y1 (ensure-bound y1)
        x2 (ensure-bound x2)
        y2 (ensure-bound y2))
  (if (and (null-bound-p x1)
           (null-bound-p y1)
           (null-bound-p x2)
           (null-bound-p y2))
      +null-fill-req+
      (make-instance 'fill-req :x1 x1 :y1 y1 :x2 x2 :y2 y2)))

(defun ensure-fill-req (value)
  (typecase value
    (fill-req value)
    (null +null-fill-req+)
    (list (apply #'make-fill-req value))
    (otherwise (make-fill-req value value value value))))

(defun fill-req-combine (fun b1 b2)
  (setf b1 (ensure-fill-req b1))
  (setf b2 (ensure-fill-req b2))
  (make-fill-req (bound-combine fun (fill-x1 b1) (fill-x1 b2))
                 (bound-combine fun (fill-y1 b1) (fill-y1 b2))
                 (bound-combine fun (fill-x2 b1) (fill-x2 b2))
                 (bound-combine fun (fill-y2 b1) (fill-y2 b2))))

(defun fill-req-combine* (fun b1 b2)
  (setf b1 (ensure-fill-req b1))
  (setf b2 (ensure-fill-req b2))
  (make-fill-req (bound-combine* fun (fill-x1 b1) (fill-x1 b2))
                 (bound-combine* fun (fill-y1 b1) (fill-y1 b2))
                 (bound-combine* fun (fill-x2 b1) (fill-x2 b2))
                 (bound-combine* fun (fill-y2 b1) (fill-y2 b2))))

(defun fill-equal (b1 b2)
  (setf b1 (ensure-fill-req b1))
  (setf b2 (ensure-fill-req b2))
  (and (bound-equal (fill-x1 b1) (fill-x1 b2))
       (bound-equal (fill-y1 b1) (fill-y1 b2))
       (bound-equal (fill-x2 b1) (fill-x2 b2))
       (bound-equal (fill-y2 b1) (fill-y2 b2))))

;;; Extras for easy composition with sizes:
(defmethod size-ws ((object fill-req))
  (bound-combine #'+ (fill-x1 object) (fill-x2 object)))

(defmethod size-hs ((object fill-req))
  (bound-combine #'+ (fill-y1 object) (fill-y2 object)))


;;; The standard space requirement. We extend the basic notion convered in the
;;; specification with padding and margins. Moreovoer we add a spedification
;;; about the children layout.

;;; For compatibility with "other" space requirements:
(defmethod space-requirement-measure (space-requirement)
  (multiple-value-bind (ws min-ws max-ws hs min-hs max-hs)
      (space-requirement-components space-requirement)
    (make-size-req (make-bound ws min-ws max-ws)
                   (make-bound hs min-hs max-hs))))

(defmethod space-requirement-padding (space-requirement) +null-fill-req+)
(defmethod space-requirement-margins (space-requirement) +null-fill-req+)

(defmethod bounding-rectangle* ((sr space-requirement))
  (let ((measure (space-requirement-measure sr))
        (padding (space-requirement-padding sr))
        (margins (space-requirement-margins sr)))
    (let* ((x1 (bound-val (fill-x1 margins)))
           (y1 (bound-val (fill-y1 margins)))
           (x2 (+ x1
                  (bound-val (fill-x1 padding))
                  (bound-val (size-ws measure))
                  (bound-val (fill-x2 padding))))
           (y2 (+ x1
                  (bound-val (fill-y1 padding))
                  (bound-val (size-hs measure))
                  (bound-val (fill-y2 padding)))))
      (values x1 y1 x2 y2))))

(defclass standard-space-requirement (space-requirement)
  ((measure :initarg :measure :reader space-requirement-measure
            :documentation "Element requirements")
   (padding :initarg :padding :reader space-requirement-padding
            :documentation "Element padding")
   (margins :initarg :margins :reader space-requirement-margins
            :documentation "Element margins")
   ;; Arranging children.
   #+ (or)
   (spacing :initarg :spacing :reader space-requirement-spacing
            :documentation "Spacing betwen scions")
   #+ (or)
   (situate :initarg :situate :reader space-requirement-situate
            :documentation "Alignment of each scion"))
  (:default-initargs :measure +null-size-req+
                     :padding +null-fill-req+
                     :margins +null-fill-req+))

(macrolet ((def-reader (name reader result)
             `(defmethod ,name ((sr standard-space-requirement))
                (,result (reduce (curry #'bound-combine #'+)
                                 (list (space-requirement-measure sr)
                                       (space-requirement-padding sr)
                                       ;; Margins do not count to area.
                                       #+ (or)
                                       (space-requirement-margins sr))
                                 :key (function ,reader))))))
  (def-reader space-requirement-width      size-ws bound-val)
  (def-reader space-requirement-min-width  size-ws bound-min)
  (def-reader space-requirement-max-width  size-ws bound-max)
  (def-reader space-requirement-height     size-hs bound-val)
  (def-reader space-requirement-min-height size-hs bound-min)
  (def-reader space-requirement-max-height size-hs bound-max))

(defmethod space-requirement-components ((sr space-requirement))
  (values (space-requirement-width sr)
          (space-requirement-min-width sr)
          (space-requirement-max-width sr)
          (space-requirement-height sr)
          (space-requirement-min-height sr)
          (space-requirement-max-height sr)))

;;; Spoiled^HSpilled readers.
(macrolet ((def-reader (name &rest slots)
             `(defmethod ,name ((sr standard-space-requirement))
                (reduce #'slot-value ',slots :initial-value sr)))
           (def-readers (prefix suffix &rest slots)
             (let ((val (intern (format nil "~a-~a" prefix suffix)))
                   (min (intern (format nil "~a-MIN-~a" prefix suffix)))
                   (max (intern (format nil "~a-MAX-~a" prefix suffix))))
               `(progn
                  (def-reader ,val ,@slots val)
                  (def-reader ,min ,@slots min)
                  (def-reader ,max ,@slots max)))))
  (def-readers space-requirement measure-ws measure ws)
  (def-readers space-requirement measure-hs measure hs)
  (def-readers space-requirement padding-x1 padding x1)
  (def-readers space-requirement padding-y1 padding y1)
  (def-readers space-requirement padding-x2 padding x2)
  (def-readers space-requirement padding-y2 padding y2)
  (def-readers space-requirement margins-x1 margins x1)
  (def-readers space-requirement margins-y1 margins y1)
  (def-readers space-requirement margins-x2 margins x2)
  (def-readers space-requirement margins-y2 margins y2))

(defvar +null-space-req+
  (make-instance 'standard-space-requirement))

(defun null-space-req-p (sr)
  (eql sr +null-space-req+))

(defmethod print-object ((object standard-space-requirement) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~%  measure: ~s~%  padding: ~s~%  margins: ~s"
            (space-requirement-measure object)
            (space-requirement-padding object)
            (space-requirement-margins object))))

(defun make-space-requirements (measure &optional padding margins)
  (setf measure (ensure-size-req measure)
        padding (ensure-fill-req padding)
        margins (ensure-fill-req margins))
  (if (and (null-size-p measure)
           (null-fill-p padding)
           (null-fill-p margins))
      +null-space-req+
      (make-instance 'standard-space-requirement :measure measure
                                                 :padding padding
                                                 :margins margins)))

(defun ensure-space-requirements (space)
  (typecase space
    (space-requirement space)
    (null +null-space-req+)
    (list (apply #'make-space-requirements space))
    (real (make-space-requirements space))
    (otherwise
     (with-bounding-rectangle* (x1 y1 :width w :height h) space
       (make-space-requirements (make-size-req w h)
                                +null-fill-req+
                                (make-fill-req x1 y1))))))

(defun fuse-space-requirements (default
                                &key (measure (space-requirement-measure default))
                                     (padding (space-requirement-padding default))
                                     (margins (space-requirement-margins default)))
  (make-space-requirements measure padding margins))

(defun space-requirement-combine (function sr1 sr2)
  (make-space-requirements (size-req-combine function
                                             (space-requirement-measure sr1)
                                             (space-requirement-measure sr2))
                           (fill-req-combine function
                                             (space-requirement-padding sr1)
                                             (space-requirement-padding sr2))
                           (fill-req-combine function
                                             (space-requirement-margins sr1)
                                             (space-requirement-margins sr2))))

;;; The started variant of space-requirement-combine* combines requirements on
;;; per-bound basis. That allows us to "maximize" boxes.
(defun space-requirement-combine* (function sr1 sr2)
  (make-space-requirements (size-req-combine* function
                                              (space-requirement-measure sr1)
                                              (space-requirement-measure sr2))
                           (fill-req-combine* function
                                              (space-requirement-padding sr1)
                                              (space-requirement-padding sr2))
                           (fill-req-combine* function
                                              (space-requirement-margins sr1)
                                              (space-requirement-margins sr2))))

(defun space-requirement+ (sr1 sr2)
  (space-requirement-combine #'+ sr1 sr2))

(defun space-requirement+* (sr &key (width  0) (min-width  0) (max-width  0)
                                    (height 0) (min-height 0) (max-height 0))
  (let ((new-req (make-space-requirement
                  :width  width  :min-width  min-width  :max-width  max-width
                  :height height :min-height min-height :max-height max-height)))
    (space-requirement+ sr new-req)))

;;; McCLIM extension.
(defmethod equals ((a space-requirement) (b space-requirement))
  (space-requirement-equal a b))

(defmethod space-requirement-equal ((sr1 space-requirement)
                                    (sr2 space-requirement))
  (and (size-equal (space-requirement-measure sr1) (space-requirement-measure sr2))
       (fill-equal (space-requirement-padding sr1) (space-requirement-padding sr2))
       (fill-equal (space-requirement-margins sr1) (space-requirement-margins sr2))))

;;; Don't upset the standard.
(defun make-space-requirement
    (&rest args
     &key (width  0) (min-width  0)  (max-width  +fill+)
          (height 0) (min-height 0) (max-height +fill+))
  (declare (ignorable args))
  (let ((measure (make-size-req (make-bound width  min-width  max-width)
                                (make-bound height min-height max-height)))
        (padding +null-fill-req+)
        (margins +null-fill-req+))
    #+ (or)
    (let ((old (apply #'make-legacy-space-requirement args))
          (new (make-space-requirements measure padding margins)))
      (assert (space-requirement-equal old new))
      new)
    (make-space-requirements measure padding margins)))

;;; The almighty constructor.
(defun make-space-requirement*
    (&key
       (measure-ws 0) (min-measure-ws measure-ws) (max-measure-ws measure-ws)
       (measure-hs 0) (min-measure-hs measure-hs) (max-measure-hs measure-hs)
       ;;
       (padding-x1 0) (min-padding-x1 padding-x1) (max-padding-x1 padding-x1)
       (padding-y1 0) (min-padding-y1 padding-y1) (max-padding-y1 padding-y1)
       (padding-x2 0) (min-padding-x2 padding-x2) (max-padding-x2 padding-x2)
       (padding-y2 0) (min-padding-y2 padding-y2) (max-padding-y2 padding-y2)
       ;;
       (margins-x1 0) (min-margins-x1 margins-x1) (max-margins-x1 margins-x1)
       (margins-y1 0) (min-margins-y1 margins-y1) (max-margins-y1 margins-y1)
       (margins-x2 0) (min-margins-x2 margins-x2) (max-margins-x2 margins-x2)
       (margins-y2 0) (min-margins-y2 margins-y2) (max-margins-y2 margins-y2))
  (let ((measure (make-size-req (make-bound measure-ws min-measure-ws max-measure-ws)
                                (make-bound measure-hs min-measure-hs max-measure-hs)))
        (padding (make-fill-req (make-bound padding-x1 min-padding-x1 max-padding-x1)
                                (make-bound padding-y1 min-padding-y1 max-padding-y1)
                                (make-bound padding-x2 min-padding-x2 max-padding-x2)
                                (make-bound padding-y2 min-padding-y2 max-padding-y2)))
        (margins (make-fill-req (make-bound margins-x1 min-margins-x1 max-margins-x1)
                                (make-bound margins-y1 min-margins-y1 max-margins-y1)
                                (make-bound margins-x2 min-margins-x2 max-margins-x2)
                                (make-bound margins-y2 min-margins-y2 max-margins-y2))))
    (make-space-requirements measure padding margins)))

;;; This function is a bit goofy, but the reason for it is to be able to say:
;;;     (fuse-space-requirement base-requirement :min-width 100)
(macrolet ((def-fuse (args &body body)
             (flet ((spill (var-name)
                      (let* ((min-name (intern (format nil "MIN-~a" var-name)))
                             (max-name (intern (format nil "MAX-~a" var-name)))
                             (var-read (intern (format nil "SPACE-REQUIREMENT-~A" var-name)))
                             (min-read (intern (format nil "SPACE-REQUIREMENT-~A" min-name)))
                             (max-read (intern (format nil "SPACE-REQUIREMENT-~A" max-name))))
                        `((,var-name (,var-read default))
                          (,min-name (,min-read default))
                          (,max-name (,max-read default))))))
               (let ((keys (loop for arg in args append (spill arg))))
                 `(defun fuse-space-requirement (default &key ,@keys)
                    ,@body)))))
  (def-fuse (measure-ws measure-hs
             padding-x1 padding-y1 padding-x2 padding-y2
             margins-x1 margins-y1 margins-x2 margins-y2)
      (let ((measure (make-size-req (make-bound measure-ws min-measure-ws max-measure-ws)
                                    (make-bound measure-hs min-measure-hs max-measure-hs)))
            (padding (make-fill-req (make-bound padding-x1 min-padding-x1 max-padding-x1)
                                    (make-bound padding-y1 min-padding-y1 max-padding-y1)
                                    (make-bound padding-x2 min-padding-x2 max-padding-x2)
                                    (make-bound padding-y2 min-padding-y2 max-padding-y2)))
            (margins (make-fill-req (make-bound margins-x1 min-margins-x1 max-margins-x1)
                                    (make-bound margins-y1 min-margins-y1 max-margins-y1)
                                    (make-bound margins-x2 min-margins-x2 max-margins-x2)
                                    (make-bound margins-y2 min-margins-y2 max-margins-y2))))
        (make-space-requirements measure padding margins))))


(defclass legacy-space-requirement (space-requirement)
  ((width      :initform 1
               :initarg :width
               :reader space-requirement-width)
   (max-width  :initform 1
               :initarg :max-width
               :reader space-requirement-max-width)
   (min-width  :initform 1
               :initarg :min-width
               :reader space-requirement-min-width)
   (height     :initform 1
               :initarg :height
               :reader space-requirement-height)
   (max-height :initform 1
               :initarg :max-height
               :reader space-requirement-max-height)
   (min-height :initform 1
               :initarg :min-height
               :reader space-requirement-min-height) ) )

(defmethod print-object ((space legacy-space-requirement) stream)
  (with-slots (width height min-width max-width min-height max-height) space
    (print-unreadable-object (space stream :type t :identity nil)
      (format stream "width: ~S [~S,~S] height: ~S [~S,~S]"
              width
              min-width
              max-width
              height
              min-height
              max-height))))

(defun make-legacy-space-requirement (&key (min-width 0) (min-height 0)
                                           (width min-width) (height min-height)
                                           (max-width +fill+) (max-height +fill+))
  ;; Defensive programming. For instance SPACE-REQUIREMENT-+ may cause
  ;; max-{width,height} to be (+ +fill+ +fill+), what exceeds our biggest
  ;; allowed values. We fix that here.
  (clampf min-width 0 +fill+)
  (clampf max-width 0 +fill+)
  (clampf width min-width  max-width)
  (clampf min-height 0 +fill+)
  (clampf max-height 0 +fill+)
  (clampf height min-height max-height)
  (assert (<= min-width  max-width)  (min-width  max-width))
  (assert (<= min-height max-height) (min-height max-height))
  (make-instance 'legacy-space-requirement
                 :width width
                 :max-width max-width
                 :min-width min-width
                 :height height
                 :max-height max-height
                 :min-height min-height))

(defun make-legacy-space-requirement* (sr &key
                                            (width (space-requirement-width sr))
                                            (min-width (space-requirement-min-width sr))
                                            (max-width (space-requirement-max-width sr))
                                            (height (space-requirement-height sr))
                                            (min-height (space-requirement-min-height sr))
                                            (max-height (space-requirement-max-height sr)))
  (make-legacy-space-requirement
   :width width :min-width min-width :max-width max-width
   :height height :min-height min-height :max-height max-height))

(defmethod space-requirement-components ((space-req legacy-space-requirement))
  (with-slots (width min-width max-width height min-height max-height) space-req
    (values width min-width max-width height min-height max-height)))
