;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2024 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; A demo for testing DRAW-TEXT. Contains embedded gadgets.
;;;

;;; FIXME (WITH-OUTPUT-AS-GADGET TEXT-FIELD) is acting w/o incremental-redisplay.
;;; FIXME (WITH-OUTPUT-AS-GADGET TEXT-FIELD) cursor is reset on redisplay.

(in-package #:clim-demo)

(defparameter *canvas* (make-rectangle* 50 220 1230 680))

(defparameter *ltr-text* "A quick brown fox jumps over the lazy dog.")
(defparameter *rtl-text* "א  בְּרֵאשִׁית, בָּרָא אֱלֹהִים, אֵת הַשָּׁמַיִם, וְאֵת הָאָרֶץ.") ; gensis

(defparameter *ttb-text* "あなたは美しいです。あなたは素晴らしいです。") ;jp
(defparameter *ttb-text* "国际化活动") ;ch

;;; Bidirectional text with the initial direction left-to-right without and with
;;; right-to-left mark (rlm):
(defparameter *bidi-ltr* "I enjoyed staying -- באמת! -- at his house.")
(defparameter *bidi-ltr+rlm* "I enjoyed staying -- באמת!‏ -- at his house.")

;;; Bidirectional text with the initial direction right-to-left without and with
;;; left-to-right mark (lrm):
(defparameter *bidi-rtl* "لغة C++ هي لغة برمجة تستخدم...")
(defparameter *bidi-rtl+lrm* "لغة C++‎ هي لغة برمجة تستخدم...")

(defparameter *neutral* "█0123456789")

(define-application-frame draw-text-test ()
  ((coords :accessor coords :initform '(400 400 600 400))
   (increase :accessor increase :initform 1)
   (transf-1 :accessor transf-1 :initform +identity-transformation+)
   (transf-2 :accessor transf-2 :initform +identity-transformation+)
   ;;
   (text :accessor text :initform *neutral*)
   (align-x :accessor align-x :initform :left)
   (align-y :accessor align-y :initform :baseline)
   (direction :accessor transform-glyphs :initform :left-to-right))
  (:menu-bar nil)
  (:reinitialize-frames t)
  (:pane :application :display-function #'display
         :incremental-redisplay t
         :min-width 1280 :min-height 720
         :text-margins '(:left 50 :top 25)
         :scroll-bars nil :borders nil))

(defun transf (frame)
  (compose-transformations (transf-1 frame) (transf-2 frame)))

(define-draw-text-test-command com-set-baseline
    ((coords 'sequence))
  (with-application-frame (frame)
    (setf (coords frame) (untransform-coordinates (transf frame) coords))))

(define-draw-text-test-command com-set-rotation
    ((transf 'transformation))
  (with-application-frame (frame)
    (setf (transf-2 frame) transf)))

(define-draw-text-test-command com-increase
    ((signum 'blank-area :gesture :scroll-up))
  (declare (ignore signum))
  (with-application-frame (frame)
    (when (zerop (incf (increase frame) 1/10))
      (incf (increase frame) 1/10))
    (let ((inc (increase frame)))
      (with-bounding-rectangle* (:center-x cx :center-y cy) *canvas*
        (setf (transf-1 frame)
              (make-scaling-transformation* inc inc cx cy))))))

(define-draw-text-test-command com-decrease
    ((signum 'blank-area :gesture :scroll-down))
  (declare (ignore signum))
  (with-application-frame (frame)
    (when (zerop (decf (increase frame) 1/10))
      (decf (increase frame) 1/10))
    (let ((inc (increase frame)))
      (with-bounding-rectangle* (:center-x cx :center-y cy) *canvas*
        (setf (transf-1 frame)
              (make-scaling-transformation* inc inc cx cy))))))

(define-draw-text-test-command com-set-text
    ((text 'string))
  (with-application-frame (frame)
    (setf (text frame) text)))

(define-draw-text-test-command com-set-align-x
    ((align-x '(member :left :center :right)))
  (with-application-frame (frame)
    (setf (align-x frame) align-x)))

(define-draw-text-test-command com-set-align-y
    ((align-y '(member :baseline :top :center :bottom)))
  (with-application-frame (frame)
    (setf (align-y frame) align-y)))

(define-draw-text-test-command com-set-transform-glyphs
    ((direction '(member :left-to-right :right-to-left :top-to-bottom :bottom-to-top)))
  (with-application-frame (frame)
    (setf (transform-glyphs frame) direction)))


(defun untransform-coordinates (transformation coords)
  (let ((transf (invert-transformation transformation)))
   (coerce (climi::transform-positions transf coords) 'list)))

(defun transform-coordinates (transformation coords)
  (coerce (climi::transform-positions transformation coords) 'list))

(defun draw-string (frame stream string x0 y0 x1 y1)
  (draw-text* stream string x0 y0 :toward-x x1 :toward-y y1
                                  :align-x (align-x frame)
                                  :align-y (align-y frame)
                                  :text-size :large
                                  :text-family :sans-serif
                                  :transform-glyphs (transform-glyphs frame))
  (draw-arrow* stream x0 y0 x1 y1 :ink (compose-in +dark-red+ (make-opacity .5))
                                  :line-thickness 3
                                  :line-dashes nil
                                  :head-filled t :head-length 20
                                  :head-width 10))

(defun draw-compas (frame stream ink cx cy)
  (declare (ignore frame))
  (draw-circle* stream cx cy 50 :filled nil :ink ink :line-thickness 3)
  (draw-arrow* stream cx cy cx (- cy 50) :ink (compose-in ink (make-opacity .3))
                                         :line-thickness 3
                                         :head-filled t :head-length 20
                                         :head-width 10))

(defun repaint-canvas (frame stream string transf coords)
  (with-bounding-rectangle* (:center-x cx :center-y cy) *canvas*
    (draw-design stream *canvas* :ink +grey+)
    ;(draw-compas stream +dark-red+ cx cy)
    (with-drawing-options (stream :clipping-region *canvas*
                                  :line-dashes t)
      (with-drawing-options (stream :transformation transf)
        (draw-compas frame stream +dark-blue+ cx cy)
        (apply #'draw-string frame stream string coords)))))

(defun drag-test (x y) (region-contains-position-p *canvas* x y))

(defun draw-drag-baseline (frame from stream x0 y0 x1 y1 state)
  (declare (ignore from state))
  (with-output-recording-options (stream :draw t :record nil)
    (let ((transf (transf frame))
          (coords (list x0 y0 x1 y1)))
      (repaint-canvas frame stream (text frame) transf
                      (untransform-coordinates transf coords)))))

(define-drag-and-drop-translator drag-baseline
    (blank-area command blank-area draw-text-test
                :menu nil
                :feedback draw-drag-baseline
                :tester ((object x y) (drag-test x y))
                :destination-tester ((object x y) (drag-test x y)))
    (object destination-object)
  (let ((coords (list (pointer-event-x object)
                      (pointer-event-y object)
                      (pointer-event-x destination-object)
                      (pointer-event-y destination-object))))
    `(com-set-baseline ,coords)))

(defun compute-rotation (x0 y0 x1 y1)
  (let* ((dx (- x1 x0))
         (dy (- y1 y0))
         (th (climi::find-angle 0 -50 dx dy)))
    (make-rotation-transformation* th x0 y0)))

(defun draw-drag-rotation (frame from stream x0 y0 x1 y1 state)
  (declare (ignore x0 y0 from))
  (with-output-recording-options (stream :draw t :record nil)
    (ecase state
      (:unhighlight)
      (:highlight
       (with-drawing-options (stream :clipping-region *canvas*)
         (with-bounding-rectangle* (:center-x cx :center-y cy) *canvas*
           (let ((transf (compute-rotation cx cy x1 y1))
                 (coords (coords frame)))
             (repaint-canvas frame stream (text frame)
                             (compose-transformations (transf-1 frame) transf)
                             coords))))))))

(define-drag-and-drop-translator drag-rotation
    (blank-area command blank-area draw-text-test
                :gesture :menu
                :menu nil
                :feedback draw-drag-rotation
                :tester ((object x y) (drag-test x y))
                :destination-tester ((object x y) (drag-test x y)))
    (object destination-object)
  (with-bounding-rectangle* (:center-x cx :center-y cy) *canvas*
    (let ((x1 (pointer-event-x destination-object))
          (y1 (pointer-event-y destination-object)))
      `(com-set-rotation ,(compute-rotation cx cy x1 y1)))))

(defmethod display ((frame draw-text-test) stream)
  (format stream "Change the baseline with the pointer left button.~%")
  (format stream "Change the rotation with the pointer right button.~%")
  (format stream "Change the scaling with the pointer scroller [~,1f].~%" (increase frame))
  (terpri stream)
  (formatting-item-list (stream :n-columns 3)
    (formatting-cell (stream)
      (with-output-as-gadget (stream)
        (declare (ignorable stream))
        (labelling (:label "ALIGN-X" :background +white+)
          (make-pane 'option-pane :items '(:left :center :right)
                                  :value (align-x frame)
                                  :value-changed-callback
                                  (lambda (g v)
                                    (declare (ignore g))
                                    (execute-frame-command frame `(com-set-align-x ,v)))))))
    (formatting-cell (stream)
      (with-output-as-gadget (stream)
        (declare (ignorable stream))
        (labelling (:label "ALIGN-Y" :background +white+)
          (make-pane 'option-pane :items '(:baseline :top :center :bottom)
                                  :value (align-y frame)
                                  :value-changed-callback
                                  (lambda (g v)
                                    (declare (ignore g))
                                    (execute-frame-command frame `(com-set-align-y ,v)))))))
    (formatting-cell (stream)
      (with-output-as-gadget (stream)
        (declare (ignorable stream))
        (labelling (:label "TRANSFORM-GLYPHS" :background +white+)
          (make-pane 'option-pane :items '(:left-to-right :right-to-left :top-to-bottom :bottom-to-top)
                                  :value (transform-glyphs frame)
                                  :value-changed-callback
                                  (lambda (g v)
                                    (declare (ignore g))
                                    (execute-frame-command frame `(com-set-transform-glyphs ,v))))))))
  (multiple-value-bind (x0 y0) (stream-cursor-initial-position stream)
    (declare (ignore x0))
    (setf (stream-cursor-position stream) (values 660 y0))
    (updating-output (stream :cache-value t :cache-test (constantly t))
      (with-output-as-gadget (stream)
        (declare (ignorable stream))
        (labelling (:label "Text" :background +white+)
          (make-pane 'text-editor-pane
                     :value (text frame) :width 500 :nlines 6
                     :value-changed-callback
                     (lambda (g v)
                       (declare (ignore g))
                       (execute-frame-command frame `(com-set-text ,v))))))))
  (terpri stream)
  (repaint-canvas frame stream (text frame) (transf frame) (coords frame)))


