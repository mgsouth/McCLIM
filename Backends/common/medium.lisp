(in-package #:climi)

(defun draw-text-rotation* (x y toward-x toward-y line-direction)
  ;; Rounding here is important to ensure a numerical stability of rotation.
  (let* ((x (round-coordinate x))
         (y (round-coordinate y))
         (toward-x (round-coordinate toward-x))
         (toward-y (round-coordinate toward-y))
         (dx (- toward-x x))
         (dy (- toward-y y))
         (angle (ecase line-direction
                  ((:left-to-right :right-to-left) (find-angle 1 0 dx dy))
                  ((:top-to-bottom :bottom-to-top) (find-angle 0 1 dx dy)))))
    (make-rotation-transformation* angle x y)))

(defun draw-text-line-advance (x y toward-x toward-y line-direction)
  ;; Rounding here is important to ensure a numerical stability of rotation.
  (let* ((x (round-coordinate x))
         (y (round-coordinate y))
         (toward-x (round-coordinate toward-x))
         (toward-y (round-coordinate toward-y))
         (dx (- toward-x x))
         (dy (- toward-y y))
         (angle (ecase line-direction
                  ((:left-to-right :right-to-left) (find-angle 1 0 dx dy))
                  ((:top-to-bottom :bottom-to-top) (find-angle 0 1 dx dy)))))
    (make-rotation-transformation* angle x y)))

(defun medium-text-transformation (medium x0 y0 x1 y1 line-direction)
  (flet ((text-transformation (fx fy tx ty)
           (if (ecase line-direction
                 ((:left-to-right :right-to-left) (and (= fy ty) (< fx tx)))
                 ((:top-to-bottom :bottom-to-top) (and (< fy ty) (= fx tx))))
               (make-translation-transformation fx fy)
               (compose-transformations
                (draw-text-rotation* fx fy tx ty line-direction)
                (make-translation-transformation fx fy)))))
    (if (eq (text-style-unit (medium-text-style medium)) :coordinate)
        (compose-transformations (medium-device-transformation medium)
                                 (text-transformation x0 y0 x1 y1))
        (with-transformed-positions* ((medium-transformation medium) x0 y0 x1 y1)
          (compose-transformations (medium-native-transformation medium)
                                   (text-transformation x0 y0 x1 y1))))))

(defun medium-text-transformation* (medium x0 y0 x1 y1 line-direction)
  (flet ((text-transformation (fx fy tx ty)
           (if (ecase line-direction
                 ((:left-to-right :right-to-left) (and (= fy ty) (< fx tx)))
                 ((:top-to-bottom :bottom-to-top) (and (< fy ty) (= fx tx))))
               (make-translation-transformation fx fy)
               (compose-transformations
                (draw-text-rotation* fx fy tx ty line-direction)
                (make-translation-transformation fx fy)))))
    (if (eq (text-style-unit (medium-text-style medium)) :coordinate)
        (compose-transformations (medium-transformation medium)
                                 (text-transformation x0 y0 x1 y1))
        (with-transformed-positions* ((medium-transformation medium) x0 y0 x1 y1)
          (text-transformation x0 y0 x1 y1)))))

;;; The baseline is assumed to be at y=0.
(defun align-bounding-rectangle (xmin ymin xmax ymax align-x align-y)
  (ecase align-x
    (:left)
    (:center
     (let ((hcenter (/ (- xmax xmin) 2)))
       (setf xmin (- hcenter))
       (setf xmax (+ hcenter))))
    (:right
     (let ((hsize (- xmax xmin)))
       (setf xmin (- hsize))
       (setf xmax 0))))
  (ecase align-y
    (:top
     (let ((vsize (- ymax ymin)))
       (setf ymin 0)
       (setf ymax vsize)))
    (:center
     (let ((vcenter (/ (- ymax ymin) 2)))
       (setf ymin (- vcenter))
       (setf ymax (+ vcenter))))
    (:baseline)
    (:bottom
     (let ((vsize (- ymax ymin)))
       (setf ymin (- vsize))
       (setf ymax 0))))
  (values xmin ymin xmax ymax))


(defclass multiline-medium-mixin (medium) ())

(defmethod medium-draw-text* :around ((medium multiline-medium-mixin) string x y
                                      start end
                                      align-x align-y
                                      toward-x toward-y transform-glyphs)
  (loop with page-dir = (medium-page-direction medium)
        with line-dir = (medium-line-direction medium)
        with trn = (draw-text-rotation* x y toward-x toward-y line-dir)
        for idx0 = start then (1+ idx1)
        for idx1 = (position #\newline string :start idx0 :end end)
        do (call-next-method medium string x y idx0 idx1
                             align-x align-y toward-x toward-y
                             transform-glyphs)
           (multiple-value-bind (w h dx dy)
               (if (null idx1)
                   (text-size medium string :start idx0) ; v \n included
                   (text-size medium string :start idx0 :end (1+ idx1)))
             (declare (ignore w dx dy))
             ;; FIXME text-size should recognize newlines from get-go and this
             ;; atracious ecase should vanish. -- jd 2024-06-08
             #+ (or) (progn (incf x dx) (incf toward-x dx)
                            (incf y dy) (incf toward-y dy))
             (multiple-value-bind (w h)
                 (ecase line-dir
                   ((:left-to-right :right-to-left)
                    (ecase page-dir
                      ((:top-to-bottom :left-to-right)
                       (transform-distance trn 0 h))
                      ((:bottom-to-top :right-to-left)
                       (transform-distance trn 0 (- h)))))
                   ((:top-to-bottom :bottom-to-top)
                    (ecase page-dir
                      ((:bottom-to-top :left-to-right)
                       (transform-distance trn h 0))
                      ((:top-to-bottom :right-to-left)
                       (transform-distance trn (- h) 0)))))
               (incf x w) (incf toward-x w)
               (incf y h) (incf toward-y h)))
        while idx1))
