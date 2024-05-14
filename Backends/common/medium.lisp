(in-package #:climi)

(defun draw-text-rotation* (x y toward-x toward-y &optional direction)
  ;; Rounding here is important to ensure a numerical stability of rotation.
  (let* ((x (round-coordinate x))
         (y (round-coordinate y))
         (toward-x (round-coordinate toward-x))
         (toward-y (round-coordinate toward-y))
         (dx (- toward-x x))
         (dy (- toward-y y))
         (angle (ecase (canonical-text-direction direction)
                  ((:left-to-right :right-to-left) (find-angle 1 0 dx dy))
                  ((:top-to-bottom :bottom-to-top) (find-angle 0 1 dx dy)))))
    (make-rotation-transformation* angle x y)))

(defun medium-text-transformation (medium x y toward-x toward-y
                                   &optional direction)
  (if (eq (text-style-unit (medium-text-style medium)) :coordinate)
      (if (ecase (canonical-text-direction direction)
            ((:left-to-right :right-to-left)
             (and (= y toward-y) (< x toward-x)))
            ((:top-to-bottom :bottom-to-top)
             (and (< y toward-y) (= x toward-x))))
          (compose-transformations
           (medium-device-transformation medium)
           (make-translation-transformation x y))
          (compose-transformations
           (compose-transformations
            (medium-device-transformation medium)
            (draw-text-rotation* x y toward-x toward-y direction))
           (make-translation-transformation x y)))
      (with-transformed-positions* ((medium-device-transformation medium)
                                    x y toward-x toward-y)
        (if (ecase (canonical-text-direction direction)
              ((:left-to-right :right-to-left)
               (and (= y toward-y) (< x toward-x)))
              ((:top-to-bottom :bottom-to-top)
               (and (< y toward-y) (= x toward-x))))
            (make-translation-transformation x y)
            (compose-transformations
             (draw-text-rotation* x y toward-x toward-y direction)
             (make-translation-transformation x y))))))

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

(defun canonical-text-direction (direction)
  (ecase direction
    ((nil t)
     :left-to-right)
    ((:left-to-right :right-to-left :top-to-bottom :bottom-to-top)
     direction)))
