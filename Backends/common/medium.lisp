(in-package #:climi)

(defun draw-text-rotation* (x y toward-x toward-y)
  ;; Rounding here is important to ensure a numerical stability of rotation.
  (let* ((x (round-coordinate x))
         (y (round-coordinate y))
         (toward-x (round-coordinate toward-x))
         (toward-y (round-coordinate toward-y))
         (dx (- toward-x x))
         (dy (- toward-y y))
         (angle (find-angle 1 0 dx dy)))
    (make-rotation-transformation* angle x y)))

(defun medium-text-transformation (medium x y toward-x toward-y)
  (if (and (= y toward-y) (< x toward-x))
      (medium-device-transformation medium)
      (compose-transformations (medium-device-transformation medium)
                               (draw-text-rotation* x y toward-x toward-y))))

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
