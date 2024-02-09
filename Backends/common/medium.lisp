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

;; For multiline text alignment may change the bbox. For instance longest line
;; may start with a character with left-bearing=0 and shorter line starts with a
;; character which has left-bearing=-10. If text is left-aligned then bbox
;; starts from coordinate x=-10, but if text is right-aligned it is x=0. This
;; mixin provides decent adjustment for alignment for simpler algorithms. Method
;; is not pixel-perfect hence it should be used sparingly for early prototypes.
(defclass approx-bbox-medium-mixin () ()
  (:documentation "Adjusts bounding rectangle to alignment with a decent heuristic."))

(defmethod text-bounding-rectangle* :around
    ((medium approx-bbox-medium-mixin) string &key text-style start end
                                                (align-x :left)
                                                (align-y :baseline)
                                                (direction :ltr))
  (declare (ignore start end direction))
  (multiple-value-bind (left top right bottom) (call-next-method)
    (let ((width (- right left)))
      (ecase align-x
        (:left)
        (:right
         (decf left width)
         (decf right width))
        (:center
         (decf left (/ width 2.0s0))
         (decf right (/ width 2.0s0)))))
    (let ((ascent (text-style-ascent text-style medium))
          (descent (text-style-descent text-style medium))
          (height (- bottom top)))
      (ecase align-y
        (:baseline)
        (:top
         (setf top (- ascent (abs top))
               bottom (+ top height)))
        (:bottom
         (decf top bottom)
         (decf bottom bottom))
        (:center
         (setf top (- (/ height 2.0s0)))
         (setf bottom (/ height 2.0s0)))))
    (values left top right bottom)))
