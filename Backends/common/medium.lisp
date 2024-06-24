(in-package #:climi)

(defun draw-text-rotation* (x y toward-x toward-y line-direction)
  ;; Rounding here is important to ensure a numerical stability of rotation.
  (let* ((x (round-coordinate x))
         (y (round-coordinate y))
         (toward-x (round-coordinate toward-x))
         (toward-y (round-coordinate toward-y))
         (dx (- toward-x x))
         (dy (- toward-y y))
         (angle (find-angle 1 0 dx dy)))
    (make-rotation-transformation* angle x y)))

(defun draw-text-transformation* (medium x0 y0 x1 y1 line-direction transform-glyphs)
  (flet ((text-transformation (fx fy tx ty)
           (if (and (= fy ty) (< fx tx))
               (make-translation-transformation fx fy)
               (compose-transformations
                (draw-text-rotation* fx fy tx ty line-direction)
                (make-translation-transformation fx fy)))))
    (if transform-glyphs
        (compose-transformations (medium-transformation medium)
                                 (text-transformation x0 y0 x1 y1))
        (with-transformed-positions* ((medium-transformation medium) x0 y0 x1 y1)
          (text-transformation x0 y0 x1 y1)))))

(defun text-alignment-offset (xmin ymin xmax ymax align-x align-y)
  (let ((xcenter (/ (+ xmax xmin) 2.0))
        (ycenter (/ (+ ymax ymin) 2.0)))
    (values
     (ecase align-x
       (:baseline 0)
       (:left     (- 0 xmin))
       (:right    (- 0 xmax))
       (:center   (- 0 xcenter)))
     (ecase align-y
       (:baseline 0)
       (:top      (- 0 ymin))
       (:bottom   (- 0 ymax))
       (:center   (- 0 ycenter))))))


(defclass multiline-medium-mixin (medium) ())

(defmethod text-size :around ((medium multiline-medium-mixin) string
                              &key text-style start end)
  (orf start 0)
  (orf end (length string))
  (let ((block-ws 0)
        (block-hs 0)
        (line-breaks 0))
    (flet ((handle-line (idx0 idx1)
             (multiple-value-bind (ws hs dx dy baseline)
                 (call-next-method medium string :text-style text-style
                                                 :start idx0 :end idx1)
               (declare (ignore dx dy baseline))
               (incf line-breaks)
               (maxf block-ws ws)
               (incf block-hs hs)))
           (handle-last (idx0)
             (multiple-value-bind (ws hs dx dy baseline)
                 (call-next-method medium string :text-style text-style
                                                 :start idx0 :end end)
               (when (plusp line-breaks)
                 (ecase (medium-page-direction medium)
                   (:top-to-bottom (setf dy (+ block-hs)))
                   (:bottom-to-top (setf dy (- block-hs)))
                   (:left-to-right (setf dy (- block-hs)))
                   (:right-to-left (setf dy (+ block-hs)))))
               (maxf block-ws ws)
               (incf block-hs hs)
               (values block-ws
                       block-hs
                       dx dy
                       (- baseline dy)))))
      (loop for idx0 = start then (1+ idx1)
            for idx1 = (position #\newline string :start idx0 :end end)
            until (null idx1)
            do (handle-line idx0 idx1)
            finally
               (return (handle-last idx0))))))

(defmethod medium-draw-text* :around ((medium multiline-medium-mixin) string x y
                                      start end
                                      align-x align-y toward-x toward-y
                                      transform-glyphs)
  (let* ((line-direction (medium-line-direction medium))
         (base-transf (draw-text-rotation* x y toward-x toward-y line-direction))
         (transformation (if transform-glyphs
                             base-transf
                             (compose-transformations
                              base-transf
                              (invert-transformation (medium-device-transformation medium))))))
    (loop for idx0 = start then (1+ idx1)
          for idx1 = (position #\newline string :start idx0 :end end)
          do (call-next-method medium string x y idx0 (or idx1 end)
                               align-x align-y toward-x toward-y
                               transform-glyphs)
             (multiple-value-bind (w h dx dy baseline)
                 (if (null idx1)
                     (text-size medium string :start idx0 :end end) ; vvv \n included
                     (text-size medium string :start idx0 :end (1+ idx1)))
               (declare (ignore w h baseline))
               (with-transformed-distance (transformation dx dy)
                 (incf x dx) (incf toward-x dx)
                 (incf y dy) (incf toward-y dy)))
          while idx1)))
