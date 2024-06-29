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
    (make-rotation-transformation* angle 0 0)))

(defun draw-text-transformation* (medium x0 y0 x1 y1 transform-glyphs)
  (flet ((text-transformation (fx fy tx ty)
           (if (and (= fy ty) (< fx tx))
               (make-translation-transformation fx fy)
               (compose-transformations
                (make-translation-transformation fx fy)
                (draw-text-rotation* fx fy tx ty)))))
    (if transform-glyphs
        (compose-transformations (medium-transformation medium)
                                 (text-transformation x0 y0 x1 y1))
        (with-transformed-positions* ((medium-transformation medium) x0 y0 x1 y1)
          (text-transformation x0 y0 x1 y1)))))


(defclass multiline-medium-mixin (medium) ())

(defmethod text-bounding-rectangle* :around ((medium multiline-medium-mixin) string
                                             &key text-style start end)
  (orf start 0)
  (orf end (length string))
  (let ((total-xmin 0)
        (total-ymin 0)
        (total-xmax 0)
        (total-ymax 0)
        (current-dx 0)
        (current-dy 0))
    (flet ((handle-line (idx0 idx1)
             (multiple-value-bind (xmin ymin xmax ymax)
                 (call-next-method medium string :text-style text-style
                                                 :start idx0 :end idx1)
               (minf total-xmin (+ current-dx xmin))
               (minf total-ymin (+ current-dy ymin))
               (maxf total-xmax (+ current-dx xmax))
               (maxf total-ymax (+ current-dy ymax))
               (ecase (medium-page-direction medium)
                 ((:top-to-bottom :right-to-left)
                  (incf current-dy (- ymax ymin)))
                 ((:bottom-to-top :left-to-right)
                  (decf current-dy (- ymax ymin))))))
           (handle-last (idx0)
             (multiple-value-bind (xmin ymin xmax ymax)
                 (call-next-method medium string :text-style text-style
                                                 :start idx0 :end end)
               (minf total-xmin (+ current-dx xmin))
               (minf total-ymin (+ current-dy ymin))
               (maxf total-xmax (+ current-dx xmax))
               (maxf total-ymax (+ current-dy ymax))
               (values total-xmin total-ymin total-xmax total-ymax))))
      (loop for idx0 = start then (1+ idx1)
            for idx1 = (position #\newline string :start idx0 :end end)
            until (null idx1)
            do (handle-line idx0 idx1)
            finally
               (return (handle-last idx0))))))

(defmethod text-size :around ((medium multiline-medium-mixin) string
                              &key text-style start end)
  (orf start 0)
  (orf end (length string))
  (let ((block-ws 0)
        (block-hs 0)
        (current-dy 0))
    (flet ((handle-line (idx0 idx1)
             (multiple-value-bind (ws hs dx dy baseline)
                 (call-next-method medium string :text-style text-style
                                                 :start idx0 :end idx1)
               (declare (ignore dx dy baseline))
               (maxf block-ws ws)
               (incf block-hs hs)
               (ecase (medium-page-direction medium)
                 ((:top-to-bottom :right-to-left)
                  (incf current-dy hs))
                 ((:bottom-to-top :left-to-right)
                  (format *debug-io* "decf by ~s~%" hs)
                  (decf current-dy hs)))))
           (handle-last (idx0)
             (multiple-value-bind (ws hs dx dy baseline)
                 (call-next-method medium string :text-style text-style
                                                 :start idx0 :end end)
               (declare (ignore dy))
               (maxf block-ws ws)
               (incf block-hs hs)
               (values block-ws
                       block-hs
                       dx current-dy
                       baseline))))
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
  (let (text-transf dx dy)
    (labels ((text-transf ()
               (or text-transf
                   (let ((base-transf (medium-device-transformation medium)))
                     (if transform-glyphs
                         (draw-text-rotation* x y toward-x toward-y)
                         (with-transformed-positions*
                             (base-transf x y toward-x toward-y)
                           (compose-transformations
                            (draw-text-rotation* x y toward-x toward-y)
                            (invert-transformation base-transf)))))))
             (position-box ()
               (multiple-value-bind (xmin ymin xmax ymax)
                   (text-bounding-rectangle* medium string :start start :end end)
                 (let* ((xmid (/ (+ xmin xmax) 2))
                        (ymid (/ (+ ymin ymax) 2))
                        (dx (ecase align-x
                              (:baseline 0)
                              (:left   (- xmin))
                              (:right  (- xmax))
                              (:center (- xmid))))
                        (dy (ecase align-y
                              (:baseline 0)
                              (:top    (- ymin))
                              (:bottom (- ymax))
                              (:center (- ymid)))))
                   (with-transformed-distance ((text-transf) dx dy)
                     (incf x dx) (incf toward-x dx)
                     (incf y dy) (incf toward-y dy)))
                 (setf align-x :baseline align-y :baseline)))
             (advance-line ()
               (unless dx
                 (multiple-value-bind (w h line-dx line-dy baseline)
                     (text-size medium #.(format nil " ~%"))
                   ;;      ;; IDX1 is a position of #\newline hence #'1+
                   #+ (or) (text-size medium string :start idx0 :end (1+ idx1))
                   (declare (ignore w h baseline))
                   (multiple-value-setq (dx dy)
                     (transform-distance (text-transf) line-dx line-dy))))
               (incf x dx) (incf toward-x dx)
               (incf y dy) (incf toward-y dy)))
      (when (and (or (not (eq align-x :baseline))
                     (not (eq align-y :baseline)))
                 (find #\newline string :start start :end end))
        (position-box))
      (loop for idx0 = start then (1+ idx1)
            for idx1 = (position #\newline string :start idx0 :end end)
            do (call-next-method medium string x y idx0 (or idx1 end)
                                 align-x align-y toward-x toward-y
                                 transform-glyphs)
            while idx1
            do (advance-line)))))
