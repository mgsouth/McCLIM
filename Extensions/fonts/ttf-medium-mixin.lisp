(in-package #:mcclim-truetype)

(defclass ttf-medium-mixin ()
  ()
  (:documentation "Mixed in when the medium text-style-mapping returns
a font implementing the protocol defined below."))

(defmethod text-style-ascent (text-style (medium ttf-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (font-ascent font)))

(defmethod text-style-descent (text-style (medium ttf-medium-mixin))
  (let ((font (text-style-mapping (port medium) text-style)))
    (font-descent font)))

(defmethod text-style-character-width (text-style (medium ttf-medium-mixin) char)
  (let* ((font (text-style-mapping (port medium) text-style))
         (info (font-glyph-info font (char-code char) :left-to-right)))
    (abs (- (glyph-info-advance-dx info) (glyph-info-origin-x info)))))

(defmethod text-size ((medium ttf-medium-mixin) string &key text-style (start 0) end)
  (setf string (string string)
        end (or end (length string)))
  (when (>= start end)
    (return-from text-size
      (values 0 0 0 0 (text-style-ascent text-style medium))))
  (let* ((font (text-style-mapping (port medium)
                                   (merge-text-styles
                                    text-style
                                    (medium-merged-text-style medium))))
         (ascent (font-ascent font))
         (line-height (+ ascent (font-descent font))))
    (multiple-value-bind (xmin ymin xmax ymax origin-x origin-y)
        (line-bbox font string start end)
      (declare (ignore xmin ymin xmax ymax))
      (values origin-x (+ origin-y line-height)
              origin-x origin-y
              ascent))))

;;; A fallback drawing routine using the function MEDIUM-DRAW-POLYGON*.
(defmethod medium-draw-text* ((medium ttf-medium-mixin) string x y start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (let* ((line-dir (climi::medium-line-direction))
         (rotation (climi::draw-text-rotation* x y toward-x toward-y line-dir))
         (scaling (make-scaling-transformation* 1 -1 x y))
         (combined (compose-transformations rotation scaling)))
    (multiple-value-bind (width height cursor-dx cursor-dy ascent)
        (text-size medium string :start start :end end)
      (declare (ignore cursor-dx cursor-dy))
      (let ((dx (ecase align-x
                  (:left   0)
                  (:center (- (/ width 2)))
                  (:right  (- width))))
            (dy (- (ecase align-y
                     (:baseline  0)
                     (:top       ascent)
                     (:center    (- ascent (/ height 2)))
                     (:bottom    (- ascent height))))))
        (incf x dx)
        (incf y dy)
        (setf end (if (null end)
                      (length string)
                      (min end (length string))))))
    (with-drawing-options (medium :transformation combined)
      (loop with font = (text-style-mapping (port medium) (medium-text-style medium))
            with loader = (zpb-ttf-font-loader (clime:font-face font))
            with units->pixels = (slot-value font 'units->pixels)
            for glyph     = nil then (font-glyph-info font code line-dir)
            for current-x = x then (+ current-x (glyph-info-advance-dx glyph))
            for current-y = y then (+ current-y (glyph-info-advance-dy glyph))
            for code across (string-glyph-codes string :start start :end end)
            for char = (glyph-code-char code)
            for glyf = (zpb-ttf:find-glyph char loader)
            do (climi::collect (polygons)
                 (zpb-ttf:do-contours (contour glyf)
                   (climi::collect (result)
                     (labels ((collect-coords (&rest coords)
                                (climi::do-sequence ((x y) coords)
                                  (result (+ current-x (* units->pixels x))
                                          (+ current-y (* units->pixels y)))))
                              (process-segment (p0 p1 p2)
                                (multiple-value-bind (x0 y0 x1 y1 x2 y2 x3 y3)
                                    (climi::bezier-segment/quadric-to-cubic (zpb-ttf:x p0) (zpb-ttf:y p0)
                                                                            (zpb-ttf:x p1) (zpb-ttf:y p1)
                                                                            (zpb-ttf:x p2) (zpb-ttf:y p2))
                                  (apply #'collect-coords
                                         (climi::polygonalize-bezigon (list x0 y0 x1 y1 x2 y2 x3 y3)))))
                              (process-contour (contour)
                                (zpb-ttf:do-contour-segments (p0 p1 p2) contour
                                  (if (null p1)
                                      (collect-coords (zpb-ttf:x p0) (zpb-ttf:y p0)
                                                      (zpb-ttf:x p2) (zpb-ttf:y p2))
                                      (process-segment p0 p1 p2)))))
                       (process-contour contour)
                       (polygons (result)))))
                 (map-over-region-set-regions
                  (lambda (polygon) (draw-design medium polygon))
                  (let ((splits (climi::polygon-op-inner*
                                 (loop for coords in (polygons)
                                       for polygon = (make-polygon* coords)
                                       appending (climi::polygon->pg-edges polygon nil))
                                 :non-zero)))
                    (climi::pg-splitters->polygons splits))))))))
