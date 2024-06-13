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
      (values 0
              (text-style-height text-style medium)
              0
              0
              (text-style-ascent text-style medium))))
  (let* ((font (text-style-mapping (port medium)
                                   (merge-text-styles
                                    text-style
                                    (medium-merged-text-style medium))))
         (baseline (font-ascent font))
         (line-height (+ baseline (font-descent font))))
    (multiple-value-bind (cursor-dx cursor-dy)
        (line-advance medium font string start end)
      (values (abs cursor-dx) line-height
              cursor-dx cursor-dy
              baseline))))

;;; Alternative version could take pixmaps from font-glyph-info, convert them to
;;; patterns and call draw-design on that. This would be faster than consing new
;;; polygons each time, but our goal is to go straight from paths.
;;;
;;; ORIGIN-X and ORIGIN-Y are relative to glyph pixmap, while polygons are
;;; specified relative to the drawing origin, that's why we compute offset.
;;; Compare with the function GLYPH-INFO-ADVANCE.
(defun naive-render-composite-glyphs (font glyph-codes
                                      medium x y transformation direction)
  (flet ((compute-offset-x (info)
           (ecase direction
             (:left-to-right 0)
             (:right-to-left (- 0
                                (glyph-info-width info)
                                (glyph-info-left info)))
             (:top-to-bottom (- 0
                                (/ (glyph-info-width info) 2.0)
                                (glyph-info-left info)))
             (:bottom-to-top (- 0
                                (/ (glyph-info-width info) 2.0)
                                (glyph-info-left info)))))
         (compute-offset-y (info)
           (declare (ignore info))
           (ecase direction
             (:left-to-right 0)
             (:right-to-left 0)
             (:top-to-bottom (font-ascent font))
             (:bottom-to-top (- 0 (font-descent font))))))
   (with-drawing-options (medium :transformation transformation)
     (loop with loader = (zpb-ttf-font-loader (clime:font-face font))
           with units->pixels = (slot-value font 'units->pixels)
           for x0 = x then (+ x0 (glyph-info-advance-dx info))
           for y0 = y then (+ y0 (glyph-info-advance-dy info))
           for code across glyph-codes
           for info = (font-glyph-info font code direction)
           for char = (glyph-code-char code)
           for glyf = (zpb-ttf:find-glyph char loader)
           ;; The glyph origin may be different than (0 0). Moreover glyphs are
           ;; specified in graphics coordinate system.
           for x1 = (+ x0 (compute-offset-x info))
           for y1 = (+ y0 (compute-offset-y info))
           for updown = (make-scaling-transformation* 1 -1 x1 y1)
           do (climi::collect (polygons)
                (zpb-ttf:do-contours (contour glyf)
                  (climi::collect (result)
                    (labels ((collect-coords (&rest coords)
                               (climi::do-sequence ((px py) coords)
                                 (result (+ x1 (* units->pixels px))
                                         (+ y1 (* units->pixels py)))))
                             (process-segment (p0 p1 p2)
                               (multiple-value-bind (x0 y0 x1 y1 x2 y2 x3 y3)
                                   (climi::bezier-segment/quadric-to-cubic
                                    (zpb-ttf:x p0) (zpb-ttf:y p0)
                                    (zpb-ttf:x p1) (zpb-ttf:y p1)
                                    (zpb-ttf:x p2) (zpb-ttf:y p2))
                                 (apply #'collect-coords
                                        (climi::polygonalize-bezigon
                                         (list x0 y0 x1 y1 x2 y2 x3 y3)))))
                             (process-contour (contour)
                               (zpb-ttf:do-contour-segments (p0 p1 p2) contour
                                 (if (null p1)
                                     (collect-coords
                                      (zpb-ttf:x p0) (zpb-ttf:y p0)
                                      (zpb-ttf:x p2) (zpb-ttf:y p2))
                                     (process-segment p0 p1 p2)))))
                      (process-contour contour)
                      (polygons (result)))))
                (with-drawing-options (medium :transformation updown)
                  (map-over-region-set-regions
                   (lambda (polygon)
                     (draw-design medium polygon))
                   (let ((splits (climi::polygon-op-inner*
                                  (loop for coords in (polygons)
                                        for polygon = (make-polygon* coords)
                                        appending (climi::polygon->pg-edges
                                                   polygon nil))
                                  :non-zero)))
                     (climi::pg-splitters->polygons splits)))))))))

(defmethod medium-draw-text* ((medium ttf-medium-mixin) string x y start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (climi::orf end (length string))
  (let* ((direction (climi::medium-line-direction medium))
         ;; DRAW-DESIGN doesn't operate in native coordinates. This is why we
         ;; need to "cancel" the device transformation.
         (base (compose-transformations
                (invert-transformation (medium-device-transformation medium))
                (medium-text-transformation
                 medium x y toward-x toward-y direction)))
         ;; Glyph things.
         (font (text-style-mapping (port medium)
                                   (medium-text-style medium)))
         (glyph-codes (string-glyph-codes string :start start :end end))
         ;; GLYPH-IDS are not used, but font-prepare-glyphs expects it.
         (glyph-ids (make-array (- end start)
                                :element-type '(unsigned-byte 32)
                                :adjustable nil :fill-pointer nil)))
    (multiple-value-bind (x y xmin ymin xmax ymax)
        (font-prepare-glyphs
         glyph-ids font string start end 0 0 align-x align-y direction)
      (declare (ignore xmin ymin xmax ymax))
      (naive-render-composite-glyphs
       font glyph-codes medium x y base direction))))
