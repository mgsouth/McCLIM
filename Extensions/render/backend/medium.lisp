(in-package #:mcclim-render)

(defclass render-medium-mixin (ttf-medium-mixin basic-medium)
  ((%buffer% ;; stores the drawn string glyph ids.
    :initform (make-array 1024
                          :element-type '(unsigned-byte 32)
                          :adjustable nil
                          :fill-pointer nil)
    :accessor render-medium-%buffer%
    :type (simple-array (unsigned-byte 32)))))

(defun glyph-codes-buffer (medium length)
  (let ((buffer (render-medium-%buffer% medium)))
    (when (< (length (the (simple-array (unsigned-byte 32))
                          (render-medium-%buffer% medium)))
             length)
      (setf buffer (make-array length
                               :element-type '(unsigned-byte 32)
                               :adjustable nil :fill-pointer nil)
            (render-medium-%buffer% medium) buffer))
    buffer))

(defun %medium-stroke-paths (medium paths)
  (when-let ((mirror (medium-drawable medium)))
    (%stroke-paths medium mirror paths
                   (medium-line-style medium)
                   (medium-device-transformation medium)
                   (medium-device-region medium)
                   (transform-region (medium-native-transformation medium)
                                     (medium-ink medium)))))

(defun %medium-fill-paths (medium paths)
  (when-let ((mirror (medium-drawable medium)))
    (%fill-paths mirror paths
                 (medium-device-transformation medium)
                 (medium-device-region medium)
                 (transform-region (medium-native-transformation medium)
                                   (medium-ink medium)))))

(defun %medium-draw-image (target source x y width height to-x to-y)
  (%draw-image target source
               (round x) (round y)
               (round width) (round height)
               (round to-x) (round to-y)))

;;; XXX: used only for medium-draw-text* for now.
(defun %medium-fill-image-mask (medium mask-image x1 y1 x2 y2 mask-dx mask-dy)
  (when-let ((mirror (medium-drawable medium)))
    (%fill-image-mask mirror x1 y1 x2 y2
                      (transform-region (medium-native-transformation medium)
                                        (medium-ink medium))
                      (medium-device-region medium)
                      ;; Stencil
                      mask-image (floor mask-dx) (floor mask-dy))))

(defun %medium-fill-image (medium x1 y1 x2 y2)
  (when-let ((mirror (medium-drawable medium)))
    (%fill-image mirror x1 y1 x2 y2
                 (transform-region (medium-native-transformation medium)
                                   (medium-ink medium))
                 (medium-device-region medium))))

;;; standard medium protocol

(defmethod medium-clear-area ((medium render-medium-mixin) x1 y1 x2 y2)
  (when-let* ((mirror (medium-drawable medium))
              (image (image-mirror-image mirror)))
    (clear-image (pattern-array image)
                 (medium-background medium)
                 x1 y1 x2 y2
                 (medium-device-region medium))))

(defmethod medium-draw-rectangle* ((medium render-medium-mixin) x1 y1 x2 y2 filled)
  (let ((transformation (medium-device-transformation medium)))
    (if (and filled (rectilinear-transformation-p transformation))
        (climi::with-transformed-positions* (transformation x1 y1 x2 y2)
          (when (< x2 x1) (rotatef x2 x1))
          (when (< y2 y1) (rotatef y2 y1))
          (%medium-fill-image medium x1 y1 x2 y2))
        (%medium-stroke-paths medium (let ((path (make-path x1 y1)))
                                       (line-to path x2 y1)
                                       (line-to path x2 y2)
                                       (line-to path x1 y2)
                                       (close-path path)
                                       (list path))))))

(defmethod medium-draw-polygon* ((medium render-medium-mixin) coord-seq closed filled)
  (let ((x (elt coord-seq 0))
        (y (elt coord-seq 1)))
    (let ((path (make-path x y)))
      (do ((v 2 (+ 2 v)))
          ((>= v (length coord-seq)))
        (let ((x (elt coord-seq v))
              (y (elt coord-seq (1+ v))))
          (line-to path x y)))
      (when closed
        (close-path path))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))

(defmethod medium-draw-line* ((medium render-medium-mixin) x1 y1 x2 y2)
  (let ((path (make-path x1 y1)))
    (line-to path x2 y2)
    (%medium-stroke-paths medium (list path))))

(defmethod medium-draw-point* ((medium render-medium-mixin) x y)
  (let* ((line-style (medium-line-style medium))
         (thickness (line-style-effective-thickness line-style medium))
         (path (arc x y (max 1 (/ thickness 2)) pi (+ pi (* 2 pi)))))
    (%medium-fill-paths medium (list path))))

#+ (or)
(defun render-draw-circle*
    (medium center-x center-y radius start-angle end-angle filled)
  (let ((path (arc center-x center-y radius (+ pi start-angle) (+ pi end-angle))))
    (if filled
        (%medium-fill-paths medium (list path))
        (%medium-stroke-paths medium (list path)))))

(defmethod medium-draw-ellipse* ((medium render-medium-mixin) center-x center-y
                                 radius-1-dx radius-1-dy
                                 radius-2-dx radius-2-dy
                                 start-angle end-angle filled
                                 &aux (el (make-ellipse*
                                           center-x center-y
                                           radius-1-dx radius-1-dy
                                           radius-2-dx radius-2-dy
                                           :start-angle start-angle
                                           :end-angle end-angle)))
  (when (region-equal el +nowhere+)
    (return-from medium-draw-ellipse*))
  (multiple-value-bind (cx cy hx hy theta)
      (climi::ellipse-simplified-representation el)
    (declare (ignorable cx cy))
    (let* ((sa (- (* 2 pi) end-angle theta))
           (dalpha (- end-angle start-angle))
           (path (ellipse-arc center-x center-y hx hy theta
                              sa (+ sa dalpha))))
      (when filled
        (line-to path center-x center-y))
      (if filled
          (%medium-fill-paths medium (list path))
          (%medium-stroke-paths medium (list path))))))


(deftype index () `(integer 0 #.array-dimension-limit))
(define-modify-macro roundf () climi::round-coordinate)

(defun soft-render-composite-glyphs
    (target x y ink clip direction font glyph-codes length)
  (loop
    for id fixnum from 0 below length
    for code = (aref glyph-codes id)
    ;; compute coordinates
    for x0 fixnum = x then (+ x0 (glyph-info-advance-dx info))
    for y0 fixnum = y then (+ y0 (glyph-info-advance-dy info))
    for info = (font-glyph-info font code direction)
    for glyf = (glyph-info-pixarray info)
    do (let* ((stencil (make-instance 'climi::%ub8-stencil :array glyf))
              (x1 (- x0 (glyph-info-origin-x info)))
              (y1 (- y0 (glyph-info-origin-y info)))
              (x2 (+ x1 (pattern-width stencil)))
              (y2 (+ y1 (pattern-height stencil))))
         (%fill-image-mask target x1 y1 x2 y2 ink clip
                           stencil (- 0 x1) (- 0 y1)))))

(defun draw-glyphs/fast (font glyph-codes length
                         medium x y transformation direction
                         xmin ymin xmax ymax)
  (declare (optimize (speed 3))
           (type index length))
  (when-let ((mirror (medium-drawable medium)))
    (let ((ink (medium-ink medium))
          (clip (medium-device-region medium)))
      (climi::with-transformed-position (transformation x y)
        (roundf x)
        (roundf y)
        (soft-render-composite-glyphs
         mirror x y ink clip direction font glyph-codes length)))
    (%notify-image-updated mirror (make-rectangle* xmin ymin xmax ymax))))

;;; This is conceptually ~similar to what medium-xrender does.
(defun draw-glyphs/ugly (font glyph-codes length
                         medium x y transformation direction
                         xmin ymin xmax ymax)
  (declare (optimize (speed 3))
           (type index length))
  (let* ((width (- xmax xmin))
         (height (- ymax ymin))
         (x0 (- x xmin))
         (y0 (- y ymin))
         (pixmap
           (with-output-to-pixmap (new-medium medium :width width :height height)
             (draw-glyphs/fast font glyph-codes length
                               new-medium x0 y0 +identity-transformation+ direction
                               0 0 width height)))
         (design (image-mirror-image pixmap))
         (transf (compose-translation-with-transformation transformation xmin ymin))
         (pattern (transform-region transf design)))
    (draw-design medium pattern)))

(defmethod medium-draw-text* ((medium render-medium-mixin) string x y
                              start end
                              align-x align-y
                              toward-x toward-y transform-glyphs)
  (if (null end)
      (setf end (length string))
      (minf end (length string)))
  (unless (medium-drawable medium)
    (return-from medium-draw-text*))
  (let* ((port (port medium))
         (text-style (medium-text-style medium))
         (font (text-style-mapping port text-style))
         (glyph-ids (glyph-codes-buffer medium (- end start)))
         (direction (climi::medium-line-direction medium))
         (transformation (climb:medium-text-transformation
                          medium x y toward-x toward-y)))
    (multiple-value-bind (x y xmin ymin xmax ymax)
        (mcclim-truetype:font-prepare-glyphs
         glyph-ids font string start end 0 0 align-x align-y direction)
      (with-identity-transformation (medium)
        (if (translation-transformation-p transformation)
            (draw-glyphs/fast font glyph-ids (- end start)
                              medium x y transformation direction
                              xmin ymin xmax ymax)
            (draw-glyphs/ugly font glyph-ids (- end start)
                              medium x y transformation direction
                              xmin ymin xmax ymax))))))
