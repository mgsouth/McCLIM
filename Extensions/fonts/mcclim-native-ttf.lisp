;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2008 by Andy Hefner <ahefner@gmail.com>
;;;  (c) Copyright 2016 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Glyph rendering via zpb-ttf and cl-vectors.
;;;

(in-package #:mcclim-truetype)

;;; TODO:
;;;  * Implement fixed-font-width-p for zpb-ttf.
;;;  * Implement text direction for font-text-extents

;;; Wish-list:
;;;  * Subpixel antialiasing. It would be straightforward to generate the
;;;    glyphs by tripling the width as passed to cl-vectors and compressing
;;;    triplets of pixels together ourselves. I'm not certain how to draw
;;;    the result through xrender. I've seen hints on Google that there is
;;;    subpixel AA support in xrender, which isn't obvious from CLX or the
;;;    spec. Failing that, we could use a 24bpp mask with component-alpha.
;;;    That might even be how you're supposed to do it. I'm skeptical as to
;;;    whether this would be accelerated for most people.

;;;  * Subpixel positioning. Not hard in principle - render multiple versions
;;;    of each glyph, offset by fractions of a pixel. Horizontal positioning
;;;    is more important than vertical, so 1/4 pixel horizontal resolution
;;;    and 1 pixel vertical resolution should suffice. Given how ugly most
;;;    CLIM apps are, and the lack of WYSIWYG document editors crying out
;;;    for perfect text spacing in small fonts, we don't really need this.


(defvar *zpb-font-lock* (clim-sys:make-lock "zpb-font"))

(defclass truetype-font-family (font-family)
  ((all-faces :initform nil
              :accessor all-faces
              :reader font-family-all-faces)))

(defclass truetype-face (font-face)
  ((all-fonts :initform nil :accessor all-fonts)
   (preloaded :initarg :preloaded :reader preloadedp)
   (font-loader :initarg :loader :reader zpb-ttf-font-loader)))

(defmethod initialize-instance :after ((face truetype-face) &key &allow-other-keys)
  (let ((family (font-face-family face)))
    (pushnew face (all-faces family))))

(defmethod print-object ((object truetype-face) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~A, ~A, preloaded: ~a"
            (font-family-name (font-face-family object))
            (font-face-name object)
            (if (preloadedp object) "yes" "no"))))

(defclass truetype-font ()
  ((face          :initarg :face     :reader font-face)
   (size          :initarg :size     :reader font-size)
   ;; Kerning is a customized advance-dx between different pairs of letters
   ;; specified in a separate kerning-table.
   (kerning-p     :initarg :kerning  :reader font-kerning-p)
   ;; Generalized boolean. If the font character width is fixed it is returned,
   ;; otherwise returns NIL.
   (fixed-width   :initarg :fixed    :reader font-fixed-width :type (or fixnum null))
   ;; Horizontal line metrics
   (ascent                           :reader font-ascent)
   (descent                          :reader font-descent)
   ;; Vertical line metrics
   (vascent                          :reader font-vascent)
   (vdescent                         :reader font-vdescent)
   ;;
   (units->pixels                    :reader zpb-ttf-font-units->pixels))
  (:default-initargs :fixed nil :dpi 72 :kerning t))

(defgeneric font-port (font)
  (:method ((font truetype-font))
    (font-family-port (font-face-family (font-face font)))))

(defmethod initialize-instance :after
    ((font truetype-font) &key dpi &allow-other-keys)
  (with-slots (face size ascent descent vascent vdescent font-loader) font
    (let* ((loader (zpb-ttf-font-loader face))
           (em->units (zpb-ttf:units/em loader))
           (dpi-factor (/ dpi 72))
           (units->pixels (/ (* size dpi-factor) em->units)))
      (setf ascent       (+ (* units->pixels (zpb-ttf:ascender loader)))
            descent      (- (* units->pixels (zpb-ttf:descender loader)))
            vascent      (+ (* units->pixels (slot-value loader 'zpb-ttf:vascender)))
            vdescent     (- (* units->pixels (slot-value loader 'zpb-ttf:vdescender)))
            (slot-value font 'units->pixels) units->pixels))
    (pushnew font (all-fonts face))))

(defmethod zpb-ttf:kerning-offset ((left character) (right character) (font truetype-font))
  (if (null (font-kerning-p font))
      0
      (zpb-ttf:kerning-offset left right (zpb-ttf-font-loader (font-face font)))))

(defmethod font-face-all-sizes ((face truetype-face))
  (sort (mapcar #'font-size (all-fonts face)) #'<))

(defmethod font-face-text-style ((face truetype-face) &optional size)
  (make-text-style (font-family-name (font-face-family face))
                   (font-face-name face)
                   size))

(defmethod print-object ((object truetype-font) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (with-slots (size ascent descent units->pixels) object
      (format stream ":size ~a :ascent ~,2f :descent ~,2f :units->pixels ~,2f"
              size ascent descent units->pixels))))

;;; Derived from CL-VECTORS library function PATHS-TTF:PATHS-FROM-GLYPH.
(defun paths-from-glyph* (glyph tr)
  "Extract paths from a glyph."
  (flet ((point (p) (multiple-value-call #'net.tuxee.paths:make-point
                      (transform-position tr (zpb-ttf:x p) (zpb-ttf:y p)))))
    (let (result)
      (zpb-ttf:do-contours (contour glyph)
        (let ((path (net.tuxee.paths:create-path :polygon))
              (last-point nil))
          (zpb-ttf:do-contour-segments (a b c) contour
            (let ((pa (point a))
                  (pb (when b (point b)))
                  (pc (point c)))
              (unless last-point
                (net.tuxee.paths:path-reset path pa))
              (net.tuxee.paths:path-extend path
                                           (if b
                                               (net.tuxee.paths:make-bezier-curve (list pb))
                                               (net.tuxee.paths:make-straight-line))
                                           pc)
              (setq last-point pc)))
          (push path result)))
      (setq result (nreverse result))
      result)))

(defun advance-width (font glyph)
  (* (zpb-ttf-font-units->pixels font)
     (zpb-ttf:advance-width glyph)))

(defun advance-height (font glyph)
  (* (zpb-ttf-font-units->pixels font)
     (zpb-ttf:advance-height glyph)))

(defun kerning-offset (font char next direction)
  (* (zpb-ttf-font-units->pixels font)
     (ecase direction
       (:left-to-right (zpb-ttf:kerning-offset char next font))
       (:right-to-left (zpb-ttf:kerning-offset next char font))
       (:top-to-bottom 0)
       (:bottom-to-top 0))))

(defun make-glyph-pixarray (font char next direction)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array suitable
   as an alpha mask, and dimensions. This function returns seven values: alpha
   mask byte array, x-origin, y-origin (subtracted from position before
   rendering), glyph width and height, horizontal and vertical advances."
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((font-loader (zpb-ttf-font-loader (font-face font)))
             (glyph (zpb-ttf:find-glyph char font-loader))
             (hx (advance-width font glyph))
             (vy (advance-height font glyph))
             (kerning (kerning-offset font char next direction))
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (x1 (elt bounding-box 0))
             (y1 (elt bounding-box 1))
             (x2 (elt bounding-box 2))
             (y2 (elt bounding-box 3))
             width height left top array)
        (setq left (floor x1))
        (setq top (ceiling y2))
        (setq width  (- (ceiling x2) (floor x1)))
        (setq height (- (ceiling y2) (floor y1)))
        (setq array (make-array (list height width)
                                :initial-element 0
                                :element-type '(unsigned-byte 8)))
        (let* ((glyph-tr (compose-transformations
                          (make-translation-transformation (- left) top)
                          (make-scaling-transformation units->pixels (- units->pixels))))
               (paths (paths-from-glyph* glyph glyph-tr))
               (state (aa:make-state)))
          (dolist (path paths)
            (vectors:update-state state path))
          (aa:cells-sweep state
                          (lambda (x y alpha)
                            (when (array-in-bounds-p array y x)
                              (setf alpha (min 255 (abs alpha))
                                    (aref array y x) (climi::clamp
                                                      (floor (+ (* (- 256 alpha) (aref array y x))
                                                                (* alpha 255))
                                                             256)
                                                      0 255))))))
        #+ (or) ;; draw delicate border around each glyph (for testing)
        (progn
          (loop for j from 0 below height do (setf (aref array j 0)
                                                   (logior #x40 (aref array j 0))
                                                   (aref array j (1- width))
                                                   (logior #x40 (aref array j (1- width)))))
          (loop for i from 0 below width do (setf (aref array 0 i)
                                                  (logior #x40 (aref array 0 i))
                                                  (aref array (1- height) i)
                                                  (logior #x40 (aref array (1- height) i)))))
        (values array left top width height
                (climi::round-coordinate hx)
                (climi::round-coordinate vy)
                (climi::round-coordinate kerning))))))

(declaim (inline char-glyph-code glyph-code-char))
(defun char-glyph-code (char next)
  (declare (optimize (speed 3) (safety 0)))
  ;; Instead of an error we present [] character.
  #+ (or)
  (assert (and (char/= char #\newline)
               (not (eql next #\newline))))
  (if next
      (dpb (char-code next)
           (byte #.(ceiling (log char-code-limit 2))
                 #.(ceiling (log char-code-limit 2)))
           (char-code char))
      (char-code char)))

(defun glyph-code-char (code)
  (values (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code))
          (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                #.(ceiling (log char-code-limit 2)))
                          code))))

(defun map-over-string-glyph-codes (fun string start end)
  (loop with len = (length string)
        for i from start below end
        for j from (1+ start)
        for char = (char string i)
        for next = (and (< j len) (char string j))
        for code = (if (eql next #\newline)
                       (char-glyph-code char nil)
                       (char-glyph-code char next))
        do (funcall fun code)))

(defun string-glyph-codes (string &key (start 0) (end (length string)))
  "Converts string to a sequence of glyph codes. Some characters are composed of
many codepoints – it is not guaranteed that length of the string and the length
of resulting sequence are equal."
  (alexandria:minf end (length string))
  (when (>= start end)
    (return-from string-glyph-codes #()))
  (let ((index 0)
        (array (make-array (- end start) :fill-pointer nil
                                         :adjustable nil
                                         :element-type '(unsigned-byte 32))))
    (flet ((doit (code)
             (setf (aref array index) code)
             (incf index)))
      (declare (dynamic-extent #'doit))
      (map-over-string-glyph-codes #'doit string start end))
    array))


(deftype glyph-pixarray () '(simple-array (unsigned-byte 8) (* *)))

(defstruct (glyph-info (:constructor make-glyph-info
                           (id pixarray left top width height
                            advance-hx advance-vy)))
  (id 0                      :type fixnum)
  (pixarray nil :read-only t :type (or null glyph-pixarray))
  ;; duplicates the pixarray dimensions
  (width 0      :read-only t)
  (height 0     :read-only t)
  ;; Bearings
  (left 0       :read-only t)
  (top 0        :read-only t)
  ;; Horizontal and vertical advance width and height.
  (advance-hx 0)
  (advance-vy 0)
  ;; Metrics configured for the particular direction.
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum)
  (advance-dx 0 :type fixnum)
  (advance-dy 0 :type fixnum))

(defclass cached-truetype-font (truetype-font)
  ;; FIXME cache pixarrays.
  (;(all-glyph-data :initform (make-hash-table :size ))
   (ltr-glyph-info :initform (make-hash-table :size 512))
   (rtl-glyph-info :initform (make-hash-table :size 512))
   (ttb-glyph-info :initform (make-hash-table :size 512))
   (btt-glyph-info :initform (make-hash-table :size 512))))

(defun glyph-info-advance (font info kerning direction)
  (ecase direction
    (:left-to-right
     (let ((origin-x  (- (glyph-info-left info)))
           (origin-y  (glyph-info-top info))
           (cursor-dx (+ (glyph-info-advance-hx info) kerning))
           (cursor-dy 0))
       (values info origin-x origin-y cursor-dx cursor-dy)))
    (:right-to-left
     (let ((origin-x  (- (glyph-info-advance-hx info)
                         (glyph-info-left info)))
           (origin-y  (glyph-info-top info))
           (cursor-dx (- (+ (glyph-info-advance-hx info) kerning)))
           (cursor-dy 0))
       (values info origin-x origin-y cursor-dx cursor-dy)))
    (:top-to-bottom
     (let ((origin-x  (climi::round-coordinate (/ (glyph-info-width info) 2.0)))
           (origin-y  (- (glyph-info-top info)
                         (climi::round-coordinate (font-ascent font))))
           (cursor-dx 0)
           (cursor-dy (+ (glyph-info-advance-vy info) kerning)))
       (values info origin-x origin-y cursor-dx cursor-dy)))
    (:bottom-to-top
     (let ((origin-x  (climi::round-coordinate (/ (glyph-info-width info) 2.0)))
           (origin-y  (+ (glyph-info-top info)
                         (climi::round-coordinate (font-descent font))))
           (cursor-dx 0)
           (cursor-dy (- (+ (glyph-info-advance-vy info) kerning))))
       (values info origin-x origin-y cursor-dx cursor-dy)))))

(defun font-glyph-info (font code direction)
  (ensure-gethash code (ecase direction
                         (:left-to-right (slot-value font 'ltr-glyph-info))
                         (:right-to-left (slot-value font 'rtl-glyph-info))
                         (:top-to-bottom (slot-value font 'ttb-glyph-info))
                         (:bottom-to-top (slot-value font 'btt-glyph-info)))
    (font-generate-glyph (font-port font) font code direction)))

(defgeneric font-generate-glyph (port font code direction)
  (:documentation "Truetype TTF renderer internal interface.")
  (:method (port (font cached-truetype-font) code direction)
    (declare (ignore port))
    (multiple-value-bind (char next) (glyph-code-char code)
      (multiple-value-bind (arr left top width height hx vy kerning)
          (make-glyph-pixarray font char next direction)
        (let ((info (make-glyph-info code arr left top width height hx vy)))
          (multiple-value-bind (info x0 y0 dx dy)
              (glyph-info-advance font info kerning direction)
            (setf (glyph-info-origin-x info) x0)
            (setf (glyph-info-origin-y info) y0)
            (setf (glyph-info-advance-dx info) dx)
            (setf (glyph-info-advance-dy info) dy))
          info)))))


(deftype index () `(integer 0 #.array-dimension-limit))

(defun line-advance (medium font string start end)
  (let ((cursor-dx 0)
        (cursor-dy 0))
    (labels ((process-code (code)
               (let ((glyph (font-glyph-info font code (medium-line-direction medium))))
                 (incf cursor-dx (glyph-info-advance-dx glyph))
                 (incf cursor-dy (glyph-info-advance-dy glyph)))))
      (map-over-string-glyph-codes #'process-code string start end)
      (when (member (medium-line-direction medium) '(:top-to-bottom :bottom-to-top))
        (rotatef cursor-dx cursor-dy))
      (values cursor-dx cursor-dy))))

(defun fill-glyph-indexes (medium font string start end glyph-ids)
  (let ((idx 0)
        (direction (medium-line-direction medium)))
    (flet ((process (code)
             (let ((info (font-glyph-info font code direction)))
               (setf (aref glyph-ids idx) (glyph-info-id info))
               (incf idx))))
      (map-over-string-glyph-codes #'process string start end))))

(defun font-prepare-glyphs (medium font string start end align-x align-y)
  (declare (optimize (speed 3))
           (type index start end)
           (type string string))
  (when (>= start end)
    (return-from font-prepare-glyphs (values 0 0 0 0 0 0)))
  (ecase (medium-line-direction medium)
    (:left-to-right (font-prepare-glyphs/ltr medium font string start end align-x align-y))
    (:right-to-left (font-prepare-glyphs/rtl medium font string start end align-x align-y))
    (:top-to-bottom (font-prepare-glyphs/ttb medium font string start end align-x align-y))
    (:bottom-to-top (font-prepare-glyphs/btt medium font string start end align-x align-y))))

(defun font-prepare-glyphs/ltr (medium font string start end align-x align-y)
  (declare ;(optimize (speed 3))
           (type index start end)
           (type string string)
           (ignore medium))
  (let ((firstp t) xmin ymin xmax ymax advance-x)
    (flet ((process-code (code)
             (let ((info (font-glyph-info font code :left-to-right)))
               (when firstp
                 (setf firstp nil
                       xmin 0
                       ymin (- (font-ascent font))
                       ymax (+ (font-descent font))
                       advance-x 0))
               (incf advance-x (glyph-info-advance-dx info)))))
      (declare (inline process-code))
      (loop with this-char = (char string start)
            for idx1 of-type index from (1+ start) below end
            as next-char = (char string idx1)
            as code = (char-glyph-code this-char next-char)
            do (process-code code)
               (setf this-char next-char)
            finally
               (process-code (char-code this-char))
               (setf xmax (+ xmin advance-x))))
    (let ((dx (ecase align-x
                (:baseline 0)
                (:left   0 #|(- xmin)|#)
                (:center (- (/ (- xmax xmin) 2.0)))
                (:right  (- (- xmax xmin)))))
          (dy (ecase align-y
                (:baseline 0)
                (:center (- (/ (+ ymax ymin) 2.0)))
                (:top    (- ymin))
                (:bottom (- ymax)))))
      (incf xmin dx) (incf xmax dx)
      (incf ymin dy) (incf ymax dy)
      (values dx dy xmin ymin xmax ymax))))

(defun font-prepare-glyphs/ttb (medium font string start end align-x align-y)
  (declare ;(optimize (speed 3))
           (type index start end)
           (type string string)
           (ignore medium))
  (let ((firstp t) xmin ymin xmax ymax advance-y
        (line-height (+ (font-descent font)
                        (font-ascent font))))
    (flet ((process-code (code)
             (let ((info (font-glyph-info font code :top-to-bottom)))
               (when firstp
                 (setf firstp nil
                       ymin 0
                       xmin (- (/ line-height 2))
                       xmax (+ (/ line-height 2))
                       advance-y 0))
               (incf advance-y (glyph-info-advance-dy info)))))
      (declare (inline process-code))
      (loop with this-char = (char string start)
            for idx1 of-type index from (1+ start) below end
            as next-char = (char string idx1)
            as code = (char-glyph-code this-char next-char)
            do (process-code code)
               (setf this-char next-char)
            finally
               (process-code (char-code this-char))
               (setf ymax (+ ymin advance-y))))
    (let ((dy (ecase align-x
                (:baseline 0)
                (:left   0 #|(- ymin)|#)
                (:center (- (/ (- ymax ymin) 2.0)))
                (:right  (- (- ymax ymin)))))
          (dx (ecase align-y
                (:baseline 0)
                (:center (- (/ (+ xmax xmin) 2.0)))
                (:top    (- xmax))
                (:bottom (- xmin)))))
      (incf xmin dx) (incf xmax dx)
      (incf ymin dy) (incf ymax dy)
      (values dx dy xmin ymin xmax ymax))))

(defun font-prepare-glyphs/rtl (medium font string start end align-x align-y)
  (declare ;(optimize (speed 3))
           (type index start end)
           (type string string)
           (ignore medium))
  (let ((firstp t) xmin ymin xmax ymax advance-x)
    (flet ((process-code (code)
             (let ((info (font-glyph-info font code :right-to-left)))
               (when firstp
                 (setf firstp nil
                       xmax 0
                       ymin (- (font-ascent font))
                       ymax (+ (font-descent font))
                       advance-x 0))
               (incf advance-x (glyph-info-advance-dx info)))))
      (declare (inline process-code))
      (loop with this-char = (char string start)
            for idx1 of-type index from (1+ start) below end
            as next-char = (char string idx1)
            as code = (char-glyph-code this-char next-char)
            do (process-code code)
               (setf this-char next-char)
            finally
               (process-code (char-code this-char))
               (setf xmin (+ xmax advance-x))))
    (let ((dx (ecase align-x
                (:baseline 0)
                (:left   (- xmax xmin))
                (:center (+ (/ (- xmax xmin) 2.0)))
                (:right  0 #|hx-x2|#)))
          (dy (ecase align-y
                (:baseline 0)
                (:center (- (/ (+ ymax ymin) 2.0)))
                (:top    (- ymin))
                (:bottom (- ymax)))))
      (incf xmin dx) (incf xmax dx)
      (incf ymin dy) (incf ymax dy)
      (values dx dy xmin ymin xmax ymax))))

(defun font-prepare-glyphs/btt (medium font string start end align-x align-y)
  (declare ;(optimize (speed 3))
           (type index start end)
           (type string string)
           (ignore medium))
  (let ((firstp t) xmin ymin xmax ymax advance-y
        (line-height (+ (font-descent font)
                        (font-ascent font))))
    (flet ((process-code (code)
             (let ((info (font-glyph-info font code :bottom-to-top)))
               (when firstp
                 (setf firstp nil
                       ymax 0
                       xmin (- (/ line-height 2))
                       xmax (+ (/ line-height 2))
                       advance-y 0))
               (incf advance-y (glyph-info-advance-dy info)))))
      (declare (inline process-code))
      (loop with this-char = (char string start)
            for idx1 of-type index from (1+ start) below end
            as next-char = (char string idx1)
            as code = (char-glyph-code this-char next-char)
            do (process-code code)
               (setf this-char next-char)
            finally
               (process-code (char-code this-char))
               (setf ymin (+ ymax advance-y))))
    (let ((dy (ecase align-x
                (:baseline 0)
                (:left   (- ymax ymin))
                (:center (/ (- ymax ymin) 2.0))
                (:right  0 #|hx-y2|#)))
          (dx (ecase align-y
                (:baseline 0)
                (:center (- (/ (+ xmax xmin) 2.0)))
                (:top    (- xmax))
                (:bottom (- xmin)))))
      (incf xmin dx) (incf xmax dx)
      (incf ymin dy) (incf ymax dy)
      (values dx dy xmin ymin xmax ymax))))


;;; ttf-port-mixin

(defparameter *dpi* nil
  "The value of DPI used to overwrite the default font scaling.")

(defclass ttf-port-mixin ()
  ((back-memory-cache :initform (make-hash-table :test #'equal) :allocation :class)
   ;; source -> loader (the source may be a filename or a memory block)
   (font-loader-cache :initform (make-hash-table :test #'equal))
   (font-family-cache :initform (make-hash-table :test #'equal))
   ;; Cache loader -> (face fonts) - fonts is a ht keyed wit hthe size.
   (font-direct-cache :initform (make-hash-table))
   ;; Cache for the extended standard text styles (see the manual).
   (text-style-cache  :initform (make-hash-table))
   ;; All registered families. Populated by ensure-truetype-font.
   (font-families :initform '() :accessor font-families)
   ;; DPI (for font scaling)
   (font-dpi :initarg :dpi :accessor font-dpi)))

;;; We can't initialize FONT-DPI in INITIALIZE-INSTANCE :AFTER method, because
;;; some ports can create grafts only after their own initialization.
(defmethod slot-unbound (class (port ttf-port-mixin) (slot (eql 'font-dpi)))
  (let ((dpi (or *dpi*
                 (ignore-errors
                  (clim:graft-pixels-per-inch (clim:find-graft :port port)))
                 72)))
    ;; Issue a warning when DPI is suspiciously small. The value is arbitrary.
    (when (< dpi 10)
      (warn "~s: DPI ~s is suspiciously small." (class-name class) dpi))
    (setf (slot-value port 'font-dpi) dpi)))

(defun invalidate-port-font-cache (port)
  (with-slots (font-loader-cache font-family-cache font-direct-cache text-style-cache) port
    (maphash-values (lambda (val) (zpb-ttf:close-font-loader val)) font-loader-cache)
    (clrhash font-loader-cache)
    (clrhash font-family-cache)
    (clrhash font-direct-cache)
    (clrhash text-style-cache))
  (setf (font-families port) nil))

(defmethod destroy-port :after ((port ttf-port-mixin))
  (invalidate-port-font-cache port))

(defmethod port-all-font-families ((port ttf-port-mixin) &key invalidate-cache preload)
  (when invalidate-cache
    (invalidate-port-font-cache port)
    (register-all-ttf-fonts port :preload preload)
    (register-standard-fonts port :preload preload))
  (font-families port))

(defun ensure-truetype-font (port filename source size &optional preload)
  (setf size (climb:normalize-font-size size))
  (with-slots (font-loader-cache font-family-cache font-direct-cache text-style-cache) port
    (multiple-value-bind (loader loader-foundp) (gethash filename font-loader-cache)
      (unless loader-foundp
        (setf source (open source
                           :direction :input
                           :element-type '(unsigned-byte 8)
                           #+ccl :sharing #+ccl nil)))
      (let* ((loader (or loader (zpb-ttf:open-font-loader source)))
             (f1-name (zpb-ttf:family-name loader))
             (f2-name (zpb-ttf:subfamily-name loader))
             (text-style (make-text-style f1-name f2-name size))
             (font-dpi (font-dpi port)))
        (flet ((make-family ()
                 (make-instance 'truetype-font-family :name f1-name :port port))
               (make-face (family)
                 (make-instance 'truetype-face :family family :name f2-name
                                               :loader loader :preloaded preload))
               (make-font (face size)
                 (make-instance 'cached-truetype-font
                                :face face :size size :dpi font-dpi)))
          (when loader-foundp
            (return-from ensure-truetype-font
              (destructuring-bind (face fonts) (gethash loader font-direct-cache)
                (setf (gethash text-style text-style-cache)
                      (ensure-gethash size fonts (make-font face size))))))
          (let* ((family (ensure-gethash f1-name font-family-cache (make-family)))
                 (face   (make-face family))
                 (fonts  (make-hash-table :test #'eql))
                 (font   (make-font face size)))
            (setf (gethash filename font-loader-cache) loader
                  (gethash loader font-direct-cache) (list face fonts)
                  (gethash size fonts) font
                  (gethash text-style text-style-cache) font)
            (pushnew family (font-families port))
            font))))))

(defun register-ttf-font (port filename preload)
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (let* ((vector (gethash filename (slot-value port 'back-memory-cache)))
           (source (if (and (not preload) (not vector))
                       filename
                       (flexi-streams:make-in-memory-input-stream
                        (ensure-gethash filename
                                        (slot-value port 'back-memory-cache)
                                        (read-file-into-byte-vector filename))))))
      (handler-case (dolist (size '(8 10 12 14 18 24 48 72))
                      (ensure-truetype-font port filename source size preload))
        (error ()
          (ignore-errors (and (streamp source) (close source)))
          (remhash filename (slot-value port 'back-memory-cache)))))))

(defun register-all-ttf-fonts (port &key (dir *truetype-font-path*) (preload nil))
  (with-port-locked (port)
    (dolist (source (directory (merge-pathnames "*.ttf" dir)))
      (register-ttf-font port source preload))))

(defun register-standard-fonts (port &key (preload nil))
  (with-port-locked (port)
    (dolist (source (mapcar #'cdr *families/faces*))
      (register-ttf-font port source preload))))

(defmethod text-style-mapping ((port ttf-port-mixin) (text-style standard-text-style) &optional charset)
  (declare (ignore charset))
  (setf text-style (parse-text-style* text-style))
  (or (gethash text-style (slot-value port 'text-style-cache))
      (multiple-value-bind (family face size) (text-style-components text-style)
        (when-let ((source (assoc-value *families/faces* (list family face) :test #'equal)))
          (clim-sys:with-lock-held (*zpb-font-lock*)
            (ensure-truetype-font port source source size))))
      (error "~s can't map the text style ~s." port text-style)))

(defclass ttf-device-text-style (climi::device-font-text-style)
  ((path :initarg :path :reader device-font-path)
   (size :initarg :size :reader device-font-size :reader text-style-size)
   (unit :initarg :unit :reader device-font-unit :reader text-style-unit)
   (preload :initarg :preload :reader device-font-preload)))

(defmethod text-style-components ((style ttf-device-text-style))
  (with-slots (size unit) style
    (values :device :device size unit)))

(defmethod make-device-font-text-style ((port ttf-port-mixin) font)
  (destructuring-bind (path &key (size :normal) (unit :normal)
                                 (preload nil) &allow-other-keys)
      (if (listp font) font (list font))
    (make-instance 'ttf-device-text-style
                   :path path :size size :unit unit :preload preload
                   :display-device port :device-font-name font)))

(defmethod text-style-mapping ((port ttf-port-mixin)
                               (text-style ttf-device-text-style)
                               &optional charset)
  (declare (ignore charset))
  (let* ((path (device-font-path text-style))
         (size (device-font-size text-style))
         (preload (device-font-preload text-style)))
    (if-let ((file (and path (probe-file path))))
      (clim-sys:with-lock-held (*zpb-font-lock*)
        (ensure-truetype-font port file file size preload))
      (error "~s can't map the text style ~s." port text-style))))



;;; ttf-medium-mixin

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

;;; FIXME stub
(defmethod text-bounding-rectangle* ((medium ttf-medium-mixin) string
                                     &key text-style (start 0) end)
  (setf string (string string)
        end (or end (length string)))
  (multiple-value-bind (w h dx dy baseline)
      (text-size medium string :text-style text-style :start start :end end)
    (declare (ignore dy))
    (let (xmin ymin xmax ymax)
      (setf xmin (if (plusp dx) 0 dx)
            xmax (+ xmin w)
            ymin (- baseline)
            ymax (+ ymin h))
      (values xmin ymin xmax ymax))))

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
         (glyph-codes (string-glyph-codes string :start start :end end)))
    (multiple-value-bind (x y xmin ymin xmax ymax)
        (font-prepare-glyphs medium font string start end align-x align-y)
      (declare (ignore xmin ymin xmax ymax))
      (naive-render-composite-glyphs
       font glyph-codes medium x y base direction))))

