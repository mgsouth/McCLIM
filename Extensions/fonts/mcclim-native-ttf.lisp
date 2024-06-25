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

;;; This long explanation is because I've confused myself with font coordinates
;;; and origins too many times and decided to go step by step with explanations.
;;; -- jd 2024-06-19
;;; 
;;;
;;; The glyph bounding rectangle MN is specified in graphics coordinates
;;; (y grows upwards) relative to [0 0].
;;; M=[x1 y1], N=[x2 y2], O=[x1 y2], P=[x2 y1].
;;;
;;;    ^     
;;;  y2|     O...........N
;;;    |     :           :
;;;    |     :           :
;;;    |     :           :
;;;    |     :           : 
;;;  --L-----:-----------:-----R--
;;;    |     :           :
;;;  y1|     M...........P     dx
;;;    |      
;;; 
;;; 
;;; Depending on the text direction the glyph origin (the cursor position when
;;; drawing a glyph) differs. We need to compute the origin now and transform it
;;; along with the glyph. L=[0 0], R=[dx 0], T=[cx, ascent], B=[cx, -descent].
;;;
;;;
;;;    ^           T
;;;    |
;;;    |
;;;  y2|     O...........N
;;;    |     :           :
;;;    |     :           :
;;;    |     :           :
;;;    |     :           : 
;;;  --L-----:-----------:-----R--
;;;    |     :           :
;;;  y1|     M...........P     dx
;;;    |      
;;;    |           B
;;;
;;; Internally glyphs are represented as bitmaps in screen coordinates in [em]
;;; units, so we need to flip the coordinate system, scale it and then align the
;;; bounding box with axes so X and Y are positive.
;;;
;;; This step may seem not intuitive. For me it is easier to imagine that
;;; the glyph stays at place, and that we rotate y-axis. For clarity I'm
;;; leaving the same labels, but all points are transformed.
;;; 
;;;    |           T                ;;          |     T              
;;;    |                            ;;          |                    
;;;    |                            ;;          |                    
;;; -y2|     O...........N          ;;  --------O-----------N--------
;;;    |     :           :          ;;          |           :        
;;;    |     :           :          ;;          |           :        
;;;    |     :           :          ;;          |           :        
;;;    |     :           :          ;;          |           :        
;;;  --L-----:-----------:-----R--  ;;    L     |           :     R  
;;;    |     :           :          ;;          |           :        
;;; -y1|     M...........P     dx   ;;          M...........P     dx
;;;    |                            ;;          |                    
;;;    v           B                ;;          v     B              
;;;
;;; In other words the transformation is scaling by [dpi, -dpi] and
;;; transformation by [-x1 +y2].
;;;
;;; To avoid rounding errors we compute and round coordiantes by hand and use
;;; the transformation to draw this array.
;;; 
(defun make-glyph-pixarray (font char next direction)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array suitable
   as an alpha mask, and dimensions. This function returns seven values: alpha
   mask byte array, x-origin, y-origin (subtracted from position before
   rendering), glyph width and height, horizontal and vertical advances."
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels ascent descent vascent vdescent) font
      (let* ((font-loader (zpb-ttf-font-loader (font-face font)))
             (glyph (zpb-ttf:find-glyph char font-loader))
             (scale units->pixels)
             (glyf-bbox (zpb-ttf:bounding-box glyph))
             (x1 (* scale (elt glyf-bbox 0)))
             (y1 (* scale (elt glyf-bbox 1)))
             (x2 (* scale (elt glyf-bbox 2)))
             (y2 (* scale (elt glyf-bbox 3)))
             ;;
             (dx (- (floor x1)))
             (dy (ceiling y2))
             (ws (- (ceiling x2) (floor x1)))
             (hs (- (ceiling y2) (floor y1)))
             (array (make-array (list hs ws) :initial-element 0
                                             :element-type '(unsigned-byte 8)))
             (transf (compose-transformations
                      (make-translation-transformation dx dy)
                      (make-scaling-transformation units->pixels (- units->pixels)))))
        (flet ((set-pixel (x y alpha)
                 (when (array-in-bounds-p array y x) ;unnecessary test?
                   (setf alpha (min 255 (abs alpha))
                         (aref array y x) (climi::clamp
                                           (floor (+ (* (- 256 alpha) (aref array y x))
                                                     (* alpha 255))
                                                  256)
                                           0 255)))))
          (declare (dynamic-extent #'set-pixel))
          (let ((paths (paths-from-glyph* glyph transf))
                (state (aa:make-state)))
            (dolist (path paths)
              (vectors:update-state state path))
            (aa:cells-sweep state #'set-pixel)))
        #+ (or) ;; draw delicate border around each glyph (for testing)
        (let ((height (array-dimension array 0))
              (width (array-dimension array 1)))
          (loop for j from 0 below height do (setf (aref array j 0)
                                                   (logior #x40 (aref array j 0))
                                                   (aref array j (1- width))
                                                   (logior #x40 (aref array j (1- width)))))
          (loop for i from 0 below width do (setf (aref array 0 i)
                                                  (logior #x40 (aref array 0 i))
                                                  (aref array (1- height) i)
                                                  (logior #x40 (aref array (1- height) i)))))
        (let* ((hx (climi::round-coordinate (* scale (zpb-ttf:advance-width glyph))))
               (vy (climi::round-coordinate (* scale (zpb-ttf:advance-height glyph))))
               (cx (climi::round-coordinate (/ hx 2.0)))
               (kr (climi::round-coordinate
                    (ecase direction
                      (:left-to-right (* scale (zpb-ttf:kerning-offset char next font)))
                      (:right-to-left (* scale (zpb-ttf:kerning-offset next char font)))
                      ((:top-to-bottom :bottom-to-top) 0))))
               ;; Mind the flip please.
               (ymin (- ascent))
               (ymax (+ descent))
               (xmin (- cx vascent))
               (xmax (+ cx vdescent))
               origin-x origin-y advance-x advance-y)
          (case direction
            ;; 1) vascent and vdescent fallback values are too big (full-width/2)
            ;; 2) usually when we align to :left and :right we expect baseline
            ((:left-to-right :right-to-left)
             (setf xmin (- x1) xmax (- hx xmin))))
          (ecase direction
            (:left-to-right
             (setf origin-x 0
                   origin-y 0
                   advance-x (+ hx kr)
                   advance-y 0))
            (:right-to-left
             (setf origin-x (ceiling x2)
                   origin-y 0
                   advance-x (- (+ hx kr))
                   advance-y 0))
            (:top-to-bottom
             (setf origin-x cx
                   origin-y (- (ceiling ascent))
                   advance-x 0
                   advance-y vy))
            (:bottom-to-top
             (setf origin-x cx
                   origin-y (ceiling descent)
                   advance-x 0
                   advance-y (- vy))))
          (setf x1 (floor (- x1 origin-x))
                y1 (floor (- (- y2) origin-y))
                x2 (+ x1 ws)
                y2 (+ y1 hs))
          ;; Make the bounding box relative to the new origin.
          (setf xmin (floor (- xmin origin-x))
                ymin (floor (- ymin origin-y))
                xmax (ceiling (- xmax origin-x))
                ymax (ceiling (- ymax origin-y)))
          ;; Finaly, _after_ adjusting the bounding rectangle, move origin to
          ;; conform to the bitmap coordinates.
          (incf origin-x dx)
          (incf origin-y dy)
          ;; Let there be light.
          (values array origin-x origin-y advance-x advance-y
                  xmin ymin xmax ymax x1 y1 x2 y2))))))

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
                           (id pixarray
                            origin-x origin-y advance-dx advance-dy
                            xmin ymin xmax ymax x1 y1 x2 y2)))
  (id 0                      :type fixnum)
  (pixarray nil :read-only t :type (or null glyph-pixarray))
  ;; Metrics configured for the particular direction.
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum)
  (advance-dx 0 :type fixnum)
  (advance-dy 0 :type fixnum)
  ;; Maximal bounding rectangle of the glyph.
  (xmin 0 :type fixnum)
  (ymin 0 :type fixnum)
  (xmax 0 :type fixnum)
  (ymax 0 :type fixnum)
  ;; Minimal bounding rectangle of the glyph (for stencil ops).
  (x1 0 :type fixnum)
  (y1 0 :type fixnum)
  (x2 0 :type fixnum)
  (y2 0 :type fixnum))

(defclass cached-truetype-font (truetype-font)
  ;; FIXME cache pixarrays.
  (;(all-glyph-data :initform (make-hash-table :size ))
   (ltr-glyph-info :initform (make-hash-table :size 512))
   (rtl-glyph-info :initform (make-hash-table :size 512))
   (ttb-glyph-info :initform (make-hash-table :size 512))
   (btt-glyph-info :initform (make-hash-table :size 512))))

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
      (multiple-value-bind (arr origin-x origin-y advance-x advance-y
                            xmin ymin xmax ymax x1 y1 x2 y2)
          (make-glyph-pixarray font char next direction)
        (make-glyph-info code arr
                         origin-x origin-y advance-x advance-y
                         xmin ymin xmax ymax x1 y1 x2 y2)))))


(deftype index () `(integer 0 #.array-dimension-limit))

(defun line-metrics (medium font string start end)
  (let ((cursor-dx 0)
        (cursor-dy 0)
        (x1 0) (y1 0) (x2 0) (y2 0)
        (xmin 0) (ymin 0) (xmax 0) (ymax 0))
    (labels ((process-code (code)
               (let ((glyph (font-glyph-info font code (medium-line-direction medium))))
                 ;; Minimal bounding box (stencil ops)
                 (minf x1 (+ cursor-dx (glyph-info-x1 glyph)))
                 (minf y1 (+ cursor-dy (glyph-info-y1 glyph)))
                 (maxf x2 (+ cursor-dx (glyph-info-x2 glyph)))
                 (maxf y2 (+ cursor-dy (glyph-info-y2 glyph)))
                 ;; Maximal bounding box (alignment)
                 (minf xmin (+ cursor-dx (glyph-info-xmin glyph)))
                 (minf ymin (+ cursor-dy (glyph-info-ymin glyph)))
                 (maxf xmax (+ cursor-dx (glyph-info-xmax glyph)))
                 (maxf ymax (+ cursor-dy (glyph-info-ymax glyph)))
                 ;; Cursor advancement
                 (incf cursor-dx (glyph-info-advance-dx glyph))
                 (incf cursor-dy (glyph-info-advance-dy glyph)))))
      (map-over-string-glyph-codes #'process-code string start end)
      (ecase (medium-line-direction medium)
        ((:left-to-right :right-to-left))
        ((:top-to-bottom :bottom-to-top)
         (rotatef xmin ymin)
         (rotatef xmax ymax)
         (rotatef cursor-dx cursor-dy)))
      (values cursor-dx cursor-dy xmin ymin xmax ymax x1 y1 x2 y2))))

(defun fill-glyph-indexes (medium font string start end glyph-ids)
  (let ((idx 0)
        (direction (medium-line-direction medium)))
    (flet ((process (code)
             (let ((info (font-glyph-info font code direction)))
               (setf (aref glyph-ids idx) (glyph-info-id info))
               (incf idx))))
      (map-over-string-glyph-codes #'process string start end))))

(defun font-prepare-glyphs (medium font string start end align-x align-y)
  (declare ;(optimize (speed 3))
           (type index start end)
           (type string string))
  (when (>= start end)
    (return-from font-prepare-glyphs (values 0 0 0 0 0 0)))
  (multiple-value-bind (cursor-dx cursor-dy xmin ymin xmax ymax x1 y1 x2 y2)
      (line-metrics medium font string start end)
    (ecase (medium-line-direction medium)
      ((:left-to-right :right-to-left))
      ((:top-to-bottom :bottom-to-top)
       ;; Vertical metric natural orientation is top-to-bottom while alignment
       ;; values are specified in terms of left-to-right. Thus:
       (rotatef cursor-dx cursor-dy)
       (rotatef xmin ymin)
       (rotatef xmax ymax)
       (psetf align-x (ecase align-y
                        ((:baseline :center) align-y)
                        (:top :right)
                        (:bottom :left))
              align-y (ecase align-x
                        ((:baseline :center) align-x)
                        (:left :top)
                        (:right :bottom)))))
    (let* ((after xmax)
           (below ymax)
           (sw (- after xmin))
           (sh (- below ymin)))
      (case align-x
        (:left   (setf after sw))
        (:right  (setf after 0))
        (:center (setf after (/ sw 2.0))))
      (case align-y
        (:top    (setf below sh))
        (:bottom (setf below 0))
        (:center (setf below (/ sh 2.0))))
      (let* ((prior (- after sw))
             (above (- below sh))
             (dx (- prior xmin))
             (dy (- above ymin)))
        #+ (or)                         ; maximal
        (values (- prior xmin)
                (- above ymin)
                prior above
                after below)
        #- (or)                         ; minimal
        (values dx dy
                (+ x1 dx) (+ y1 dy)
                (+ x2 dx) (+ y2 dy))))))


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
   (preload :initarg :preload :reader device-font-preload)))

(defmethod text-style-components ((style ttf-device-text-style))
  (with-slots (size) style
    (values :device :device size)))

(defmethod make-device-font-text-style ((port ttf-port-mixin) font)
  (destructuring-bind (path &key (size :normal) (preload nil) &allow-other-keys)
      (if (listp font) font (list font))
    (make-instance 'ttf-device-text-style
                   :path path :size size :preload preload
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

(defun text-transformation (medium x0 y0 x1 y1 line-direction transform-glyphs)
  (labels ((draw-text-rotation (fx fy tx ty)
             ;; Rounding here is important to ensure a numerical stability of rotation.
             (let* ((dx (- (round-coordinate tx) (round-coordinate fx)))
                    (dy (- (round-coordinate ty) (round-coordinate fy)))
                    (angle (ecase line-direction
                             ((:left-to-right :right-to-left) (find-angle 1 0 dx dy))
                             ((:top-to-bottom :bottom-to-top) (find-angle 0 1 dx dy)))))
               (make-rotation-transformation* angle 0 0)))
           (text-transformation (fx fy tx ty)
             (if (ecase line-direction
                   ((:left-to-right :right-to-left) (and (= fy ty) (< fx tx)))
                   ((:top-to-bottom :bottom-to-top) (and (< fy ty) (= fx tx))))
                 (make-translation-transformation fx fy)
                 (compose-transformations
                  (make-translation-transformation fx fy)
                  (draw-text-rotation fx fy tx ty)))))
    (if transform-glyphs
        (compose-transformations (medium-device-transformation medium)
                                 (text-transformation x0 y0 x1 y1))
        (with-transformed-positions* ((medium-transformation medium) x0 y0 x1 y1)
          (compose-transformations (medium-native-transformation medium)
                                   (text-transformation x0 y0 x1 y1))))))

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

(defmethod text-bounding-rectangle* ((medium ttf-medium-mixin) string
                                     &key text-style (start 0) end)
  (setf string (string string)
        end (or end (length string)))
  (let ((font (text-style-mapping (port medium)
                                  (merge-text-styles
                                   text-style
                                   (medium-merged-text-style medium)))))
    (multiple-value-bind (cursor-dx cursor-dy xmin ymin xmax ymax)
        (line-metrics medium font string start end)
      (declare (ignore cursor-dx cursor-dy))
      (values xmin ymin xmax ymax))))

(defmethod text-size ((medium ttf-medium-mixin) string &key text-style (start 0) end)
  (setf string (string string)
        end (or end (length string)))
  (when (>= start end)
    (return-from text-size
      (values (text-style-width text-style medium)
              (text-style-height text-style medium)
              0 0
              (text-style-ascent text-style medium))))
  (let* ((font (text-style-mapping (port medium)
                                   (merge-text-styles
                                    text-style
                                    (medium-merged-text-style medium))))
         (baseline (ecase (medium-line-direction medium)
                     ((:left-to-right :right-to-left)
                      (font-ascent font))
                     ((:top-to-bottom :bottom-to-top)
                      (font-vascent font)))))
    (multiple-value-bind (cursor-dx cursor-dy xmin ymin xmax ymax)
        (line-metrics medium font string start end)
      (values (- xmax xmin) (- ymax ymin)
              cursor-dx cursor-dy
              baseline))))

;;; Alternative version could take pixmaps from font-glyph-info, convert them to
;;; patterns and call draw-design on that. This would be faster than consing new
;;; polygons each time, but our goal is to go straight from paths.
;;;
;;; ORIGIN-X and ORIGIN-Y are relative to glyph pixmap, while polygons are
;;; specified relative to the drawing origin, that's why we compute offset.
;;; Compare with the function GLYPH-INFO-ADVANCE.
#+ (or)
(defun naive-render-composite-glyphs (font glyph-codes
                                      medium x y transformation direction)
  #+ (or)
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
  #+ (or)
  (let* ((start (or start 0))
         (end (or end (length string)))
         (direction (climi::medium-line-direction medium))
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

