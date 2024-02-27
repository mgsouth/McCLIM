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
   (ascent                           :reader font-ascent)
   (descent                          :reader font-descent)
   (units->pixels                    :reader zpb-ttf-font-units->pixels))
  (:default-initargs :fixed nil :dpi 72 :kerning t))

(defgeneric font-port (font)
  (:method ((font truetype-font))
    (font-family-port (font-face-family (font-face font)))))

(defmethod initialize-instance :after
    ((font truetype-font) &key dpi &allow-other-keys)
  (with-slots (face size ascent descent font-loader) font
    (let* ((loader (zpb-ttf-font-loader face))
           (em->units (zpb-ttf:units/em loader))
           (dpi-factor (/ dpi 72))
           (units->pixels (/ (* size dpi-factor) em->units)))
      (setf ascent  (+ (* units->pixels (zpb-ttf:ascender loader)))
            descent (- (* units->pixels (zpb-ttf:descender loader)))
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
      (format stream " size=~A ascent=~A descent=~A units->pixels=~A"
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

(defun glyph-pixarray (font char next transformation)
  "Render a character of 'face', returning a 2D (unsigned-byte 8) array suitable
   as an alpha mask, and dimensions. This function returns seven values: alpha
   mask byte array, x-origin, y-origin (subtracted from position before
   rendering), glyph width and height, horizontal and vertical advances."
  (declare (optimize (debug 3)))
  (clim-sys:with-lock-held (*zpb-font-lock*)
    (with-slots (units->pixels size ascent descent) font
      (let* ((font-loader (zpb-ttf-font-loader (font-face font)))
             (glyph (zpb-ttf:find-glyph char font-loader))
             ;; (left-side-bearing  (* units->pixels (zpb-ttf:left-side-bearing  glyph)))
             ;; (right-side-bearing (* units->pixels (zpb-ttf:right-side-bearing glyph)))
             (hx (+ (* units->pixels (zpb-ttf:advance-width glyph))
                    (* units->pixels (zpb-ttf:kerning-offset char next font))))
             (hy 0)
             (vx 0)
             ;; FIXME zpb-ttf should consult horizontal metric tables, and
             ;; provide a sane fallback otherwise. -- jd 2024-02-23
             (vy (+ ascent descent))
             (bounding-box (map 'vector (lambda (x) (float (* x units->pixels)))
                                (zpb-ttf:bounding-box glyph)))
             (min-x (elt bounding-box 0))
             (min-y (elt bounding-box 1))
             (max-x (elt bounding-box 2))
             (max-y (elt bounding-box 3))
             width height left top right bottom array)
        (with-bounding-rectangle* (x1 y1 x2 y2)
            (transform-region transformation (make-rectangle* min-x min-y max-x max-y))
          (setq left (floor x1))
          (setq top (ceiling y2))
          (setq width  (- (ceiling x2) (floor x1)))
          (setq height (- (ceiling y2) (floor y1)))
          (setq right  (- hx width))
          (setq bottom (- vy height))
          (setq array (make-array (list height width)
                                  :initial-element 0
                                  :element-type '(unsigned-byte 8))))
        (let* ((glyph-tr (compose-transformations
                          (compose-transformations
                           (make-translation-transformation (- left) top)
                           (make-scaling-transformation units->pixels (- units->pixels)))
                          transformation))
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
        ;; Transformation is supplied in font coordinates for easy composition
        ;; with offset and scaling. advance values should be returned in screen
        ;; coordinates, so we transform it here.
        (let ((transformation (compose-transformations
                               #1=(make-scaling-transformation 1.0 -1.0)
                               (compose-transformations transformation #1#))))
          (multiple-value-setq (hx hy) (transform-distance transformation hx hy))
          (multiple-value-setq (vx vy) (transform-distance transformation vx vy)))
        (values array left top right bottom width height
                (climi::round-coordinate hx)
                (climi::round-coordinate hy)
                (climi::round-coordinate vx)
                (climi::round-coordinate vy))))))


(deftype glyph-pixarray () '(simple-array (unsigned-byte 8) (* *)))

(defstruct (glyph-info (:constructor make-glyph-info
                           (id pixarray width height
                            left right top bottom
                            advance-hx advance-hy
                            advance-vx advance-vy
                            origin-x origin-y
                            advance-dx advance-dy)))
  (id 0                      :type fixnum)
  (pixarray nil :read-only t :type (or null glyph-pixarray))
  ;; duplicates the pixarray dimensions
  (width 0      :read-only t)
  (height 0     :read-only t)
  ;; Bearings
  (left 0       :read-only t)
  (right 0      :read-only t)
  (top 0        :read-only t)
  (bottom 0     :read-only t)
  ;; Horizontal and vertical advance width and height.
  (advance-hx 0)
  (advance-hy 0)
  (advance-vx 0)
  (advance-vy 0)
  ;; Metrics configured for the particular font.
  (origin-x 0 :type fixnum)
  (origin-y 0 :type fixnum)
  (advance-dx 0 :type fixnum)
  (advance-dy 0 :type fixnum))

(defclass cached-truetype-font (truetype-font)
  ((char->glyph-info  :initform (make-hash-table :size 512))))

(defun font-glyph-info (font code)
  (with-slots (char->glyph-info) font
    (ensure-gethash code char->glyph-info
      (font-generate-glyph (font-port font) font code))))

(defgeneric font-generate-glyph (port font code &key &allow-other-keys)
  (:documentation "Truetype TTF renderer internal interface.")
  (:method (port (font cached-truetype-font) code
            &key (transformation +identity-transformation+))
    (declare (ignore port))
    (let ((character (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code)))
          (next-character (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                                #.(ceiling (log char-code-limit 2)))
                                          code)))
          (transformation (let ((scale (make-scaling-transformation 1.0 -1.0)))
                            (compose-transformations
                             scale (compose-transformations transformation scale)))))
      (multiple-value-bind (arr left top right bottom width height hx hy vx vy)
          (glyph-pixarray font character next-character transformation)
        (make-glyph-info code arr width height left right top bottom hx hy vx vy
                         ;; Horizontal metrics are default for origin + advance.
                         (- left) top hx hy)))))

(defun char-glyph-code (char next)
  (assert (and (char/= char #\newline)
               (not (eql next #\newline))))
  (if next
      (dpb (char-code next)
           (byte #.(ceiling (log char-code-limit 2))
                 #.(ceiling (log char-code-limit 2)))
           (char-code char))
      (char-code char)))

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

(defun font-string-glyph-codes (font string &key (start 0) (end (length string)))
  "Converts string to a sequence of glyph codes. Some characters are composed of
many codepoints – it is not guaranteed that length of the string and the length
of resulting sequence are equal."
  (declare (ignore font))
  (alexandria:minf end (length string))
  (when (>= start end)
    (return-from font-string-glyph-codes #()))
  (let ((array (make-array (- end start) :fill-pointer 0)))
    (flet ((doit (code) (vector-push code array)))
      (declare (dynamic-extent #'doit))
      (map-over-string-glyph-codes #'doit string start end))
    array))

(defun font-glyph-code-char (font code)
  (declare (ignore font))
  (values (code-char (ldb (byte #.(ceiling (log char-code-limit 2)) 0) code))
          (code-char (ldb (byte #.(ceiling (log char-code-limit 2))
                                #.(ceiling (log char-code-limit 2)))
                          code))))


(defun line-bbox (font string start end)
  (let ((origin-x 0)
        (origin-y 0)
        (xmin most-positive-fixnum)
        (ymin most-positive-fixnum)
        (xmax most-negative-fixnum)
        (ymax most-negative-fixnum))
    (flet ((process-code (code)
             (let ((glyph (font-glyph-info font code)))
               (minf xmin (+ origin-x (glyph-info-left glyph)))
               (minf ymin (+ origin-y (- (glyph-info-top glyph))))
               (maxf xmax (+ origin-x (glyph-info-right glyph)))
               (maxf ymax (+ origin-y (- (glyph-info-bottom glyph))))
               (incf origin-x (glyph-info-advance-dx glyph))
               (incf origin-y (glyph-info-advance-dy glyph)))))
      (map-over-string-glyph-codes #'process-code string start end)
      (values xmin ymin xmax ymax origin-x origin-y))))

(defun font-text-extents (font string &key start end direction)
  "Function computes text extents as if it were drawn with a specified font. It
returns two distinct extents: first is an exact pixel-wise bounding box. The
second is a text bounding box with all its bearings. Text may contain newlines,
if it doesn't linegap should be nil. Cursor advance is returned as the last two
values.

Width and height are relative to the position [-top, left]. For right-to-left
direction left will be probably a negative number with the width being close to
its absolute value. All other values are relative to the postion
origin. Coordinate system is in the fourth quadrant (same as sheet coordinates).

Returned values:

xmin ymin xmax ymax
left top width height ascent descent linegap
cursor-dx cursor-dy"
  (declare (ignore direction))
  (when (alexandria:emptyp string)
    (values 0 0 0 0 0 0))
  (let* ((ascent (font-ascent font))
         (descent (font-descent font))
         (line-height (+ ascent descent)))
    (multiple-value-bind (xmin ymin xmax ymax dx dy)
        (line-bbox font string start end)
      (values xmin ymin xmax ymax            ; text bbox
              0 ascent dx (+ dy line-height) ; x0 y0 xn yn
              ascent descent 0               ; ascent, descent, line gap
              dx dy                          ; cursor advancement
              ))))
