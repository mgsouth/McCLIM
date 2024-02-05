;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Drawing protocol (with extensions).
;;;
(in-package "CLIM-INTERNALS")

;;; Sheet and Medium Output Facilities

;;; 10 Drawing Options

;;; 10.1 Medium Components
(define-protocol-class medium nil)
(pledge :class basic-medium (medium))
(defgeneric medium-background (instance))
(defgeneric (setf medium-background) (new-value instance))
(defgeneric medium-foreground (instance))
(defgeneric (setf medium-foreground) (new-value instance))
(defgeneric medium-ink (instance))
(defgeneric (setf medium-ink) (new-value instance))
(defgeneric medium-transformation (instance))
(defgeneric (setf medium-transformation) (new-value instance))
(defgeneric medium-clipping-region (instance))
(defgeneric (setf medium-clipping-region) (new-value instance))
(defgeneric medium-line-style (instance))
(defgeneric (setf medium-line-style) (new-value instance))
(defgeneric medium-default-text-style (instance))
(defgeneric (setf medium-default-text-style) (new-value instance))
(defgeneric medium-text-style (instance))
(defgeneric (setf medium-text-style) (new-value instance))
(defgeneric medium-current-text-style (medium))
(defgeneric medium-merged-text-style (medium))
(defgeneric medium-sheet (instance))
(defgeneric medium-drawable (instance))
(defgeneric medium-device-transformation (instance))
(defgeneric medium-device-region (instance))
(defgeneric medium-native-transformation (instance))
(defgeneric medium-native-region (instance))
(pledge :class graphics-state)
(pledge :mixin gs-transformation-mixin)
(pledge :mixin gs-ink-mixin)
(pledge :mixin gs-clip-mixin)
(pledge :mixin gs-line-style-mixin)
(pledge :mixin gs-text-style-mixin)
(pledge :mixin complete-medium-state)
(defgeneric graphics-state-transformation (instance))
(defgeneric graphics-state-ink (instance))
(defgeneric graphics-state-clip (instance))
(defgeneric graphics-state-line-style (instance))
(defgeneric graphics-state-text-style (instance))
(defgeneric graphics-state-line-style-border (record medium))
(defgeneric (setf graphics-state) (new-value graphics-state))

;;; 10.2 Drawing Option Binding Forms
(pledge :macro with-medium-options ((medium args) &body body))
(pledge :macro with-drawing-options ((medium &rest drawing-options &key ink transformation clipping-region line-style text-style &allow-other-keys) &body body))
(defgeneric invoke-with-drawing-options (medium cont &rest drawing-options &key &allow-other-keys))

;;; 10.2.1 Transformation "Convenience" Forms
(pledge :macro with-translation ((medium dx dy) &body body))
(pledge :macro with-scaling ((medium sx &optional sy origin) &body body))
(pledge :macro with-rotation ((medium angle &optional origin) &body body))
(pledge :macro with-identity-transformation)
(defgeneric invoke-with-identity-transformation (medium continuation))
(declfun invoke-with-local-coordinates (medium continuation x y))
(declfun invoke-with-first-quadrant-coordinates (medium continuation x y))

;;; 10.2.2 Estabilishing Local Coordinate System
(pledge :macro with-local-coordinates ((medium &optional x y) &body body))
(pledge :macro with-first-quadrant-coordinates ((medium &optional x y) &body body))

;;; 10.3 Line Styles
(define-protocol-class line-style nil nil (:default-initargs :line-unit :normal :line-thickness 1 :line-joint-shape :miter :line-cap-shape :butt :line-dashes nil))
(pledge :class standard-line-style (line-style))
(declfun make-line-style (&key unit thickness joint-shape cap-shape dashes))
(defgeneric line-style-equalp (style1 style2))

;;; 10.3.1 Line Style Protocol and Line Style Suboptions
(defgeneric line-style-unit (instance))
(defgeneric line-style-thickness (instance))
(defgeneric line-style-joint-shape (instance))
(defgeneric line-style-cap-shape (instance))
(defgeneric line-style-dashes (instance))

;;; 10.3.2 Contrasting Dash Patterns
(declfun make-contrasting-dash-patterns (n &optional k))
(defgeneric contrasting-dash-pattern-limit (port))

;;; 11 Text Styles

;;; 11.1 Text Style
(define-protocol-class text-style nil nil)
(pledge :class standard-text-style (text-style))
(defgeneric text-style-equalp (style1 style2))
(declfun make-text-style (family face size))
(pledge :constant *default-text-style*)
(pledge :constant *undefined-text-style*)

;;; 11.1.1 Text Style Protocol and Text Style Suboptions
(defgeneric text-style-components (instance))
(defgeneric text-style-family (instance))
(defgeneric text-style-face (instance))
(defgeneric text-style-size (instance))
(declfun parse-text-style (style-spec))
(declfun parse-text-style* (style))
(declfun normalize-font-size (size))
(defgeneric merge-text-styles (style1 style2))
(defgeneric text-style-ascent (text-style medium))
(defgeneric text-style-descent (text-style medium))
(defgeneric text-style-width (text-style medium))
(defgeneric text-style-height (text-style medium))
(defgeneric text-style-fixed-width-p (text-style medium))
(defgeneric text-size (medium string &key text-style start end))
(defgeneric text-style-leading (text-style medium) (:method (text-style medium) 1.2))
(defgeneric text-style-character-width (text-style medium char) (:method (text-style medium char) (text-size medium char :text-style text-style)))
(defgeneric text-bounding-rectangle* (medium string &key text-style start end align-x align-y direction))

;;; 11.2 Text Style Binding Forms
(pledge :macro with-text-style ((medium text-style) &body body))
(defgeneric invoke-with-text-style (medium cont text-style))
(pledge :macro with-text-family ((medium family) &body body))
(pledge :macro with-text-face ((medium face) &body body))
(pledge :macro with-text-size ((medium size) &body body))

;;; 11.3 Controlling Text Style Mappings
(defgeneric text-style-mapping (port text-style &optional character-set))
(defgeneric (setf text-style-mapping) (mapping port text-style &optional character-set))
(pledge :class device-font-text-style (text-style))
(declfun device-font-text-style-p (object))
(defgeneric make-device-font-text-style (display-device device-font-name))
(defgeneric device-font-name (instance))

;;; 12 Graphics

;;; 12.5 Drawing Functions

;;; 12.5.1 Basic Drawing Functions
(pledge :macro def-sheet-trampoline (name (&rest args)))
(pledge :macro def-graphic-op (name (&rest args)))
(declfun draw-point (sheet point &rest drawing-options &key &allow-other-keys))
(declfun draw-point* (sheet x y &rest drawing-options &key  &allow-other-keys))
(declfun draw-points (sheet point-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-points* (sheet position-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-line (sheet point1 point2 &rest drawing-options &key &allow-other-keys))
(declfun draw-line* (sheet x1 y1 x2 y2 &rest drawing-options &key &allow-other-keys))
(declfun draw-lines (sheet point-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-lines* (sheet position-seq &rest drawing-options &key &allow-other-keys))
(declfun draw-polygon (sheet point-seq &rest drawing-options &key (filled t) (closed t) &allow-other-keys))
(declfun draw-polygon* (sheet position-seq &rest drawing-options &key (filled t) (closed t) &allow-other-keys))
(declfun draw-rectangle (sheet point1 point2 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rectangle* (sheet x1 y1 x2 y2 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rectangles (sheet points &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rectangles* (sheet position-seq &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-ellipse (sheet center rdx1 rdy1 rdx2 rdy2 &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-ellipse* (sheet cx cy rdx1 rdy1 rdx2 rdy2 &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-circle (sheet center radius &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-circle* (sheet cx cy radius &rest drawing-options &key (filled t) (start-angle 0.0) (end-angle (* 2.0 pi)) &allow-other-keys))
(declfun draw-text (sheet text point &rest drawing-options &key (start 0) (end nil) (align-x :left) (align-y :baseline) (toward-point nil toward-point-p) transform-glyphs &allow-other-keys))
(declfun draw-text* (sheet text x y &rest drawing-options &key (start 0) (end nil) (align-x :left) (align-y :baseline) (toward-x (1+ x)) (toward-y y) transform-glyphs &allow-other-keys))
(declfun draw-triangle (sheet point1 point2 point3 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-triangle* (sheet x1 y1 x2 y2 x3 y3 &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-bezigon (sheet point-seq &rest drawing-args &key (filled t) (closed t) &allow-other-keys))
(declfun draw-bezigon* (sheet position-seq &rest drawing-args &key (filled t) (closed t) &allow-other-keys))
(declfun draw-image (sheet pattern point &rest drawing-options))
(declfun draw-image* (sheet pattern x y &rest drawing-options))

;;; 12.5.2 Compound Drawing Functions
(declfun draw-arrow (medium point1 point2 &rest drawing-options &key (to-head t) from-head (head-length 10) (head-width 5) (head-filled nil) angle &allow-other-keys))
(declfun draw-arrow* (medium x1 y1 x2 y2 &rest drawing-options &key (to-head t) from-head (head-length 10) (head-width 5) (head-filled nil) angle &allow-other-keys))
(declfun draw-oval (medium center rx ry &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-oval* (medium cx cy rx ry &rest drawing-options &key (filled t) &allow-other-keys))
(declfun draw-rounded-rectangle* (sheet x1 y1 x2 y2 &rest args &key (radius 7) (radius-x radius) (radius-y radius) (radius-left radius-x) (radius-right radius-x) (radius-top radius-y) (radius-bottom radius-y) filled &allow-other-keys))

;;; 12.6 Pixmaps
(defgeneric allocate-pixmap (medium width height))
(defgeneric deallocate-pixmap (pixmap))
(defgeneric pixmap-width (instance))
(defgeneric pixmap-height (instance))
(defgeneric pixmap-depth (instance))
(defgeneric copy-to-pixmap (source src-x src-y width height &optional pixmap dst-x dst-y))
(defgeneric copy-from-pixmap (pixmap src-x src-y width height destination dst-x dst-y))
(defgeneric copy-area (medium src-x src-y width height dst-x dst-y))
(defgeneric medium-copy-area (source src-x src-y width height destination dst-x dst-y))
(pledge :macro with-output-to-pixmap ((medium-var medium &key width height) &key body))
(defgeneric invoke-with-output-to-pixmap (medium cont &key width height))

;;; 12.7 Graphics Protocols

;;; 12.7.2 Medium-specific Drawing Functions
(defgeneric medium-draw-point* (medium x y))
(defgeneric medium-draw-points* (medium coord-seq))
(defgeneric medium-draw-line* (medium x1 y1 x2 y2))
(defgeneric medium-draw-lines* (medium coord-seq))
(defgeneric medium-draw-polygon* (medium coord-seq closed filled))
(defgeneric medium-draw-rectangles* (medium coord-seq filled))
(defgeneric medium-draw-rectangle* (medium left top right bottom filled))
(defgeneric medium-draw-ellipse* (medium cx cy rdx1 rdy1 rdx2 rdy2 start-angle end-angle filled))
(defgeneric medium-draw-text* (medium string x y start end align-x align-y toward-x toward-y transform-glyphs))
(defgeneric medium-draw-bezigon* (medium coord-seq closed filled))
(defgeneric medium-draw-pattern* (medium pattern x y))

;;; 12.7.3 Other Medium-specific Output Functions
(defgeneric medium-finish-output (medium))
(defgeneric medium-force-output (medium))
(defgeneric medium-clear-area (medium x1 y1 x2 y2))
(defgeneric medium-beep (medium))
(defgeneric beep (&optional medium))
(defgeneric medium-buffering-output-p (instance))
(defgeneric (setf medium-buffering-output-p) (new-value instance))
(pledge :macro with-output-buffered (medium &optional (buffer-p t)))
(defgeneric invoke-with-output-buffered (medium cont &optional buffered-p))
(pledge :macro with-output-to-drawing-stream ((stream backend destination &rest args) &body body))
(defgeneric invoke-with-output-to-drawing-stream (cont backend destination &key &allow-other-keys))

(defgeneric medium-miter-limit (medium)
  (:documentation
   "If LINE-STYLE-JOINT-SHAPE is :MITER and the angle between two
   consequent lines is less than the values return by
   MEDIUM-MITER-LIMIT, :BEVEL is used instead."))

(defgeneric line-style-effective-thickness (line-style medium)
  (:documentation
   "Returns the thickness in device units of a line,
rendered on MEDIUM with the style LINE-STYLE."))

(defgeneric line-style-effective-dashes (line-style medium)
  (:documentation
   "Return a dash length or a sequence of dash lengths device units
for a dashed line, rendered on MEDIUM with the style LINE-STYLE."))

;;; 13 Drawing in Color

;;; 13.3 Color
(define-protocol-class color (design))
(pledge :class standard-color (color))
(declfun make-rgb-color (red green blue))
(declfun make-ihs-color (intensity hue saturation))
(declfun make-gray-color (luminance))
(declfun make-named-color (name red green blue))
(defgeneric color-rgb (instance))
(defgeneric color-ihs (instance))
(defgeneric color-rgba (instance))
(defgeneric highlight-shade (ink)
  (:documentation
   "Produce an alternate shade of the given ink for the purpose of highlighting.
    Typically the ink will be brightened, but very light inks may be darkened."))

;;; 13.3.1 Standard Color Names and Constants
(pledge :constant +red+)
(pledge :constant +green+)
(pledge :constant +blue+)
(pledge :constant +cyan+)
(pledge :constant +magenta+)
(pledge :constant +yellow+)
(pledge :constant +black+)
(pledge :constant +white+)

;;; 13.3.2 Contrastin Colors
(declfun make-contrasting-inks (n &optional k))
(defgeneric contrasting-inks-limit (instance))

;;; 13.4 Opacity
(define-protocol-class opacity (design))
(declfun make-opacity (value))
(pledge :constant +transparent-ink+)
(defgeneric opacity-value (instance))

;;; 13.6 Indirect Inks
(pledge :constant +foreground-ink+)
(pledge :constant +background-ink+)
(pledge :variable *foreground-ink*)
(pledge :variable *background-ink*)
(pledge :class indirect-ink (design))
(declfun indirect-ink-p (design))
(declfun indirect-ink-ink (indirect-ink))

;;; 13.7 Flipping Ink
(pledge :class standard-flipping-ink (design))
(defgeneric make-flipping-ink (design1 design2))
(pledge :constant +flipping-ink+)
(defgeneric flipping-ink-design1 (instance))
(defgeneric flipping-ink-design2 (instance))

;;; 14 General Design

;;; 14.1 The Compositing Protocol
(defgeneric compose-over (design1 design2))
(defgeneric compose-in (ink mask))
(defgeneric compose-out (ink mask))

;;; 14.2 Patterns and Stencils
(declfun make-pattern (array designs))
(declfun make-stencil (array))
(declfun make-rectangular-tile (design width height))
(declfun make-pattern-from-bitmap-file (pathname &key format designs))
(pledge :variable *bitmap-file-readers*)
(pledge :variable *bitmap-file-writers*)
(pledge :macro define-bitmap-file-reader (bitmap-format (&rest args) &body body))
(pledge :macro define-bitmap-file-writer (format (&rest args) &body body))
(declfun bitmap-format-supported-p (format))
(declfun bitmap-output-supported-p (format))
(pledge :condition unsupported-bitmap-format (error))
(declfun read-bitmap-file (pathname &key (format :bitmap)))
(declfun write-bitmap-file (image pathname &key (format :bitmap)))
(defgeneric pattern-width (instance))
(defgeneric pattern-height (instance))
(define-protocol-class pattern (design) ()
  (:documentation "Abstract class for all pattern-like designs."))
(pledge :class stencil (pattern))
(pledge :class indexed-pattern (pattern))
(pledge :class image-pattern (pattern))
(pledge :class rectangular-tile (pattern))
(pledge :class transformed-pattern (transformed-design pattern))
(defgeneric pattern-array (instance))
(defgeneric pattern-designs (instance))
(defgeneric transformed-design-design (instance))
(defgeneric transformed-design-transformation (instance))
(defgeneric rectangular-tile-design (instance))

;;; 14.5 Arbitrary Designs
(declfun make-uniform-compositum (ink opacity-value))
(pledge :class transformed-design (design))
(pledge :class masked-compositum (design))
(defgeneric compositum-mask (instance))
(defgeneric compositum-ink (instance))
(pledge :class in-compositum (masked-compositum))
(pledge :class uniform-compositum (in-compositum))
(pledge :class out-compositum (masked-compositum))
(pledge :class over-compositum (design))
(defgeneric compositum-foreground (instance))
(defgeneric compositum-background (instance))
(defgeneric design-ink (design x y))
(declfun design-ink* (design x y))
(defgeneric design-equalp (design1 design2))
(defgeneric draw-design (medium design &key ink filled clipping-region transformation line-style line-thickness line-unit line-dashes line-joint-shape line-cap-shape text-style text-family text-face text-size))
(declfun draw-pattern* (medium pattern x y &key clipping-region transformation))

;;; 14.7 Design Protocol

;;; Minor issue: The generic functions underlying the functions described in
;;; this and the preceding chapter will be documented later. This will allow for
;;; programmer-defined design classes. This also needs to describe how to decode
;;; designs into inks. --- SWM
