;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Windowing protocol (with extensions).
;;;
(in-package "CLIM-INTERNALS")

;;; Geometry Substrate

;;; 3 Regions

;;; 3.1 General Regions
(define-protocol-class design nil)
(define-protocol-class region (design))
(define-protocol-class path (region bounding-rectangle))
(define-protocol-class area (region bounding-rectangle))
(pledge :type coordinate)
(declfun coordinate (n))
(declfun coordinate-epsilon nil)
(declfun coordinate= (x y))
(declfun coordinate/= (x y))
(declfun coordinate<= (x y))
(declfun coordinate-between (c1 x c2))
(declfun coordinate-between* (low x high))
(pledge :type standard-rectangle-coordinate-vector)
(pledge :class nowhere-region (region))
(pledge :class everywhere-region (region))
(pledge :constant +everywhere+ region)
(pledge :constant +nowhere+ region)

;;; 3.1.1 The Region Predicate Protocol
(defgeneric region-equal (region1 region2))
(defgeneric region-contains-region-p (region1 region2))
(defgeneric region-contains-position-p (region x y))
(defgeneric region-intersects-region-p (region1 region2))

;;; 3.1.2 Region Composition Protocol
(define-protocol-class region-set (region bounding-rectangle))
(pledge :class standard-region-union (region-set))
(pledge :class standard-region-intersection (region-set))
(pledge :class standard-region-difference (region-set))
(pledge :class standard-region-complement (region-set))
(pledge :class standard-rectangle-set (region-set))
(defgeneric region-set-regions (region &key normalize))
(defgeneric map-over-region-set-regions (fun region &key normalize))
(defgeneric region-union (region1 region2))
(defgeneric region-intersection (region1 region2))
(defgeneric region-difference (region1 region2))
(defgeneric region-complement (region))
(declfun region-exclusive-or (region1 region2))
(pledge :condition region-set-not-rectangular (error))

;;; 3.2 Other Region Types

;;; 3.2.1 Points
(define-protocol-class point (region bounding-rectangle))
(pledge :class standard-point (point))
(declfun make-point (x y))
(defgeneric point-position (point))
(defgeneric point-x (point))
(defgeneric point-y (point))

;;; 3.2.2 Polygons and Polylines
(define-protocol-class polygon (bezigon))
(define-protocol-class polyline (polybezier))
(pledge :class standard-polygon (polygon))
(declfun make-polygon (point-seq))
(declfun make-polygon* (coord-seq))
(pledge :class standard-polyline (standard-polyline))
(declfun make-polyline (point-seq &key closed))
(declfun make-polyline* (coord-seq &key closed))
(defgeneric polygon-points (polygon-or-polyline))
(defgeneric map-over-polygon-coordinates (fun polygon-or-polyline))
(defgeneric map-over-polygon-segments (fun polygon-or-polyline))
(defgeneric polyline-closed (polyline))

;;; 3.2.3 Lines
(define-protocol-class line (polyline))
(pledge :class standard-line (line))
(declfun make-line (start-point end-point))
(declfun make-line* (x1 y1 x2 y2))
(defgeneric line-start-point* (line))
(defgeneric line-end-point* (line))
(defgeneric line-start-point (line))
(defgeneric line-end-point (line))

;;; 3.2.4 Rectangles
(define-protocol-class rectangle (polygon))
(pledge :class standard-rectangle (rectangle))
(declfun make-rectangle (point1 point2))
(declfun make-rectangle* (x1 y1 x2 y2))
(defgeneric rectangle-edges* (rectangle))
(defgeneric rectangle-min-point (rectangle))
(defgeneric rectangle-max-point (rectangle))
(defgeneric rectangle-min-x (rectangle))
(defgeneric rectangle-min-y (rectangle))
(defgeneric rectangle-max-x (rectangle))
(defgeneric rectangle-max-y (rectangle))
(defgeneric rectangle-width (rectangle))
(defgeneric rectangle-height (rectangle))
(defgeneric rectangle-size (rectangle))

;;; 3.2.5 Ellipses and Elliptical Arcs
(define-protocol-class ellipse (area))
(define-protocol-class elliptical-arc (path))
(pledge :class standard-ellipse (ellipse))
(declfun make-ellipse (center rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(declfun make-ellipse* (cx cy rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(pledge :class standard-elliptical-arc (elliptical-arc))
(declfun make-elliptical-arc (center rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(declfun make-elliptical-arc* (cx cy rdx1 rdy1 rdx2 rdy1 &key start-angle end-angle))
(defgeneric ellipse-center-point* (elliptical-object))
(defgeneric ellipse-center-point (elliptical-object))
(defgeneric ellipse-radii (elliptical-object))
(defgeneric ellipse-start-angle (elliptical-object))
(defgeneric ellipse-end-angle (elliptical-object))

;;; 3.2.6 The Bezigon and Bezier Curve Protocol (McCLIM extension)
(define-protocol-class bezigon (area))
(define-protocol-class polybezier (path))
(pledge :class standard-bezigon (bezigon))
(declfun make-bezigon (point-seq))
(declfun make-bezigon* (coord-seq))
(pledge :class standard-polybezier (polybezier))
(declfun make-polybezier (point-seq))
(declfun make-polybezier* (coord-seq))
(defgeneric bezigon-points (object))
(defgeneric bezigon-order (object))
(defgeneric map-over-bezigon-segments (fun object))
(declfun map-over-bezigon-segments* (fun coord-seq order))

;;; 4 Bounding Rectangles

;;; 4.1 Bounding Rectangles
(define-protocol-class bounding-rectangle nil)
(pledge :class standard-bounding-rectangle (bounding-rectangle rectangle))
(declfun make-bounding-rectangle (x1 y1 x2 y2))
(defgeneric bounding-rectangle* (region))
(defgeneric bounding-rectangle (region))
(pledge :macro with-bounding-rectangle* ((&rest variables) region &body body))
(pledge :macro with-standard-rectangle* ((&rest variables) rectangle &body body))
(defgeneric bounding-rectangle-position (region))
(defgeneric bounding-rectangle-min-x (region))
(defgeneric bounding-rectangle-min-y (region))
(defgeneric bounding-rectangle-max-x (region))
(defgeneric bounding-rectangle-max-y (region))
(defgeneric bounding-rectangle-width (region))
(defgeneric bounding-rectangle-height (region))
(defgeneric bounding-rectangle-size (region))
(declfun copy-bounding-rectangle (region))
(declfun rounded-bounding-rectangle (region))

;;; 5 Affine Transformations

;;; 5.1 Transformations
(define-protocol-class transformation nil)
(declfun get-transformation (transformation))
(pledge :class standard-transformation (transformation))
(pledge :class standard-identity-transformation)
(pledge :class standard-translation (transformation))
(pledge :class standard-hairy-transformation (transformation))
(pledge :constant +identity-transformation+ transformation)
(pledge :macro with-transformed-position ((transformation x y) &body body))
(pledge :macro with-transformed-distance ((transformation dx dy) &body body))
(pledge :macro with-transformed-angles ((transformation clockwisep &rest angles) &body body))
(pledge :macro with-transformed-positions ((transformation coord-seq) &body body))
(pledge :macro with-transformed-positions* ((transformation &rest coord-seq) &body body))
(declfun transform-angle (transformation phi))
(declfun untransform-angle (transformation phi))

;;; 5.1.1 Transformation Conditions
(pledge :condition transformation-error (error) (:initargs :points))
(pledge :condition transformation-underspecified (error) (:initargs :points))
(pledge :condition reflection-underspecified (error) (:initargs :points))
(pledge :condition singular-transformation (error) (:initargs :transformation))
(pledge :condition rectangle-transformation-error (error) (:initargs :rect))

;;; 5.2 Transformation Constructors
(declfun make-translation-transformation (dx dy))
(declfun make-rotation-transformation (angle &optional origin))
(declfun make-rotation-transformation* (angle &optional x0 y0))
(declfun make-scaling-transformation (sx sy &optional origin))
(declfun make-scaling-transformation* (sx sy &optional x0 y0))
(declfun make-reflection-transformation (point1 point2))
(declfun make-reflection-transformation* (x1 y1 x2 y2))
(declfun make-transformation (mxx mxy myx myy tx ty))
(declfun make-3-point-transformation (p1 p2 p3 p1* p2* p3*))
(declfun make-3-point-transformation* (x1 y1 x2 y2 x3 y3 x1* y1* x2* y2* x3* y3*))

;;; 5.3 Transformation Protocol

;;; 5.3.1 Transformation Predicates
(defgeneric transformation-equal (transformation1 transformation2))
(defgeneric identity-transformation-p (transformation))
(defgeneric invertible-transformation-p (transformation))
(defgeneric translation-transformation-p (transformation))
(defgeneric reflection-transformation-p (transformation))
(defgeneric rigid-transformation-p (transformation))
(defgeneric even-scaling-transformation-p (transformation))
(defgeneric scaling-transformation-p (transformation))
(defgeneric rectilinear-transformation-p (transformation))
;;; McCLIM extension:
(defgeneric y-inverting-transformation-p (transformation))

;;; 5.3.2 Composition of Transformations
(defgeneric compose-transformations (transformation1 transformation2))
(defgeneric invert-transformation (transformation))
(declfun compose-translation-with-transformation (transformation dx dy))
(declfun compose-scaling-with-transformation (transformation sx sy &optional origin))
(declfun compose-rotation-with-transformation (transformation angle &optional origin))
(declfun compose-transformation-with-translation (transformation dx dy))
(declfun compose-transformation-with-scaling (transformation sx sy &optional origin))
(declfun compose-transformation-with-rotation (transformation angle &optional origin))

;;; 5.3.3 Applying Transformations
(defgeneric transform-region (transformation region))
(defgeneric untransform-region (transformation region))
(defgeneric transform-position (transformation x y))
(declfun transform-positions (transformation coord-seq))
(declfun transform-position-sequence (seq-type transformation coord-seq))
(defgeneric untransform-position (transformation x y))
(defgeneric transform-distance (transformation dx dy))
(defgeneric untransform-distance (transformation dx dy))
(defgeneric transform-rectangle* (transformation x1 y1 x2 y2))
(defgeneric untransform-rectangle* (transformation x1 y1 x2 y2))
