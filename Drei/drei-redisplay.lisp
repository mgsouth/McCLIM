;;; -*- Mode: Lisp; Package: DREI -*-

;;;  (c) copyright 2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2005 by
;;;           Aleksandar Bakic (a_bakic@yahoo.com)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Declarations and definitions of the generic functions and helper
;;; utilities needed for the Drei redisplay engine

(in-package :drei)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Display of Drei instances.
;;;
;;; Syntaxes can customize their redisplay (for things such as syntax
;;; highlighting, presentation types, etc), through specializing on
;;; the generic functions `display-drei-contents' and
;;; `display-drei-cursor'. Methods defined on these functions can
;;; assume that they are writing to a normal CLIM stream pane, but
;;; cannot expect that they are the only Drei instance on the stream,
;;; and cannot assume that they completely control the stream.  The
;;; redisplay scaffolding code will take care of packaging the output
;;; records generated by methods into something useful to the concrete
;;; Drei implementation.
;;;
;;; The basic Drei redisplay functions:

(defgeneric display-drei-contents (stream drei syntax)
  (:documentation "The purpose of this function is to display the
buffer contents of a Drei instance to some output
surface. `Stream' is the CLIM output stream that redisplay should
be performed on, `drei' is the Drei instance that is being
redisplayed, and `syntax' is the syntax object of the buffer in
`drei'. Methods defined for this generic function can draw
whatever they want, but they should not assume that they are the
only user of `stream', unless the `stream' argument has been
specialized to some application-specific pane class that can
guarantee this. For example, when accepting multiple values using
the `accepting-values' macro, several Drei instances will be
displayed simultaneously on the same stream. It is permitted to
only specialise `stream' on `clim-stream-pane' and not
`extended-output-stream'. When writing methods for this function,
be aware that you cannot assume that the buffer will contain only
characters, and that any subsequence of the buffer is coercable
to a string. Drei buffers can contain arbitrary objects, and
redisplay methods are required to handle this (though they are
not required to handle it nicely, they can just ignore the
object, or display the `princ'ed representation.)")
  (:method :around ((stream extended-output-stream) (drei drei) (syntax syntax))
           (letf (((stream-default-view stream) (view drei)))
             (call-next-method))))

;; XXX: If the display begins with a blank area - for example spaces -
;; CLIM will (rightly) think the output records position is at the
;; first output. This is not good, because it means that the output
;; record will "walk" across the screen if the buffer starts with
;; blanks. Therefore, we make sure that an output record exists at the
;; very beginning of the output.
(defmethod display-drei-contents :before ((stream extended-output-stream) (drei drei-area) syntax)
  (with-new-output-record (stream 'standard-sequence-output-record record)
    (setf (output-record-position record) (values-list (input-editor-position drei)))))

(defgeneric display-drei-cursor (stream drei cursor syntax)
  (:documentation "The purpose of this function is to display a
visible indication of a cursor of a Drei instance to some output
surface. `Stream' is the CLIM output stream that drawing should
be performed on, `drei' is the Drei instance that is being
redisplayed, `cursor' is the cursor object to be displayed (a
subclass of `drei-cursor') and `syntax' is the syntax object of
the buffer in `drei'}. Methods on this generic function can draw
whatever they want, but they should not assume that they are the
only user of `stream', unless the `stream' argument has been
specialized to some application-specific pane class that can
guarantee this. It is permitted to only specialise `stream' on
`clim-stream-pane' and not `extended-output-stream'. It is
recommended to use the function `offset-to-screen-position' to
determine where to draw the visual representation for the
cursor. It is also recommended to use the ink specified by
`cursor' to perform the drawing, if applicable. This method will
only be called by the Drei redisplay engine when the cursor is
active and the buffer position it refers to is on display -
therefore, `offset-to-screen-position' is *guaranteed* to not
return NIL or T.")
  (:method :around ((stream extended-output-stream) (drei drei)
                    (cursor drei-cursor) (syntax syntax))
           (when (visible cursor drei)
             (letf (((stream-default-view stream) (view drei)))
               (call-next-method)))))

(defmethod display-drei-cursor :after ((stream extended-output-stream) (drei drei)
                                       (cursor point-cursor) (syntax syntax))
  ;; Make sure there is room for the cursor.
  (let ((br-height (bounding-rectangle-height (bounding-rectangle cursor))))
    (when (> br-height (bounding-rectangle-height stream))
      (change-space-requirements stream :height br-height))))

(defgeneric record-line-vertical-offset (pane syntax line-number)
  (:documentation "Record the end of the line `line-number' (>=
1) as whereever the text-cursor is for `pane'."))

(defmethod record-line-vertical-offset ((pane clim-stream-pane) (syntax syntax) (line-number integer))
  (assert (plusp line-number))
  (with-accessors ((cursor-positions cursor-positions)) syntax
    (with-sheet-medium (medium pane)
      (multiple-value-bind (cursor-x cursor-y)
          (stream-cursor-position pane)
        (setf (aref cursor-positions line-number)
              (list cursor-x
                    (+ cursor-y
                       ;; FIXME: This isn't necessarily the height of
                       ;; the line. Effectively, this assumption
                       ;; prevents variable-height lines.
                       (text-style-height (pane-text-style pane)
                                          medium))))))))

(defgeneric line-vertical-offset (pane syntax line-number)
  (:documentation "Return the horizontal position (in device
units) of the line with the given number in `syntax' on
`pane'. `Pane' is used to get text style information and so
forth. `Line-number' should be >= 1."))

(defmethod line-vertical-offset ((pane clim-stream-pane) (syntax syntax) (line-number integer))
  (assert (>= line-number 0))
  (with-accessors ((cursor-positions cursor-positions)) syntax
    (with-sheet-medium (medium pane)
      (let ((prev-line-end-pos (when (> (length cursor-positions)
                                        line-number)
                                 (second (aref cursor-positions line-number))))
            (text-style-height (text-style-height (medium-text-style medium) medium)))
        (if (integerp prev-line-end-pos)
            ;; We recorded a position for the end of the previous line,
            ;; which means we can just add vertical stream spacing to
            ;; get the start offset of this line.
            (+ prev-line-end-pos (stream-vertical-spacing pane))
            ;; No end position for the previous line was recored. So
            ;; we find the latest recorded position, and add a
            ;; multiple of vertical spacing and standard line-height
            ;; for the missed lines.
            (destructuring-bind (found-line-number vertical-offset)
                (reduce #'(lambda (prev-data value)
                            (if value (list (1+ (first prev-data))
                                            (second value))
                                prev-data))
                        cursor-positions :initial-value '(0 0))
              (+ vertical-offset
                 (* (stream-vertical-spacing pane)
                    (- line-number (1- found-line-number)))
                 (* text-style-height
                    (- line-number (1- found-line-number))))))))))

(defun offset-x-displacement (pane line-beg-mark offset)
  (with-sheet-medium (medium pane)
    (let ((displacement 0)
          (style (medium-text-style pane)))
      (flet ((string-size (string)
               (text-size medium string :text-style style))
             (object-size (object)
               ;; FIXME: This can quickly become extremely slow and
               ;; expensive, find a better way.
               (bounding-rectangle-width
                (with-output-to-output-record (pane)
                  (present object (presentation-type-of object)
                           :stream pane)))))
        (loop
           with array = (make-array (- offset (offset line-beg-mark))
                                    :element-type 'character
                                    :initial-element #\null
                                    :adjustable t
                                    :fill-pointer 0)
           for go-again = (> offset (offset line-beg-mark))
           for p = (when go-again
                     (forward-object line-beg-mark))
           for object = (when go-again
                          (object-before line-beg-mark))
           while go-again
           when (characterp object)
           do (vector-push-extend object array)
           else do (progn (incf displacement (string-size array))
                          (incf displacement (object-size object))
                          (setf (fill-pointer array) 0))
           finally (incf displacement (string-size array))))
      displacement)))

(defgeneric offset-to-screen-position (pane drei offset)
  (:documentation "Returns the position of offset as a screen position.
Returns X Y LINE-HEIGHT CHAR-WIDTH as values if offset is on the screen,
NIL if offset is before the beginning of the screen,
and T if offset is after the end of the screen."))

(defmethod offset-to-screen-position ((pane clim-stream-pane) (drei drei) (offset number))
  (with-accessors ((buffer buffer)) drei
    (with-slots (top bot) drei
      (cond
        ((< offset (offset top)) nil)
        ((< (offset bot) offset) t)
        (t
         (let* ((line-number (number-of-lines-in-region top offset))
                (line-beg (let ((mark (clone-mark top)))
                            (setf (offset mark) offset)
                            (beginning-of-line mark)))
                (style (medium-text-style pane))
                (style-width (text-style-width style pane))
                (ascent (text-style-ascent style pane))
                (descent (text-style-descent style pane))
                (height (+ ascent descent))
                (y (line-vertical-offset pane (syntax buffer) line-number))
                (x (offset-x-displacement pane line-beg offset)))
           (values x y height style-width)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drei area redisplay.

(defmethod stream-add-output-record :after ((stream extended-output-stream)
                                            (drei drei-area))
  (dolist (cursor (cursors drei))
    (stream-add-output-record stream cursor)))

(defmethod erase-output-record :after ((drei drei-area) (stream extended-output-stream)
                                       &optional (errorp nil errorp-supplied))
  (dolist (cursor (cursors drei))
    (apply #'erase-output-record cursor stream
           (when errorp-supplied
             errorp))))

(defmethod bounding-rectangle* ((drei drei-area))
  (with-accessors ((pane editor-pane)
                   (min-width min-width)) drei
    (let* ((style (medium-text-style pane))
           (style-width (text-style-width style pane))
           (ascent (text-style-ascent style pane))
           (descent (text-style-descent style pane))
           (height (+ ascent descent)))
      (multiple-value-bind (x1 y1 x2 y2)
          (call-next-method)
        (values x1 y1 (max x2 (+ x1 style-width)
                           (cond ((numberp min-width)
                                  (+ x1 min-width))
                                 ;; Must be T, then.
                                 ((pane-viewport pane)
                                  (+ x1 (bounding-rectangle-width (pane-viewport-region pane))))
                                 (t 0)))
                (max y2 (+ y1 height)))))))

;; XXX: this :before-method on `replay-output-record' are responsible
;; for actually calling the output-generating functions. This works,
;; incremental redisplay functions and it seems to be fast, but I
;; don't think CLIM means us to do this kind of thing there.
(defmethod replay-output-record :before ((drei drei-area) (stream extended-output-stream) &optional
                                         (x-offset 0) (y-offset 0) (region +everywhere+))
  (declare (ignore x-offset y-offset region))
  (with-bounding-rectangle* (old-x1 old-y1 old-x2 old-y2) drei
    (clear-output-record drei)
    (draw-rectangle* stream old-x1 old-y1 old-x2 old-y2
                     :ink (background-ink drei)
                     :filled t)
    ;; XXX: Ugly, but McCLIM doesn't seem to handle +transparent-ink+,
    ;; so we can't implement inactive cursors by drawing
    ;; transparently, but only by not drawing at all. This means that
    ;; inactive cursor objects will have a null-size bounding
    ;; rectangle, preventing us from doing this more elegantly.
    (mapcar #'(lambda (cursor)
                (with-bounding-rectangle* (x1 y1 x2 y2) cursor
                  (unless (= x1 y1 x2 y2)
                    (draw-rectangle* stream x1 y1 x2 y2
                                     :ink (background-ink drei)
                                     :filled t))))
            (cursors drei))
    (with-output-recording-options (stream :record t :draw nil)
      (letf (((stream-current-output-record stream) drei)
             ((stream-cursor-position stream) (values-list (input-editor-position drei))))
        (display-drei-contents stream drei (syntax (buffer drei)))))))

(defmethod replay-output-record :after ((drei drei-area) (stream extended-output-stream) &optional
                                        (x-offset 0) (y-offset 0) (region +everywhere+))
  (declare (ignore x-offset y-offset region))
  (mapcar #'(lambda (cursor)
              (replay cursor stream))
          (cursors drei))
  ;; Make sure that point is the very last cursor that is displayed.
  (replay (point-cursor drei) stream))

(defmethod replay-output-record :before ((cursor drei-cursor) stream &optional
                                         (x-offset 0) (y-offset 0) (region +everywhere+))
  (declare (ignore x-offset y-offset region))
  (clear-output-record cursor)
  (when (active cursor)
    (with-output-recording-options (stream :record t :draw nil)
      (letf (((stream-current-output-record stream) cursor)
             ((stream-cursor-position stream) (output-record-position cursor)))
        (display-drei-cursor stream (drei-instance cursor) cursor (syntax (buffer (drei-instance cursor))))))))

(defmethod offset-to-screen-position :around (pane (drei drei-area) offset)
  "Adjust the returned offset with the position of the Drei area
on display."
  (multiple-value-bind (x y height style-width) (call-next-method)
    (values (+ x (first (input-editor-position drei))) y height style-width)))

(defun display-drei-area (drei)
  (with-accessors ((stream editor-pane)) drei
    (update-syntax-for-display (buffer drei) (syntax (buffer drei)) (top drei) (bot drei))
    (replay drei stream)
    (with-bounding-rectangle* (dx1 dy1 dx2 dy2) drei
      (declare (ignore dx1 dy1 dy2))
      (with-bounding-rectangle* (x1 y1 x2 y2) (point-cursor drei)
        (apply #'change-space-requirements stream (when (> x2 dx2)
                                                    (list :width x2)))
        (when (pane-viewport stream)
          (let* ((viewport (pane-viewport stream))
                 (viewport-height (bounding-rectangle-height viewport))
                 (viewport-width (bounding-rectangle-width viewport))
                 (viewport-region (pane-viewport-region stream)))
            ;; Scroll if point went outside the visible area.
            (when (and (active drei)
                       (pane-viewport stream)
                       (not (and (region-contains-position-p viewport-region x2 y2)
                                 (region-contains-position-p viewport-region x1 y1))))
              (scroll-extent stream
                             (max 0 (- x2 viewport-width))
                             (max 0 (- y2 viewport-height))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drei pane redisplay.

(defun nb-lines-in-pane (pane)
  (let* ((medium (sheet-medium pane))
	 (style (medium-text-style medium))
	 (height (text-style-height style medium)))
    (multiple-value-bind (x y w h) (bounding-rectangle* pane)
      (declare (ignore x y w))
      (max 1 (floor h (+ height (stream-vertical-spacing pane)))))))

(defun adjust-pane-bot (pane)
  "Make the region on display fit the size of the pane as closely
as possible by adjusting bot leaving top intact."
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-slots (top bot) pane
      (setf (offset bot) (offset top))
      (end-of-line bot)
      (loop until (end-of-buffer-p bot)
         repeat (1- nb-lines-in-pane)
         do (forward-object bot)
         (end-of-line bot)))))

(defun reposition-pane (pane)
  "Try to put point close to the middle of the pane by moving top
half a pane-size up."
  (let ((nb-lines-in-pane (nb-lines-in-pane pane)))
    (with-accessors ((top top)) pane
      (setf (offset top) (offset (point pane)))
      (loop do (beginning-of-line top)
         repeat (floor nb-lines-in-pane 2)
         until (beginning-of-buffer-p top)
         do (decf (offset top))
         (beginning-of-line top)))))

(defun adjust-pane (pane)
  "Adjust the bottom and top marks of the pane to be correct, and
reposition the pane if point is outside the visible area."
  (with-accessors ((buffer buffer) (top top) (bot bot)) pane
    (let ((low-mark (low-mark buffer))
          (nb-lines-in-pane (nb-lines-in-pane pane)))
      (beginning-of-line top)
      (end-of-line bot)
      (when (or (mark< (point pane) top)
                (>= (number-of-lines-in-region top (point pane))
                    nb-lines-in-pane)
                (mark< low-mark top))
        (reposition-pane pane))))
  (adjust-pane-bot pane))

(defun page-down (pane)
  (with-slots (top bot) pane
    (when (mark> (size (buffer bot)) bot)
      (setf (offset top) (offset bot))
      (beginning-of-line top)
      (setf (offset (point pane)) (offset top)))))

(defun page-up (pane)
  (with-slots (top bot) pane
    (when (> (offset top) 0)
      (let ((nb-lines-in-region (number-of-lines-in-region top bot)))
        (setf (offset bot) (offset top))
        (end-of-line bot)
        (loop repeat nb-lines-in-region
           while (> (offset top) 0)
           do (decf (offset top))
           (beginning-of-line top))
        (setf (offset (point pane)) (offset bot))
        (beginning-of-line (point pane))))))

(defgeneric fix-pane-viewport (pane))

(defmethod fix-pane-viewport ((pane drei-pane))
  (let* ((output-width (bounding-rectangle-width (stream-current-output-record pane)))
         (viewport (pane-viewport pane))
         (viewport-width (and viewport (bounding-rectangle-width viewport)))
         (pane-width (bounding-rectangle-width pane)))
    ;; If the width of the output is greater than the width of the
    ;; sheet, make the sheet wider. If the sheet is wider than the
    ;; viewport, but doesn't really need to be, make it thinner.
    (when (or (> output-width pane-width)
              (and viewport
                   (> pane-width viewport-width)
                   (>= viewport-width output-width)))
      (change-space-requirements pane :width output-width))
    (when (and viewport (active pane))
      (multiple-value-bind (cursor-x cursor-y) (offset-to-screen-position pane pane (offset (point pane)))
        (declare (ignore cursor-y))
        (let ((x-position (abs (transform-position (sheet-transformation pane) 0 0)))
              (viewport-width (bounding-rectangle-width (or (pane-viewport pane) pane))))
          (cond ((> cursor-x (+ x-position viewport-width))
                 (move-sheet pane (round (- (- cursor-x viewport-width))) 0))
                ((> x-position cursor-x)
                 (move-sheet pane (if (> viewport-width cursor-x)
                                      0
                                      (round (- cursor-x)))
                             0))))))))

(defmethod handle-repaint :before ((pane drei-pane) region)
  (declare (ignore region))
  (redisplay-frame-pane (pane-frame pane) pane))

(defun display-drei-pane (frame drei-pane)
  "Display `pane'. If `pane' has focus, `current-p' should be
non-NIL."
  (declare (ignore frame))
  (with-accessors ((buffer buffer) (top top) (bot bot)
                   (point-cursor point-cursor)) drei-pane
    (if (full-redisplay-p drei-pane)
        (progn (reposition-pane drei-pane)
               (adjust-pane-bot drei-pane)
               (setf (full-redisplay-p drei-pane) nil))
        (adjust-pane drei-pane))
    (update-syntax-for-display buffer (syntax buffer) top bot)
    (display-drei-contents drei-pane drei-pane (syntax buffer))
    ;; Point must be on top of all other cursors.
    (display-drei-cursor drei-pane drei-pane point-cursor (syntax buffer))
    (dolist (cursor (cursors drei-pane))
      (display-drei-cursor drei-pane drei-pane cursor (syntax buffer)))
    (fix-pane-viewport drei-pane)))

(defgeneric full-redisplay (pane)
  (:documentation "Queue a full redisplay for `pane'."))

(defmethod full-redisplay ((pane drei-pane))
  (setf (full-redisplay-p pane) t))

(defgeneric display-region (pane syntax))
