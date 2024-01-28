;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2018 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Pattern and design-related demos.
;;;

(defpackage #:clim-demo.patterns
  (:use #:clim-lisp #:clim)
  (:import-from #:alexandria #:if-let #:curry #:emptyp #:with-gensyms)
  (:export #:pattern-design-test))
(in-package #:clim-demo.patterns)

;;; uniform designs arranged in a pattern
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter *text-right-margin* 380)
 (defparameter *block-height* 220)
 (defparameter *total-height* 1280))

(defparameter *patterns* nil)

(defun initialize-patterns ()
  (unless *patterns*
    (setf *patterns*
          (let* ((array (make-array '(50 50) :initial-element 1 :element-type 'bit))
                 (array2 (make-array '(20 20) :initial-element 1 :element-type 'fixnum))
                 (array3 (make-array '(50 50) :initial-element 0.0f0 :element-type 'single-float))
                 (2-designs  (list +dark-blue+ +dark-red+))
                 (2-designs* (list +dark-salmon+ +dark-slate-grey+))
                 (4-designs  (list +orange+ +dark-green+ +red+ +blue+))
                 (4-designs* (list +purple+ +blue+ +red+ +grey+))
                 (x-designs  (list (make-rectangular-tile (make-pattern array2 4-designs) 25 25)
                                   (make-rectangular-tile (make-pattern array2 4-designs*) 25 25))))
            ;; set array for 4x4 checkboard
            (dotimes (i 25)
              (dotimes (j 25)
                (setf (aref array i j) 0
                      (aref array (- 49 i) (- 49 j)) 0)))
            ;; set array2 for 5x5 checkboard
            (dotimes (i 20)
              (dotimes (j 20)
                (setf (aref array2 i j) (mod (+ (truncate i 5) (truncate j 5)) 4))))
            ;; set array3 for gradient stencil
            (dotimes (i 50)
              (dotimes (j 50)
                (setf (aref array3 i j) (float (/ (+ i j) 100.0f0)))))

            (list
             ;; 50x50 checkboard indexed pattern
             (make-pattern array 2-designs)
             ;; 50x50 checkboard indexed pattern with indexed rectangular-tile inks
             (make-pattern array x-designs)
             ;; 30x30 checkboard 4-indexed pattern rotated by pi/4 and translated
             (transform-region (compose-transformation-with-translation
                                (make-rotation-transformation* (/ pi 4) 10.5 10.5)
                                12.5 12.5)
                               (make-pattern array2 4-designs))
             ;; 40x40 checkboard indexed rectangular-tile (ink is 20x20)
             (make-rectangular-tile (make-pattern array2 4-designs) 40 40)
             ;; 30x30 checkboard indexed rectangular-tile (ink is 50x50)
             (make-rectangular-tile (make-pattern array 2-designs*) 30 30)
             ;; 50x50 checkboard stencil
             (make-stencil array3))))))

(defparameter *general-description*
  "In this demo we explore draw-pattern* and draw-design capabilities when used
on patterns under various transformation and ink variations. We test these demos
in various arrangaments (drawn as a recorded stream and moved, simply painted on
a basic-sheet which is not a stream etc.). Under each test you will find a
description what should be seen with a possible failure description.

Images will be drawn on top of a rectangle which is transformed with normal
current medium transformation as a set of square patterns. To allow easy
function scanning draw-pattern* is drawn on top of a red dashed rectangle and
draw-design is drawn on top of a blue dashed rectangle.

Pattern 1: a blue-red 2x2 checkboard pattern (50x50)
Pattern 2: a checkboard rectangular-tile (25x25, ink 20x20) pattern (50x50)
Pattern 4: a checkboard pattern (20x20) rotated by pi/4 translated by [12.5, 12.5]
Pattern 3: a rectangular tile bigger than its ink (40x40 vs 20x20)
Pattern 4: a rectangular tile smaller than its ink (30x30 vs 5x50)
Pattern 5: a stencil with opacity increasing with XY from 0->1 (50x50) [disabled]")

(defparameter *options*
  "Keyboard shortcuts:
-----------------------------------------------------
1: draw-pattern*                             PATTERN
2: draw-design                                DESIGN
3: draw-rectangle with pattern being ink   RECTANGLE
-----------------------------------------------------
R: restart demo
Q: quit application frame
Space: redisplay application")

(defvar *draw* :pattern)

(define-application-frame pattern-design-test ()
  ()
  (:menu-bar nil)
  (:geometry :width 1280 :height 720)
  (:panes (pane1 :application :display-function 'display :scroll-bars :vertical)
          (pane2 :application :display-function 'display :scroll-bars nil))
  (:layouts (default (horizontally ()
                       (1/4 (labelling (:label "Application :SCROLL-BARS :BOTH")
                              pane1))
                       (1/4 (labelling (:label "Application")
                              (scrolling (:scroll-bars :vertical)
                                pane2)))))))

(defun draw-patterns (pane)
  (draw-rectangle* pane 5 5 (+ (* 60 (length *patterns*)) 5) 65
                   :filled nil :line-dashes t :line-thickness 3 :ink +dark-red+
                   ;; Bug # medium-gcontext has no primary method for
                   ;; uniform-compositum on CLX
                   :filled t :ink (climi::make-uniform-compositum +dark-red+ 0.5))
  (do* ((i 0 (1+ i))
        (x 10 (+ 60 x))
        (p* *patterns* (cdr p*))
        (pattern #1=(first p*) #1#))
       ((endp p*))
    (draw-pattern* pane pattern x 10)
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :filled nil)))

(defun draw-designs (pane)
  (draw-rectangle* pane 5 5 (+ (* 60 (length *patterns*)) 5) 65
                   :filled nil :line-dashes t :line-thickness 3 :ink +dark-blue+
                   :filled t :ink (climi::make-uniform-compositum +dark-blue+ 0.5))
  (do* ((i 0 (1+ i))
        (x 10 (+ 60 x))
        (p* *patterns* (cdr p*))
        (pattern #1=(first p*) #1#))
       ((endp p*))
    (draw-design pane pattern :transformation (make-translation-transformation x 10))
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :filled nil :line-dashes nil :line-thickness 2 :ink +grey42+)))

(defun draw-rects (pane)
  (draw-rectangle* pane 5 5 (+ (* 60 (length *patterns*)) 5) 65
                   :filled nil :line-dashes t :line-thickness 3 :ink +dark-green+
                   :filled t :ink (climi::make-uniform-compositum +dark-green+ 0.5))
  (do* ((i 0 (1+ i))
        (x 10 (+ 60 x))
        (p* *patterns* (cdr p*))
        (pattern #1=(first p*) #1#))
       ((endp p*))
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :ink pattern)
    (draw-rectangle* pane x 10 (+ x (pattern-width pattern)) (+ 10 (pattern-height pattern))
                     :filled nil :line-dashes nil :line-thickness 2 :ink +grey42+)))

(defun %split-line (character string &key (count 1) from-end)
  (check-type count (integer 1))
  (if-let ((pos (position-if #'(lambda (c)
                                 (and (char= character c)
                                      (zerop (decf count))))
                             string
                             :from-end from-end)))
    (values (string-right-trim '(#\space) (subseq string 0 pos))
            (string-right-trim '(#\space) (subseq string (1+ pos))))
    string))

(defun %split-sequence-to-list (character string)
  (if-let ((pos (position-if (curry #'char= character) string)))
    (cons (subseq (string-right-trim (list character #\space) string) 0 pos)
          (%split-sequence-to-list character (subseq string (1+ pos))))
    (list (string-right-trim (list character #\space) string))))

(defun split-line-by-word (text margin &optional (text-size-fn #'length))
  "Splits line of text by word so the first part fits inside the margin. If
there is a non-empty remainder it is returned as a second value. If no word fits
in margin the first word is returned neverless. Returned strings are
right-trimmed for spaces."
  ;; XXX: do we want to trim leading space? Yes! but not on this demo :-)
  (setf text (string-trim '(#\newline #\space)
                          (string-right-trim '(#\space #\newline) text)))
  (when (<= (funcall text-size-fn text) margin)
    (return-from split-line-by-word text))
  (do* ((count 1 (1+ count))
        (results (multiple-value-list
                  (%split-line #\space text :count count :from-end t)))
        (string (first results))
        (remainder (second results)))
       ((or (<= (funcall text-size-fn string) margin)
            (emptyp remainder))
        (if (emptyp remainder)
            (%split-line #\space text)
            (values string remainder)))
    (multiple-value-setq (string remainder)
      (%split-line #\space text :count count :from-end t))))

(defun test-example (pane &key first-quadrant transformation (description "") draw)
  (with-room-for-graphics (pane :first-quadrant first-quadrant :move-cursor nil)
    (with-drawing-options (pane :transformation transformation)
      (ecase draw
        (:design (draw-designs pane))
        (:pattern (draw-patterns pane))
        (:rectangle (draw-rects pane))))
    (draw-arrow* pane *text-right-margin* 0 *text-right-margin* 75 :line-thickness 3)))

(defun display (frame pane &aux (draw *draw*))
  (declare (ignore frame))
  (initialize-patterns)
  (test-example pane :first-quadrant nil :draw draw)
  (setf (stream-cursor-position pane) (values 0 200))
  (draw-line* pane 0 100 *text-right-margin* 100)
  (test-example pane :first-quadrant t :draw draw)
  (draw-line* pane 0 225 *text-right-margin* 225)
  (setf (stream-cursor-position pane) (values 0 250))
  (test-example pane :first-quadrant nil :draw draw
                     :transformation (make-rotation-transformation (/ pi 8)))
  (draw-line* pane 0 475 *text-right-margin* 475)
  (setf (stream-cursor-position pane) (values 0 700))
  (test-example pane :first-quadrant t :draw draw
                     :transformation (make-rotation-transformation (/ pi 8))))

(define-pattern-design-test-command (refresh-pattern-design :keystroke #\space) ()
  (format *debug-io* "."))

(progn (define-pattern-design-test-command (dpattern :keystroke #\1) ()
         (setf *draw* :pattern)
         #1=(map-over-sheets (lambda (sheet)
                               (redisplay-frame-pane *application-frame* sheet :force-p t)
                               (repaint-sheet sheet +everywhere+))
                             (frame-top-level-sheet *application-frame*)))
       (define-pattern-design-test-command (ddesign :keystroke #\2) ()
         (setf *draw* :design)
         #1#)
       (define-pattern-design-test-command (drectangle :keystroke #\3) ()
         (setf *draw* :rectangle)
         #1#))

(define-pattern-design-test-command (exit :keystroke #\q) ()
  (frame-exit *application-frame*))

(define-pattern-design-test-command (com-restart :keystroke #\r) ()
  (bt:make-thread (lambda ()
                    (run-frame-top-level
                     (make-application-frame 'pattern-design-test)))
                  :initial-bindings `((*default-server-path* . ',*default-server-path*)))
  ;; frame-exit throws! we need to start a new demo before it, because rest of
  ;; the body is never executed.
  (frame-exit *application-frame*))
