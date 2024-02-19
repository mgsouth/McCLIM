;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2024 Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Utilities useful while drawing.
;;;

(in-package #:clim-internals)

(declaim (inline round-coordinate))
(defun round-coordinate (x)
  "Function used for rounding coordinates."
  ;; When in doubt we use "round half up" rounding, instead of the CL:ROUND
  ;; "round half to even".
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often want to
  ;; operate with coordinates, which are multiples of 1/2.  Using CL:ROUND gives
  ;; "random" results. Using "round half up" gives you more consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners, while in X11
  ;; they are at the centers. We don't do much about the discrepancy, but
  ;; rounding up at half pixel boundaries seems to work well.
  (etypecase x
    (integer      x)
    (single-float (values (floor (+ x .5f0))))
    (double-float (values (floor (+ x .5d0))))
    (long-float   (values (floor (+ x .5l0))))
    (ratio        (values (floor (+ x 1/2))))))

(defmacro with-round-positions ((transformation &rest coordinates) &body body)
  (destructuring-bind (x y &rest rest-coords) coordinates
    (if (null rest-coords)
        `(with-transformed-position (,transformation ,x ,y)
           (setf ,x (round-coordinate ,x)
                 ,y (round-coordinate ,y))
           ,@body)
        `(with-transformed-position (,transformation ,x ,y)
           (setf ,x (round-coordinate ,x)
                 ,y (round-coordinate ,y))
           (with-round-positions (,transformation ,@rest-coords)
             ,@body)))))

(defmacro with-round-coordinates ((transformation coordinates) &body body)
  `(with-transformed-positions (,transformation ,coordinates)
     (map-into ,coordinates #'round-coordinate ,coordinates)
     ,@body))
