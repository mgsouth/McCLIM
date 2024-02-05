(in-package #:clim-demo)

(defparameter *pattern*
  (let ((array (make-array '(50 50) :initial-element 0)))
    (loop for x from 0 below 50 do
      (loop for y from 0 below 50 do
        (setf (aref array y x)
              (truncate (+ (floor y 25) (floor x 25))))))
    (make-pattern array (list +dark-blue+ +dark-red+ +dark-green+))))

(clim:define-application-frame wrfg-test ()
  ()
  (:pane (make-pane :application :display-function 'display
                    :end-of-line-action :allow)))

;; This test illustrates how we can embed patterns on the line. The first
;; pattern extends from the baseline up and the second from the baseline down.

(defmethod display ((frame wrfg-test) stream)
  (declare (ignore frame))
  (format stream "This is the first line.~%")
  (format stream "HELLO!")
  (with-room-for-graphics (stream :first-quadrant t)
    (draw-pattern* stream *pattern* 0 0))
  (format stream "FUNNY!")
  (with-room-for-graphics (stream :first-quadrant nil)
    (draw-pattern* stream *pattern* 0 0))
  (format stream "BYE!")
  (terpri stream)
  (format stream "This is the next line."))
