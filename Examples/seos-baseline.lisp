(defpackage "CLIM-DEMO.SEOS-BASELINE"
  (:use "CLIM-LISP" "CLIM" "CLIME")
  (:export "SEOS-BASELINE"))
(in-package "CLIM-DEMO.SEOS-BASELINE")

(define-application-frame seos-baseline ()
  ()
  (:panes (app :application
               :width 400
               :height 400
               :display-function #'display
               :end-of-line-action :wrap*
               :end-of-page-action :allow
               :text-margins '(:left (:relative 50)
                               :right (:relative 50)
                               :top (:relative 50)
                               :bottom (:relative 50))))
  (:reinitialize-frames t))

(defvar *data1*)
(defvar *data2*)
(defvar *data3*)
(defvar *size2*)
(defvar *inks2*)
(defvar *picks*)

(define-seos-baseline-command (com-new-data :keystroke (#\r :control)) ()
  (setf *data1* (lorem-ipsum:paragraph :word-count 32)
        *data2* (lorem-ipsum:words 32)
        *data3* (lorem-ipsum:paragraphs 3 :word-count 32)
        *size2* (loop with elts = '(:tiny :small :normal :large :huge)
                      repeat 32 collect (elt elts (random (length elts))))
        *inks2* (loop with inks = (make-contrasting-inks 8)
                      repeat 32 collect (elt inks (random (length inks))))
        *picks* (loop repeat 32
                      collect (zerop (random 4)))))

(com-new-data)

(define-seos-baseline-command (com-redisplay :keystroke #\space) ()
  (setf *inks2* (loop with inks = (make-contrasting-inks 8)
                      repeat 32 collect (elt inks (random (length inks)))))
  (let ((repaint (make-instance 'window-repaint-event
                                :region +everywhere+
                                :sheet *standard-output*)))
    (schedule-event *standard-output* repaint 1)))

(define-seos-baseline-command (com-line-action :menu t) ()
  (setf (stream-end-of-line-action *standard-output*)
        (or (menu-choose '(:allow :scroll :wrap :wrap*))
            (stream-end-of-line-action *standard-output*))))

(define-seos-baseline-command (com-page-action :menu t) ()
  (setf (stream-end-of-page-action *standard-output*)
        (or (menu-choose '(:allow :scroll :wrap :wrap*))
            (stream-end-of-page-action *standard-output*))))

(defun print-header (text stream)
  (fresh-line stream)
  (with-drawing-options (stream :text-size :large :text-face :bold)
    (princ text stream))
  (terpri stream))

(defmethod display ((frame seos-baseline) stream)
  (draw-design stream (stream-page-region stream) :ink +light-grey+)
  (setf (frame-pretty-name frame)
        (format nil "line ~s, page ~s"
                (stream-end-of-line-action stream)
                (stream-end-of-page-action stream)))
  (progn
    (print-header "One paragraph:" stream)
    (princ *data1* stream))
  (progn
    (print-header "Tiny paragraphs:" stream)
    (with-drawing-options (stream :text-size :tiny)
      (princ *data1* stream)
      (terpri stream)
      (terpri stream)
      (princ *data1* stream)
      (terpri stream)))
  (progn
    (print-header "Long line, varying size:" stream)
    (loop for iter from 0
          for word in *data2*
          for size in *size2*
          for ink  in *inks2*
          do (with-drawing-options (stream :text-size size :ink ink)
               (format stream "~a " word))))
  (progn
    (print-header "Long line, varying size, without spaces:" stream)
    (loop for iter from 0
          for word in *data2*
          for size in *size2*
          for ink  in *inks2*
          do (with-drawing-options (stream :text-size size :ink ink)
               (format stream "~a" word))))
  (progn
    (print-header "Long line, mixed graphics:" stream)
    (loop for iter from 0
          for word in *data2*
          for size in *size2*
          for ink  in *inks2*
          for pick in *picks*
          do (if (null pick)
                 (with-drawing-options (stream :text-size size :ink ink)
                   (format stream "~a " word))
                 (with-room-for-graphics (stream :first-quadrant t :move-cursor t)
                   (draw-polygon* stream '(-20 0 0 25 20 0)
                                  :ink (compose-in ink (make-opacity .5)))))))
  (progn
    (print-header "Mean scenario for word wrap:" stream)
    (format stream "AAA BBBBBBBBBBBBBBB CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD EEE FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))
  (progn
    (print-header "Three paragraphs:" stream)
    (format stream "~{~a~%~}" *data3*))
  (progn
    (print-header "Mean scenario for the last line's size" stream)
    (with-drawing-options (stream :text-size :huge)
      (format stream "Good bye with a twist!"))))

;; (find-application-frame 'seos-baseline)
