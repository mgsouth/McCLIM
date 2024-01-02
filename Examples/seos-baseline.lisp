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

(define-seos-baseline-command (com-new-data :keystroke (#\r :control)) ()
  (setf *data1* (lorem-ipsum:paragraph :word-count 32)
        *data2* (lorem-ipsum:words 32)
        *data3* (lorem-ipsum:paragraphs 3 :word-count 32)
        *size2* (loop with elts = '(:tiny :small :normal :large :huge)
                      repeat 32 collect (elt elts (random (length elts))))
        *inks2* (loop with inks = (make-contrasting-inks 8)
                      repeat 32 collect (elt inks (random (length inks))))))

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
  (print-header "1. One paragraph:" stream)
  (princ *data1* stream)
  (print-header "2. Long line, varying size:" stream)
  (loop for iter from 0
        for word in *data2*
        for size in *size2*
        for ink  in *inks2*
        do (with-drawing-options (stream :text-size size :ink ink)
             (format stream "~a " word)))
  (print-header "3. Long line, varying size, without spaces:" stream)
  (loop for iter from 0
        for word in *data2*
        for size in *size2*
        for ink  in *inks2*
        do (with-drawing-options (stream :text-size size :ink ink)
             (format stream "~a" word)))
  (print-header "4. Mean scenario for word wrap:" stream)
  (format stream "AAA BBBBBBBBBBBBBBB CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD EEE FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
  (print-header "5. Three paragraphs:" stream)
  (format stream "~{~a~%~}" *data3*)
  (print-header "6. Mean scenario for the last line's size" stream)
  (with-drawing-options (stream :text-size :huge)
    (format stream "Good bye with a twist!")))

;; (find-application-frame 'seos-baseline)
