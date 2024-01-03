(in-package #:clim-demo)

(define-application-frame text-multiline-positioning ()
  ()
  (:menu-bar nil)
  (:pane (make-pane :application :display-function #'display
                                 :text-margins '(:left (:absolute 20) :top (:absolute 10))
                    :end-of-line-action :allow))
  (:reinitialize-frames t))

(defun draw-text-box (pane align-x align-y transformation)
  (let ((string* (format nil
                         "~A~%~A~%~A~%~A~%~%~s~%~A"
                         "First line of text."
                         "The quick brown fox jumps over"
                         "the lazy dog. Belive it or not."
                         "Below is an empty line:"
                         ;; <intentionally empty>
                         (list :align-x align-x :align-y align-y)
                         "Last line of text. ytmMΣ音!")))
    (with-drawing-options (pane :transformation transformation)
      (surrounding-output-with-border (pane :filled t
                                            :ink +light-pink+
                                            :padding-x 0
                                            :padding-y 0)
        (clim:draw-text* pane string*
                         0 0 :transform-glyphs t
                         :align-x align-x :align-y align-y))
      (clim:draw-circle* pane 0 0 15 :ink (compose-in +blue+ (make-opacity .5))))))

(defmethod display ((frame text-multiline-positioning) pane)
  (with-drawing-options (pane :text-style (make-text-style nil nil 12)
                              ;; this is also good, but the test is more clear
                              ;; to understand without transformations.
                              #|:transformation (make-rotation-transformation (/ pi 4))|#)
    (format pane "~&Box alignment:~%")
    (formatting-item-list (pane :row-wise nil :n-columns 3)
      (dolist (ax '(:left :center :right))
        (dolist (ay '(:top :center :bottom))
          (formatting-cell (pane)
            (draw-text-box pane ax ay +identity-transformation+)))))
    (format pane "~&Basline alignment:~%")
    (formatting-item-list (pane :row-wise nil :n-columns 3)
      (loop for (ax ay) in '((:left :baseline)
                             (:center :baseline*)
                             (:right :baseline))
            do (formatting-cell (pane)
                 (draw-text-box pane ax ay +identity-transformation+))))
    (format pane "~&Transformations:~%")
    (formatting-item-list (pane :row-wise nil :n-columns 3)
      (loop for (ax ay tr) in `((:right :center ,(make-scaling-transformation -1.1 0.8))
                                (:center :center ,(make-rotation-transformation (/ pi 4)))
                                (:center :top ,(make-rotation-transformation (/ pi 2))))
            do (formatting-cell (pane)
                 (draw-text-box pane ax ay tr))))))

(define-text-multiline-positioning-command (com-refresh-multiline :keystroke #\space) ())
