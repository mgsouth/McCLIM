;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) copyright 2020 Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;;
;;; ---------------------------------------------------------------------------

(cl:in-package #:clim-tests)

(def-suite* :mcclim.gestures
  :in :mcclim)

(test gestures.1.add-gesture-name.smoke
  "Test errors signaled by `add-gesture-name'."
  (mapc (lambda (arguments-and-expected)
          (destructuring-bind (name type gesture-spec &optional expected)
              arguments-and-expected
            (flet ((do-it ()
                     (add-gesture-name name type gesture-spec)))
              (if expected
                  (signals error (do-it))
                  (finishes (do-it))))))
        '((1    :keyboard               (#\a)                     error) ; invalid name
          (:foo :no-such-type           (#\a)                     error) ; invalid type
          (:foo :keyboard               (#\a :no-such-modifier)   error) ; invalid modifier
          (:foo :keyboard               (#\a))                           ; ok
          (:foo :keyboard               (#\a :meta))                     ; ok
          (:foo :keyboard               #\a)                             ; extension
          (:foo :keyboard               (t))                             ; extension
          (:foo :keyboard               (#\a t))                         ; extension
          (:foo :keyboard               (t t))                           ; extension

          (:foo :pointer-button         (1)                       error) ; invalid button
          (:foo :pointer-button         (:no-such-button)         error) ; invalid button
          (:foo :pointer-button         (:left :no-such-modifier) error) ; invalid modifier
          (:foo :pointer-button         (:left))                         ; ok
          (:foo :pointer-button-press   (:middle))                       ; ok
          (:foo :pointer-button-release (:right))                        ; ok
          (:foo :pointer-scroll         (:wheel-up))                     ; ok
          (:foo :pointer-button         :left)                           ; extension
          (:foo :pointer-button         (t))                             ; extension
          (:foo :pointer-button         (:left t))                       ; extension
          (:foo :pointer-button         (t t))                           ; extension
          ;;
          (:foo :pointer-motion         (t t))
          (:foo :pointer-motion         (:left t))
          (:foo :pointer-motion         ((:left :right) t))
          (:foo :pointer-motion         (:none t))
          (:foo :pointer-motion         (:none t))
          (:foo :pointer-motion         ((:left :no-button) t)    error)
          ;;
          (:foo :timer                  (:alarm))
          (:foo :timer                  (:alarm :meta)            error))))

(test gestures.2.ensure-physical-gesture.smoke
  "Smoke test for the `ensure-physical-gesture' function."
  (mapc (lambda (designator-and-expected)
          (destructuring-bind (designator expected) designator-and-expected
            (flet ((do-it ()
                     (climi::ensure-physical-gesture designator)))
              (case expected
                (error     (signals error (do-it)))
                (otherwise (is (equal expected (do-it))))))))

        `(;; Physical gestures
          ((:keyboard #\x 0)         (:keyboard #\x 0))
          ;; Keys
          (#\x                       (:keyboard #\x 0))
          ((#\x :control)            (:keyboard #\x ,+control-key+))
          (:left                     (:keyboard :left 0))
          ((:left :meta)             (:keyboard :left ,+meta-key+))
          ;; Errors
          ((:left :no-such-modifier) error))))

(test gestures.3.event-data-matches-gesture-p.smoke
  "Smoke test for the `event-data-matches-gesture-p' function."
  (mapc (lambda (arguments-and-expected)
          (destructuring-bind (type device-name modifier-state gesture expected)
              arguments-and-expected
            (flet ((do-it ()
                     (climi::event-data-matches-gesture-p
                      type device-name modifier-state gesture)))
              (if expected
                  (is-true  (do-it))
                  (is-false (do-it))))))
        `(;; Wildcard gesture
          (:keyboard    #\a     0       t  t) ; extension
          (:keyboard    #\a     :ignore t  t) ; extension
          (:ignore      :ignore :ignore t  t) ; extension
          ;; Wildcards in gesture elements
          (:keyboard    #\a    0              ((t          #\a   0))    t) ; extension
          (:keyboard    #\a    0              ((:keyboard  t     0))    t) ; extension
          (:keyboard    #\a    0              ((:keyboard  #\a   t))    t) ; extension
          ;; Wildcards in event data
          (:ignore      #\a     0             ((:keyboard  #\a   0))    t)
          (:keyboard    :ignore 0             ((:keyboard  #\a   0))    t)
          (:keyboard    #\a     :ignore       ((:keyboard  #\a   0))    t)
          ;; Keyboard
          (:keyboard    #\a     0             ((:keyboard  #\a   0))    t)
          (:keyboard    #\b     0             ((:keyboard  #\a   0))    nil)
          (:keyboard    #\a     ,+shift-key+  ((:keyboard  #\a   0))    nil)
          (:keyboard    :down   0             ((:keyboard  :down 0))    t)
          ;; Pointer
          (:pointer-button-press :left  0            ((:pointer-button-press :left 0)) t)
          (:pointer-button-press :right 0            ((:pointer-button-press :left 0)) nil)
          (:pointer-button-press :left  ,+shift-key+ ((:pointer-button-press :left 0)) nil)
          ;; `:pointer-button' "sub-typing"
          (:pointer-button-press :left 0             ((:pointer-button       :left 0)) t)
          ;; Multiple physical gestures
          (:keyboard #\a 0       ((:keyboard #\b 0) (:keyboard #\a 0)) t)
          (:keyboard #\a :ignore ((:keyboard #\a 0) (:keyboard #\b 0)) t)
          ;; Extended gestures
          (:pointer-motion 0 0                      ((:pointer-motion 0 0)) t)
          (:pointer-motion ,+pointer-left-button+ 0 ((:pointer-motion 0 0)) nil)
          ;; Modifier state is ignored for timer events.
          (:timer :alarm  :ignore ((:timer t        0))   t)
          (:timer :alarm  :ignore ((:timer :alarm   0))   t)
          (:timer :alarm  :ignore ((:timer :timeout 0)) nil)
          (:timer :ignore :ignore ((:timer :timeout 0))   t))))

(test gestures.4.indirect-gestures.smoke
  (flet ((reset-gestures ()
           (delete-gesture-name 'movement)
           (delete-gesture-name 'forward)
           (delete-gesture-name 'move-n)
           (delete-gesture-name 'move-s)
           (delete-gesture-name 'move-w)
           (delete-gesture-name 'move-e)
           (delete-gesture-name 'move-f))
         (define-game ()                ;indirect mapping
           ;; Define an indirect gesture that matches all movement.
           (add-gesture-name 'movement :indirect 'move-n)
           (add-gesture-name 'movement :indirect 'move-s :unique nil)
           (add-gesture-name 'movement :indirect 'move-w :unique nil)
           (add-gesture-name 'movement :indirect 'move-e :unique nil)
           (add-gesture-name 'movement :indirect 'move-f :unique nil)
           ;; Define an indirect gesture that matches "move forward".
           (add-gesture-name 'forward :indirect 'move-f))
         (define-wsad ()                ;normal gestures
           (add-gesture-name 'move-n :keyboard #\w :unique t)
           (add-gesture-name 'move-s :keyboard #\s :unique t)
           (add-gesture-name 'move-w :keyboard #\a :unique t)
           (add-gesture-name 'move-e :keyboard #\d :unique t)
           ;;
           (add-gesture-name 'move-f :keyboard #\space          :unique nil)
           (add-gesture-name 'move-f :pointer-button '(:left t) :unique nil))
         (define-move ()                ;normal gestures (redefines)
           (add-gesture-name 'move-n :keyboard :up    :unique t)
           (add-gesture-name 'move-s :keyboard :down  :unique t)
           (add-gesture-name 'move-w :keyboard :left  :unique t)
           (add-gesture-name 'move-e :keyboard :right :unique t)
           ;;
           (add-gesture-name 'move-f :keyboard #\newline         :unique nil)
           (add-gesture-name 'move-f :pointer-button '(:right t) :unique nil))
         (check (data gesture expected)
           (destructuring-bind (gtype gdevt gmods) data
             (flet ((do-it ()
                      (climi::event-data-matches-gesture-p
                       gtype gdevt gmods (climi::find-gesture gesture))))
               (ecase expected
                 (:true  (is-true  (do-it) "Gesture ~s should match ~s." gesture data))
                 (:false (is-false (do-it) "Gesture ~s shan't match ~s." gesture data))
                 (:error (signals error (do-it))))))))
    (reset-gestures)
    (define-game)
    ;; Indirect gestures are unbound at this point.
    (check '(:ignore :ignore :ignore)   'movement :true)
    (check '(:keyboard :ignore :ignore) 'movement :false)
    (check '(:indirect :ignore :ignore) 'movement :error) ;no real data is "indirect"
    ;; Define target gestures (WSAD)
    (define-wsad)
    (check '(:keyboard :ignore :ignore)         'movement :true)
    (check `(:keyboard #\w :ignore)             'movement :true)
    (check `(:keyboard #\e :ignore)             'movement :false)
    (check `(:keyboard #\s ,climi::+no-key+)    'movement :true)
    (check `(:keyboard #\s ,climi::+meta-key+)  'movement :false)
    (check `(:keyboard :left :ignore)           'movement :false)
    (check `(:keyboard #\space :ignore)         'movement :true)
    ;; forward movement
    (check `(:keyboard #\space ,climi::+no-key+)   'forward :true)
    (check `(:keyboard #\space ,climi::+meta-key+) 'forward :false)
    (check `(:pointer-button ,climi::+pointer-left-button+ ,climi::+no-key+)   'forward :true)
    (check `(:pointer-button ,climi::+pointer-left-button+ ,climi::+meta-key+) 'forward :true)
    (check `(:keyboard #\newline :ignore) 'forward :false)
    ;; Redefine target gestures.
    (define-move)
    (check `(:keyboard #\w :ignore)     'movement :false)
    (check `(:keyboard :left :ignore)   'movement :true)
    (check `(:keyboard :left :ignore)   'movement :true)
    ;; move-f is not redefined (only extended)
    (check `(:keyboard #\space :ignore) 'movement :true)
    ;; forward movement
    (check `(:keyboard #\space ,climi::+no-key+)   'forward :true)
    (check `(:keyboard #\space ,climi::+meta-key+) 'forward :false)
    (check `(:pointer-button ,climi::+pointer-left-button+ ,climi::+no-key+)   'forward :true)
    (check `(:pointer-button ,climi::+pointer-left-button+ ,climi::+meta-key+) 'forward :true)
    (check `(:keyboard #\newline :ignore) 'forward :true)
    (reset-gestures)))
