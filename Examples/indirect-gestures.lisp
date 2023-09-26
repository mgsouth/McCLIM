(defpackage "CLIM-DEMO.INDIRECT-GESTURES"
  (:use "CLIM-LISP" "CLIM")
  (:export "THE-GAME"))
(in-package "CLIM-DEMO.INDIRECT-GESTURES")

;;; This example illustrates a new extension proposed by Andrea De Michele. To
;;; allow more flexibility with gesture mappings indirect gestures allow to
;;; treat named gestures as physical gestures. This enables two things:
;;;
;;; - defining a local gesture that uses a global gesture to match events
;;; - defining a gesture matching different gestures (i.e "movement group")
;;;

;;; First we define two alternative mappings for the movement keys.
(define-gesture-name wsad-up     :keyboard #\w)
(define-gesture-name wsad-down   :keyboard #\s)
(define-gesture-name wsad-left   :keyboard #\a)
(define-gesture-name wsad-right  :keyboard #\d)
;;;
(define-gesture-name arrow-up    :keyboard :up)
(define-gesture-name arrow-down  :keyboard :down)
(define-gesture-name arrow-left  :keyboard :left)
(define-gesture-name arrow-right :keyboard :right)

(define-application-frame the-game ()
  ((col :initform 0 :accessor col)
   (row :initform 0 :accessor row))
  (:panes
   ;(int :interactor)
   (app :application
        :scroll-bars nil
        :display-function #'display))
  (:menu-bar (("Movement"
               :menu (("WSAD"   :command use-wsad)
                      ("Arrows" :command use-arrows))))))

(defgeneric display (frame stream)
  (:method ((frame the-game) stream)
    (with-scaling (stream 50)
      (dotimes (i 100)
        (with-translation (stream (truncate i 10) (rem i 10))
          (draw-circle* stream .5 .5 .55 :ink +deep-sky-blue+)))
      (with-translation (stream (col frame) (row frame))
        (draw-circle* stream .5 .5 .45 :ink +deep-pink+)))))

;;; The following two commands are responsible for remapping controls.
(define-the-game-command use-wsad ()
  (define-gesture-name move-north :indirect wsad-up)
  (define-gesture-name move-south :indirect wsad-down)
  (define-gesture-name move-west  :indirect wsad-left)
  (define-gesture-name move-east  :indirect wsad-right))

(define-the-game-command use-arrows ()
  (define-gesture-name move-north :indirect arrow-up)
  (define-gesture-name move-south :indirect arrow-down)
  (define-gesture-name move-west  :indirect arrow-left)
  (define-gesture-name move-east  :indirect arrow-right))

;;; Now we define commands that move the character.
;;; We could make a macro for these four operators but that would be wankery.
(define-the-game-command (move-north :keystroke move-north) ()
  (with-application-frame (frame)
    (setf (row frame) (max 0 (1- (row frame))))))

(define-the-game-command (move-south :keystroke move-south) ()
  (with-application-frame (frame)
    (setf (row frame) (min 9 (1+ (row frame))))))

(define-the-game-command (move-west :keystroke move-west) ()
  (with-application-frame (frame)
    (setf (col frame) (max 0 (1- (col frame))))))

(define-the-game-command (move-east :keystroke move-east) ()
  (with-application-frame (frame)
    (setf (col frame) (min 9 (1+ (col frame))))))
