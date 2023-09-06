(in-package #:clim-user)

;;; A complete sheet should subclass basic-sheet and pick its mixins:
;;;
;;;   Input protocol (xor):
;;;
;;;     standard-sheet-input-mixin
;;;     delegate-sheet-input-mixin
;;;     immediate-sheet-input-mixin
;;;     sheet-mute-input-mixin
;;;
;;;   Output protocol:
;;;
;;;     standard-sheet-output-mixin xor sheet-mute-output-mixin
;;;     sheet-with-medium-mixin
;;;         permanent-medium-sheet-output-mixin
;;;         temporary-medium-sheet-output-mixin
;;;
;;;   Genealogy:
;;;
;;;     sheet-parent-mixin
;;;     sheet-leaf-mixin xor sheet-single-child-mixin xor sheet-multiple-child-mixin
;;;
;;;   Repainting (xor): [clim-repainting-mixin is a default]
;;;
;;;     standard-repainting-mixin
;;;     immediate-repainting-mixin
;;;     sheet-mute-repainting-mixin
;;;
;;;   Geometry (xor):
;;;
;;;     sheet-identity-transformation-mixin
;;;     sheet-translation-mixin
;;;     sheet-y-inverting-transformation-mixin
;;;     sheet-transformation-mixin
;;;
;;;   Windowing (zero or more, may be mixed, the order of mixins is important)
;;;
;;;     top-level-sheet-mixin
;;;     unmanaged-sheet-mixin
;;;     mirrored-sheet-mixin
;;;

(defvar *glider*
  (make-pattern-from-bitmap-file
   (asdf:component-pathname
    (asdf:find-component "clim-examples" '("images" "glider.png")))))

(defclass plain-sheet (;; repainting
                       immediate-repainting-mixin
                       ;; input
                       immediate-sheet-input-mixin
                       ;; output
                       permanent-medium-sheet-output-mixin
                       ;temporary-medium-sheet-output-mixin
                       ;sheet-with-medium-mixin
                       ;sheet-mute-output-mixin
                       ;; geometry
                       sheet-transformation-mixin
                       ;; genealogy
                       sheet-parent-mixin
                       sheet-leaf-mixin
                       ;; windowing
                       top-level-sheet-mixin
                       mirrored-sheet-mixin
                       ;; the base class
                       basic-sheet)
  ()
  (:default-initargs :icon *glider*
                     :pretty-name "McCLIM Test Sheet"
                     :region (make-rectangle* -200 -200 200 200)
                     :transformation (make-scaling-transformation 2 2)))

(defmethod handle-event ((sheet plain-sheet) event)
  )

(defvar *title* "McCLIM Test Sheet")
(defvar *extra* nil)

(defun update-title (sheet)
  (setf (sheet-pretty-name sheet)
        (format nil "~a ~{~s~^ ~}" *title* *extra*)))

(defun open-plain-sheet (path sheet)
  (let ((port (find-port :server-path path)))
    (let ((graft (find-graft :port port)))
      (sheet-adopt-child graft sheet)
      ;; FIXME CLX thinks that every tpl sheet is adopted by a frame.
      (climb:enable-mirror port sheet)
      sheet)))

(defun make-plain-sheet ()
  (make-instance 'plain-sheet))

(defun close-plain-sheet (sheet)
  (sheet-disown-child (graft sheet) sheet)
  nil)

(defmethod handle-event ((sheet plain-sheet) (event window-manager-delete-event))
  (sheet-disown-child (graft sheet) sheet))

(defmethod handle-event ((sheet plain-sheet) (event window-repaint-event))
  (dispatch-repaint sheet (window-event-region event)))

;;; It may be surprising that nobody updates SHEET-MIRROR-GEOMETRY but
;;; HANDLE-SDL2-WINDOW-EVENT gets correct values. This is because of a
;;; HANDLE-EVENT :BEFORE method specialized to MIRRORED-SHEET-MIXIN in core.
;;; Whether that method stays depends on how we resolve the FIXME above.

(defmethod handle-event ((sheet plain-sheet) (event window-configuration-event))
  (with-bounding-rectangle* (x1 y1 x2 y2 :width w :height h)
                            (window-event-native-region event)
    (let ((climi::*configuration-event-p* sheet))
      (let ((new-transformation (make-translation-transformation x1 y1))
            (new-region (make-bounding-rectangle 0 0 w h)))
        ;; This indirectly modifies the native region.
        (climi::%set-sheet-region-and-transformation sheet new-region new-transformation)))
    (setf (getf *extra* :dims)
          (format nil "[~s ~s ~s ~s] (~s x ~s)" x1 y1 x2 y2 w h))
    (update-title sheet)))

(defmethod handle-event ((sheet plain-sheet) (event window-manager-focus-event))
  (setf (port-keyboard-input-focus (port sheet)) sheet))

(defmethod handle-event ((sheet plain-sheet) (event pointer-enter-event))
  (setf (getf *extra* :pointer) "y")
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event pointer-exit-event))
  (setf (getf *extra* :pointer) "n")
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event key-press-event))
  (setf (getf *extra* :key) (keyboard-event-key-name event))
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event key-release-event))
  (setf (getf *extra* :key) nil)
  (update-title sheet))

#+ (or)
(defmethod handle-event ((sheet plain-sheet) (event text-input-event))
  (setf (getf *extra* :str) (text-input-event-string event))
  (when (string= " " (text-input-event-string event))
    (repaint-sheet sheet +everywhere+))
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event pointer-motion-event))
  (setf (getf *extra* :pointer)
        (format nil "ptr+~s [~s ~s]"
                (event-modifier-state event)
                (pointer-event-x event)
                (pointer-event-y event)))
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event climb:pointer-scroll-event))
  (setf (getf *extra* :pointer)
        (format nil "scr+~s [~s ~s]"
                (event-modifier-state event)
                (climi::pointer-event-delta-x event)
                (climi::pointer-event-delta-y event)))
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event pointer-button-press-event))
  (setf (getf *extra* :pointer)
        (format nil "bdn+~s ~s"
                (event-modifier-state event)
                (pointer-event-button event)))
  (update-title sheet))

(defmethod handle-event ((sheet plain-sheet) (event pointer-button-release-event))
  (setf (getf *extra* :pointer)
        (format nil "bup+~s ~s"
                (event-modifier-state event)
                (pointer-event-button event)))
  (update-title sheet))

;;; Repainting protocol
(defmethod handle-repaint ((sheet plain-sheet) region)
  (let ((medium sheet))
    (with-bounding-rectangle* (x1 y1 x2 y2) medium
      (medium-clear-area medium x1 y1 x2 y2)
      (draw-rectangle* medium (+ x1 10) (+ y1 10) (- x2 10) (- y2 10)
                       :ink +deep-pink+)
      (draw-circle* medium 0 0 75 :ink (alexandria:random-elt
                                        (make-contrasting-inks 8)))
                                        ;(draw-text* medium "(0,0)" 0 0)
                                        ;(sleep 1)
      (medium-finish-output medium))))

(defun start (server-path)
  (open-plain-sheet server-path (make-plain-sheet)))

;;; This function migrates a frame from one port to another without
;;; reconstructing the pane hierarchy.
(defun wololo (frame port)
  (let ((tpl-sheet (frame-top-level-sheet frame))
        (new-graft (find-graft :port port)))
    (sheet-disown-child (sheet-parent tpl-sheet) tpl-sheet)
    (sheet-adopt-child new-graft tpl-sheet)
    (climb:enable-mirror port tpl-sheet)))
