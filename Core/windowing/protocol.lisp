;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2022 by Daniel Kochmański <daniel@turtleware.eu>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; Windowing protocol (with extensions).
;;;
(in-package "CLIM-INTERNALS")

;;; Windowing Substrate

;;; 7 Properties of Sheets

;;; 7.1 Basic Sheet Classes

(pledge :class basic-sheet (sheet))
(defgeneric sheet-name (instance))
(define-accessor sheet-pretty-name (new-value instance))
(define-accessor sheet-icon (new-value instance))
(define-accessor sheet-pointer-cursor (new-value instance))

;;; 7.2 Relationships Between Classes

;;; 7.2.1 Sheet Relationship Functions
(defgeneric sheet-parent (instance))
(defgeneric sheet-children (instance))
(defgeneric sheet-child (instance))
(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child &key errorp))
(defgeneric sheet-siblings (sheet))
(defgeneric sheet-enabled-children (sheet))
(defgeneric sheet-ancestor-p (sheet putative-ancestor))
(defgeneric raise-sheet (sheet))
(defgeneric bury-sheet (sheet))
(defgeneric reorder-sheets (sheet new-ordering))
(defgeneric shrink-sheet (sheet))
(pledge :condition sheet-is-not-child (error))
(pledge :condition sheet-is-top-level (error))
(pledge :condition sheet-ordering-underspecified (error))
(pledge :condition sheet-is-not-ancestor (error))
(pledge :condition sheet-already-has-parent (error))
(pledge :condition sheet-supports-only-one-child (error))
(define-accessor sheet-enabled-p (new-value instance))
(defgeneric sheet-viewable-p (sheet))
(defgeneric sheet-occluding-sheets (sheet child))
(defgeneric map-over-sheets (fun sheet))

;;; 7.2.2 Sheet Genealogy Classes
(pledge :mixin sheet-parent-mixin)
(pledge :mixin sheet-leaf-mixin)
(pledge :mixin sheet-single-child-mixin)
(pledge :mixin sheet-multiple-child-mixin)

;;; 7.3 Sheet Geometry

;;; 7.3.1 Sheet Geometry Functions
(define-accessor sheet-transformation (new-value instance))
(define-accessor sheet-region (new-value instance))
(defgeneric move-sheet (sheet x y))
(defgeneric resize-sheet (sheet width height))
(defgeneric move-and-resize-sheet (sheet x y width height))
(defgeneric map-sheet-position-to-parent (sheet x y))
(defgeneric map-sheet-position-to-child (sheet x y))
(defgeneric map-sheet-rectangle*-to-parent (sheet x1 y1 x2 y2))
(defgeneric map-sheet-rectangle*-to-child (sheet x1 y1 x2 y2))
(defgeneric map-over-sheets-containing-position (fun sheet x y))
(defgeneric map-over-sheets-overlapping-region (fun sheet region))
(defgeneric child-containing-position (sheet x y))
(defgeneric children-overlapping-region (sheet region))
(defgeneric children-overlapping-rectangle* (sheet x1 y1 x2 y2))
(defgeneric sheet-delta-transformation (sheet ancestor))
(defgeneric sheet-allocated-region (sheet child))

;;; 7.3.1 Sheet Geometry Classes
(pledge :mixin sheet-identity-transformation-mixin)
(pledge :mixin sheet-translation-mixin)
(pledge :mixin sheet-y-inverting-transformation-mixin)
(pledge :mixin sheet-transformation-mixin)

;;; 8 Sheet Protocols

;;; 8.1 Input Protocol

;;; 8.1.1 Input Protocol Functions
(defgeneric sheet-event-queue (sheet))
(defgeneric process-next-event (port &key wait-function timeout))
(define-accessor port-keyboard-input-focus (new-value instance))
(defgeneric note-input-focus-changed (sheet state)
  (:documentation "Called when a sheet receives or loses the keyboard input
focus. STATE argument is T when the sheet gains focus and NIL otherwise. This
is a McCLIM extension."))
(defgeneric distribute-event (port event))
(defgeneric dispatch-event (client event))
(defgeneric queue-event (client event))
(defgeneric schedule-event (client event delay))
(defgeneric handle-event (client event))
(defgeneric event-read (client))
(defgeneric event-read-no-hang (client))
(defgeneric event-peek (client &optional event-type))
(defgeneric event-unread (client event))
(defgeneric event-listen (client))
(defgeneric event-read-with-timeout (client &key timeout wait-function) (:documentation "Reads event from the event queue. Function returns when event is succesfully
read, timeout expires or wait-function returns true. Time of wait-function call
depends on a port."))
(defgeneric event-listen-or-wait (client &key timeout wait-function) (:documentation "When wait-function is nil then function waits for available event. Otherwise
function returns when wait-function predicate yields true. Time of wait-function
call depends on a port."))

(define-protocol-class event-queue nil)
(define-accessor event-queue-port (new-value instance))
(define-accessor event-queue-head (new-value instance))
(define-accessor event-queue-tail (new-value instance))
(pledge :class simple-event-queue (event-queue))
(pledge :class concurrent-event-queue (event-queue))

(defgeneric schedule-event-queue (queue event delay))

(defgeneric event-queue-read (event-queue)
  (:documentation "Reads one event from the queue, if there is no event, hang
until here is one."))

(defgeneric event-queue-read-no-hang (event-queue)
  (:documentation "Reads one event from the queue, if there is no event just
return NIL."))

(defgeneric event-queue-read-with-timeout (event-queue timeout wait-function)
  (:documentation "Waits until wait-function returns true, event queue
is not empty or none of the above happened before a timeout.

- Returns (values nil :wait-function) if wait-function returns true
- Reads and returns one event from the queue if it is not empty
- Returns (values nil :timeout) otherwise."))

(defgeneric event-queue-append (event-queue item)
  (:documentation "Append the item at the end of the queue. Does event compression."))

(defgeneric event-queue-prepend (event-queue item)
  (:documentation "Prepend the item to the beginning of the queue."))

(defgeneric event-queue-peek (event-queue)
  (:documentation "Peeks the first event in a queue. Queue is left unchanged.
If queue is empty returns NIL."))

(defgeneric event-queue-peek-if (predicate event-queue)
  (:documentation "Goes through the whole event queue and returns the first
event, which satisfies PREDICATE. Queue is left unchanged. Returns NIL if there
is no such event."))

(defgeneric event-queue-listen (event-queue)
  (:documentation "Returns true if there are any events in the queue. Otherwise
returns NIL."))

(defgeneric event-queue-listen-or-wait (event-queue &key timeout wait-function)
  (:documentation "Waits until wait-function returns true, event queue
is not empty or none of the above happened before a timeout.

- Returns (values nil :wait-function) when wait-function returns true
- Returns true when there are events in the queue before a timeout
- Returns (values nil :timeout) otherwise."))

;;; 8.1.2 Input Protocol Classes
(pledge :mixin standard-sheet-input-mixin)
(pledge :mixin immediate-sheet-input-mixin)
(pledge :mixin sheet-mute-input-mixin)
(pledge :mixin delegate-sheet-input-mixin)
(define-accessor delegate-sheet-delegate (new-value instance))
(pledge :mixin clim-sheet-input-mixin)

;;; 8.2 Standard Device Events
(define-protocol-class event nil nil (:default-initargs :timestamp nil))
(pledge :macro define-event-class (name superclasses slots &rest options))
(defgeneric event-timestamp (instance))
(defgeneric event-type (instance))
(pledge :class device-event (event) nil (:default-initargs :sheet nil :modifier-state nil))
(defgeneric device-event-x (instance))
(defgeneric device-event-y (instance))
(defgeneric device-event-native-x (instance))
(defgeneric device-event-native-y (instance))
(defgeneric event-sheet (instance))
(defgeneric event-modifier-state (instance))
(pledge :class keyboard-event (device-event) nil (:default-initargs :key-name nil))
(defgeneric keyboard-event-key-name (instance))
(defgeneric keyboard-event-character (instance))
(pledge :class key-press-event (keyboard-event))
(pledge :class key-release-event (keyboard-event))
(pledge :class pointer-event (device-event) nil (:default-initargs :pointer nil :x nil :y nil))
(defgeneric pointer-event-x (instance))
(defgeneric pointer-event-y (instance))
(defgeneric pointer-event-native-x (instance))
(defgeneric pointer-event-native-y (instance))
(defgeneric pointer-event-pointer (instance))
(pledge :class pointer-button-event (pointer-event))
(defgeneric pointer-event-button (instance))
(pledge :class pointer-button-press-event (pointer-button-event))
(pledge :class pointer-button-release-event (pointer-button-event))
(pledge :class pointer-button-hold-event (pointer-button-event))
(pledge :class pointer-click-event (pointer-button-event))
(pledge :class pointer-double-click-event (pointer-button-event))
(pledge :class pointer-click-and-hold-event (pointer-button-event))
(pledge :class pointer-scroll-event (pointer-button-event))
(defgeneric pointer-event-delta-x (instance))
(defgeneric pointer-event-delta-y (instance))
(pledge :class pointer-motion-event (pointer-event))
(pledge :class pointer-boundary-event (pointer-motion-event))
(defgeneric synthesize-pointer-motion-event (port pointer)
  (:documentation "Create a CLIM pointer motion event based on the current pointer state."))
(declfun synthesize-boundary-events (port event))
(defgeneric pointer-boundary-event-kind (pointer-boundary-event))

(defgeneric pointer-update-state (pointer event)
  (:documentation "Called by port event dispatching code to update the modifier
and button states of the pointer."))

(pledge :class pointer-enter-event (pointer-boundary-event))
(pledge :class pointer-exit-event (pointer-boundary-event))
(pledge :class pointer-grab-enter-event (pointer-enter-event))
(pledge :class pointer-grab-leave-event (pointer-exit-event))
(pledge :class pointer-ungrab-enter-event (pointer-enter-event))
(pledge :class pointer-ungrab-leave-event (pointer-exit-event))
(pledge :class window-event (event) nil (:default-initargs :region))
(defgeneric window-event-region (instance))
(defgeneric window-event-native-region (instance))
(defgeneric window-event-mirrored-sheet (instance))
(pledge :class window-configuration-event (window-event))
(pledge :class window-repaint-event (window-event))
(pledge :class window-map-event (window-event))
(pledge :class window-unmap-event (window-event))
(pledge :class window-destroy-event (window-event))
(pledge :class window-manager-event (window-event) nil (:default-initargs :sheet))
(pledge :class window-manager-delete-event (window-manager-event))
(pledge :class window-manager-focus-event (window-manager-event))
(pledge :class window-manager-iconify-event (window-manager-event))
(pledge :class window-manager-deiconify-event (window-manager-event))
(pledge :class timer-event (event))
(pledge :class lambda-event (event))
(defgeneric lambda-event-thunk (instance))
(pledge :macro with-synchronization (sheet test &body body))
(pledge :constant +pointer-left-button+ fixnum)
(pledge :constant +pointer-middle-button+ fixnum)
(pledge :constant +pointer-right-button+ fixnum)
(pledge :constant +pointer-wheel-up+ fixnum)
(pledge :constant +pointer-wheel-down+ fixnum)
(pledge :constant +pointer-wheel-left+ fixnum)
(pledge :constant +pointer-wheel-right+ fixnum)
(pledge :constant +shift-key+ fixnum)
(pledge :constant +control-key+ fixnum)
(pledge :constant +meta-key+ fixnum)
(pledge :constant +super-key+ fixnum)
(pledge :constant +hyper-key+ fixnum)
(pledge :constant +alt-key+ fixnum)

;;; 8.3 Output Protocol

;;; 8.3.3 Output Protocol Functions
(pledge :mixin standard-sheet-output-mixin)
(pledge :mixin sheet-mute-output-mixin)
(pledge :mixin sheet-with-medium-mixin)
(pledge :mixin permanent-medium-sheet-output-mixin)
(pledge :mixin temporary-medium-sheet-output-mixin)

;;; 8.3.4 Associating a Medium with a Sheet
(pledge :macro with-sheet-medium ((medium sheet) &body body))
(pledge :macro with-sheet-medium-bound ((medium sheet) &body body))
(defgeneric invoke-with-sheet-medium (cont sheet))
(defgeneric invoke-with-sheet-medium-bound (cont medium sheet))
(defgeneric sheet-medium (instance))

;;; 8.3.4.1 Grafting and Degrafting of Mediums
(defgeneric allocate-medium (port sheet))
(defgeneric deallocate-medium (port medium))
(defgeneric make-medium (port sheet))
(defgeneric engraft-medium (medium port sheet))
(defgeneric degraft-medium (medium port sheet))

;;; 8.4 Repaint Protocol

;;; 8.4.1 Repaint Protocol Functions
(defgeneric dispatch-repaint (sheet region))
(defgeneric queue-repaint (sheet region))
(defgeneric handle-repaint (sheet region))
(defgeneric repaint-sheet (sheet region))

;;; 8.4.2 Repaint Protocol Classes
(pledge :mixin standard-repainting-mixin)
(pledge :mixin immediate-repainting-mixin)
(pledge :mixin sheet-mute-repainting-mixin)
(pledge :mixin always-repaint-background-mixin)
(pledge :mixin never-repaint-background-mixin)
(pledge :mixin clim-repainting-mixin)

;;; 8.5 Sheet Notification Protocol

;;; 8.5.1 Relationship to Window System Change Notifications
(defgeneric note-sheet-grafted (sheet))
(defgeneric note-sheet-degrafted (sheet))
(defgeneric note-sheet-adopted (sheet))
(defgeneric note-sheet-disowned (sheet))
(defgeneric note-sheet-enabled (sheet))
(defgeneric note-sheet-disabled (sheet))

(defgeneric note-sheet-grafted-internal (port sheet))
(defgeneric note-sheet-degrafted-internal (port sheet))

;;; 8.5.2 Sheet Geometry Notifications
(defgeneric note-sheet-region-changed (sheet))
(defgeneric note-sheet-transformation-changed (sheet))

;;; 9 Ports, Grafts and Mirrored Sheets

;;; 9.2 Ports

(pledge :class basic-port)
(declfun find-port (&key (server-path *default-server-path*)))
(defgeneric find-port-type (symbol))
(pledge :variable *default-server-path*)
(defgeneric port (instance))
(pledge :macro with-port ((port-var server &rest args &key &allow-other-keys) &body body))
(declfun invoke-with-port (continuation server &rest args &key &allow-other-keys))
(pledge :macro with-port-locked ((port) &body body))
(defgeneric invoke-with-port-locked (port continuation))
(declfun map-over-ports (fun))
(defgeneric port-server-path (instance))
(defgeneric port-name (instance))
(defgeneric port-type (instance))
(defgeneric port-modifier-state (instance))
(defgeneric port-properties (port indicator))
(defgeneric (setf port-properties) (property port indicator))
(defgeneric restart-port (port))
(defgeneric destroy-port (port))
(define-accessor port-grafts (new-value instance))
(define-accessor frame-managers (new-value instance))
(define-accessor port-event-process (new-value instance))
(define-accessor port-lock (new-value instance))
(defgeneric port-text-style-mappings (instance))
(define-accessor port-pointer (new-value instance))
(defgeneric port-cursors (instance))
(defgeneric port-selections (instance))
(define-accessor port-grabbed-sheet (new-value instance))
(define-accessor port-pressed-sheet (new-value instance))
(declfun stored-object (port selection))
(declfun remove-stored-object (port selection))

;;; McCLIM extension: Font listing
(defgeneric port-all-font-families (port &key invalidate-cache &allow-other-keys)
  (:documentation "Returns the list of all FONT-FAMILY instances known by PORT.
With INVALIDATE-CACHE, cached font family information is discarded, if any."))

(defgeneric font-family-name (font-family)
  (:documentation "Return the font family's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style family for this port."))

(defgeneric font-family-port (font-family)
  (:documentation "Return the port this font family belongs to."))

(defgeneric font-family-all-faces (font-family)
  (:documentation "Return the list of all font-face instances for this family."))

(defgeneric font-face-name (font-face)
  (:documentation "Return the font face's name.  This name is meant for user display,
and does not, at the time of this writing, necessarily the same string
used as the text style face for this port."))

(defgeneric font-face-family (font-face)
  (:documentation "Return the font family this face belongs to."))

(defgeneric font-face-all-sizes (font-face)
  (:documentation "Return the list of all font sizes known to be valid for this font,
if the font is restricted to particular sizes.  For scalable fonts, arbitrary
sizes will work, and this list represents only a subset of the valid sizes.
See font-face-scalable-p."))

(defgeneric font-face-scalable-p (font-face)
  (:documentation "Return true if this font is scalable, as opposed to a bitmap font.  For
a scalable font, arbitrary font sizes are expected to work."))

(defgeneric font-face-text-style (font-face &optional size)
  (:documentation "Return an extended text style describing this font face in the specified
size.  If size is nil, the resulting text style does not specify a size."))

(pledge :class font-family)
(pledge :class font-face)
(pledge :class basic-font-family)
(pledge :class basic-font-face)

;;; 9.3 Grafts
(pledge :class graft nil)
(declfun graftp (graft))
(defgeneric make-graft (port &key orientation units))
(defgeneric sheet-grafted-p (sheet))
(declfun find-graft (&key (port nil) (server-path *default-server-path*) (orientation :default) (units :device)))
(defgeneric graft (instance))
(defgeneric map-over-grafts (fun port))
(pledge :macro with-graft-locked ((graft) &body body))
(defgeneric graft-orientation (instance))
(defgeneric graft-units (instance))
(defgeneric graft-width (graft &key units))
(defgeneric graft-height (graft &key units))
(declfun graft-pixels-per-millimeter (graft &key orientation))
(declfun graft-pixels-per-inch (graft &key orientation))
(defgeneric graft-pixel-aspect-ratio (graft))

;;; 9.4 Mirrors and Mirrored Sheets
(pledge :mixin mirrored-sheet-mixin)
(pledge :mixin top-level-sheet-mixin)
(pledge :mixin unmanaged-sheet-mixin)
(declfun get-top-level-sheet (sheet))

;;; 9.4.1 Mirror Functions
(defgeneric sheet-direct-mirror (sheet))
(defgeneric sheet-mirrored-ancestor (sheet))
(defgeneric sheet-mirror (sheet))
(defgeneric realize-mirror (port mirrored-sheet))
(defgeneric destroy-mirror (port mirrored-sheet))
(defgeneric raise-mirror (port sheet))
(defgeneric bury-mirror (port sheet))
(defgeneric set-mirror-name (port sheet name))
(defgeneric set-mirror-icon (port sheet icon))
(defgeneric set-mirror-geometry (port sheet region))
(defgeneric enable-mirror (port sheet))
(defgeneric disable-mirror (port sheet))
(defgeneric shrink-mirror (port sheet))
(defgeneric unshrink-mirror (port sheet))
(define-accessor sheet-mirror-geometry (new-value instance))
(defgeneric update-mirror-geometry (sheet))
(defgeneric (setf %sheet-direct-mirror) (new-val sheet))

;;; 9.4.2 Internal Interfaces for Native Coordinates
(defgeneric sheet-native-transformation (instance))
(defgeneric sheet-native-region (instance))
(defgeneric sheet-device-transformation (instance))
(defgeneric sheet-device-region (instance))
(defgeneric invalidate-cached-transformations (sheet))
(defgeneric invalidate-cached-regions (sheet))

;;; 22.4 The Pointer Protocol
(define-protocol-class pointer nil)
(pledge :class standard-pointer)
(define-accessor pointer-sheet (new-value instance))
(defgeneric pointer-button-state (instance))
(define-accessor pointer-position (x y pointer))
(define-accessor pointer-cursor (new-value pointer))
(pledge :macro with-pointer-grabbed ((port sheet &key pointer multiple-window) &body body))
(defgeneric port-force-output (port)
  (:documentation "Flush the output buffer of PORT, if there is one."))
(defgeneric port-grab-pointer (port pointer sheet &key multiple-window)
  (:documentation "Grab the specified pointer."))
(defgeneric port-ungrab-pointer (port pointer sheet)
  (:documentation "Ungrab the specified pointer."))
(defgeneric set-sheet-pointer-cursor (port sheet cursor)
  (:documentation "Sets the cursor associated with SHEET. CURSOR is a symbol, as described in the Franz user's guide."))
