;;; ---------------------------------------------------------------------------
;;;   License: LGPL-2.1+ (See file 'Copyright' for details).
;;; ---------------------------------------------------------------------------
;;;
;;;  (c) Copyright 2001,2002 by Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;  (c) Copyright 2014 by Robert Strandh <robert.strandh@gmail.com>
;;;
;;; ---------------------------------------------------------------------------
;;;
;;; DEFGENERICs and stuff
;;;

(in-package #:clim-internals)

;;; This is just an ad hoc list. Would it be a good idea to include all
;;; (exported) generic functions here? --GB
;;;
;;; YES!  -- CSR
;;; We'll get right on it :) -- moore
;;; Whose numbers are we using here?

;;; The numbers are section numbers from the spec. --GB

;;; 23.2 Presentations

(define-accessor presentation-object (value presentation))
(define-accessor presentation-type (value presentation))
(define-accessor presentation-view (value presentation))
(define-accessor presentation-single-box (value presentation))
(defgeneric presentation-modifier (presentation))

(declfun make-blank-area-presentation (sheet x y event))

;;; 23.4 Typed output

(pledge :macro with-output-as-presentation
        ((stream object type &rest key-args
          &key modifier single-box parent
               allow-sensitive-inferiors record-type
          &allow-other-keys)
         &body body))

(defgeneric invoke-with-output-as-presentation
    (cont stream object type &rest key-args &key &allow-other-keys))

(declfun present (object &optional type
                  &key stream view modifier acceptably for-context-type
                       single-box sensitive allow-sensitive-inferiors
                       record-type))

(defgeneric stream-present
    (stream object type
     &key view modifier acceptably for-context-type single-box
       allow-sensitive-inferiors sensitive record-type))

;;; 23.5 Context-dependent (Typed) Input

(defgeneric stream-accept
    (stream
     type &key view default default-type provide-default insert-default
     replace-input history active-p prompt prompt-mode display-default
     query-identifier activation-gestures additional-activation-gestures
     delimiter-gestures additional-delimiter-gestures))
(defgeneric prompt-for-accept (stream type view &rest accept-args &key))

;;; 23.7 Presentation Translators
(declfun highlight-applicable-presentation
    (frame stream input-context &optional prefer-pointer-window))


;;; 24.1 The Input Editor

(defgeneric input-editor-format (stream format-string &rest args)
  (:documentation "This function is like `format', except that it
is intended to be called on input editing streams. It arranges to
insert \"noise strings\" in the input editor's input
buffer. Programmers can use this to display in-line prompts in
`accept' methods.

If `stream' is a stream that is not an input editing stream, then
`input-editor-format' is equivalent to format."))


(defgeneric redraw-input-buffer (stream &optional start-from)
  (:documentation "Displays the input editor's buffer starting at
the position `start-position' on the interactive stream that is
encapsulated by the input editing stream `stream'."))

;;; 24.1.1 The Input Editing Stream Protocol

(define-accessor stream-insertion-pointer (value stream))
(define-accessor stream-scan-pointer (value stream))

(defgeneric stream-rescanning-p (stream)
  (:documentation "Returns the state of the input editing stream
`stream's \"rescan in progress\" flag, which is true if stream is
performing a rescan operation, otherwise it is false. All
extended input streams must implement a method for this, but
non-input editing streams will always returns false."))

(defgeneric reset-scan-pointer (stream &optional scan-pointer)
  (:documentation "Sets the input editing stream stream's scan
pointer to `scan-pointer', and sets the state of
`stream-rescanning-p' to true."))

(defgeneric immediate-rescan (stream)
  (:documentation "Invokes a rescan operation immediately by
\"throwing\" out to the most recent invocation of
`with-input-editing'."))

(defgeneric queue-rescan (stream)
  (:documentation "Indicates that a rescan operation on the input
editing stream `stream' should take place after the next
non-input editing gesture is read by setting the \"rescan
queued\" flag to true. "))

(defgeneric rescan-if-necessary (stream &optional inhibit-activation)
  (:documentation "Invokes a rescan operation on the input
editing stream `stream' if `queue-rescan' was called on the same
stream and no intervening rescan operation has taken
place. Resets the state of the \"rescan queued\" flag to false.

If `inhibit-activation' is false, the input line will not be
activated even if there is an activation character in it."))

(defgeneric erase-input-buffer (stream &optional start-position)
  (:documentation "Erases the part of the display that
corresponds to the input editor's buffer starting at the position
`start-position'."))
;;; 24.4 Reading and Writing of Tokens

(defgeneric replace-input
    (stream new-input &key start end buffer-start rescan)
  ;; XXX: Nonstandard behavior for :rescan.
  (:documentation "Replaces the part of the input editing stream
`stream's input buffer that extends from `buffer-start' to its
scan pointer with the string `new-input'. `buffer-start' defaults
to the current input position of stream, which is the position at
which the current accept \"session\" starts. `start' and `end' can be
supplied to specify a subsequence of `new-input'; start defaults to
0 and end defaults to the length of `new-input'.

`replace-input' will queue a rescan by calling `queue-rescan' if
the new input does not match the old input, or `rescan' is
true. If `rescan' is explicitly provided as NIL, no rescan will
be queued in any case.

The returned value is the position in the input buffer."))

(defgeneric presentation-replace-input
    (stream object type view
            &key buffer-start rescan query-identifier for-context-type)
  (:documentation "Like `replace-input', except that the new
input to insert into the input buffer is gotten by presenting
`object' with the presentation type `type' and view
`view'. `buffer-start' and `rescan' are as for `replace-input',
and `query-identifier' and `for-context-type' as as for
`present'.

Typically, this function will be implemented by calling
`present-to-string' on `object', `type', `view', and
`for-context-type', and then calling `replace-input' on the
resulting string.

If the object cannot be transformed into an acceptable textual
form, it may be inserted as a special \"accept result\" that is
considered a single gesture. These accept result objects have no
standardised form."))


;;; 26 Dialog Facilities
(defgeneric display-exit-boxes (frame stream view))
(defgeneric accept-values-resynchronize (stream))


;;; 27.2 Command Tables
(defgeneric command-table-name (command-table))
;;; Franz user manual defines a setter.
(define-accessor command-table-inherit-from (inherit-from command-table))

;;; 27.3 Command Menus

(defgeneric display-command-table-menu (command-table stream
                                        &key max-width max-height
                                          n-rows n-columns x-spacing
                                          y-spacing initial-spacing row-wise
                                          cell-align-x cell-align-y move-cursor)
  (:documentation "Display a menu of the commands accessible in
`command-table' to `stream'.

`max-width', `max-height', `n-rows', `n-columns', `x-spacing',
`y-spacing', `row-wise', `initial-spacing', `cell-align-x',
`cell-align-y', and `move-cursor' are as for
`formatting-item-list'."))


;;; 28.2 Specifying the Panes of a Frame

(defgeneric destroy-frame (frame))
(defgeneric raise-frame (frame))
(defgeneric bury-frame (frame))

;;; 28.3 Application Frame Functions

(defgeneric frame-name (frame))
(define-accessor frame-pretty-name (name frame))
(define-accessor frame-icon (icon frame))
(define-accessor frame-command-table (command-table frame))

(defgeneric frame-standard-output (frame)
  (:documentation "Returns the stream that will be used for
`*standard-output*' for the frame `frame'. The default method (on
`standard-application-frame') returns the first named pane of type
`application-pane' that is visible in the current layout; if there is
no such pane, it returns the first pane of type `interactor-pane' that
is exposed in the current layout."))

(defgeneric frame-standard-input (frame))
(defgeneric frame-query-io (frame))
(defgeneric frame-error-output (frame))
(defgeneric frame-pointer-documentation-output (frame))
(defgeneric frame-calling-frame (frame))
(defgeneric frame-parent (frame))
;;; The setter is missing in the CLIM 2 spec (omission?)
(define-accessor frame-panes (panes frame))
(defgeneric frame-top-level-sheet (frame))
(defgeneric frame-current-panes (frame))
(defgeneric get-frame-pane (frame pane-name))

(defgeneric find-pane-named (frame pane-name)
  (:documentation "Returns the pane in the frame `frame' whose name is
`pane-name'. This can return any type of pane, not just CLIM stream
panes."))

(define-accessor frame-current-layout (layout frame))
(defgeneric frame-all-layouts (frame))
(defgeneric layout-frame (frame &optional width height))
(defgeneric frame-exit-frame (condition))
(defgeneric frame-exit (frame))
(define-accessor pane-needs-redisplay (value pane))
(defgeneric redisplay-frame-pane (frame pane &key force-p))
(defgeneric redisplay-frame-panes (frame &key force-p))
(defgeneric frame-replay (frame stream &optional region))
(defgeneric notify-user (frame message &key associated-window title
                               documentation exit-boxes name style text-style))
(defgeneric frame-properties (frame property))
(defgeneric (setf frame-properties) (value frame property))

;;; 28.3.1 Interface with Presentation Types

(defgeneric frame-maintain-presentation-histories (frame))
(defgeneric frame-find-innermost-applicable-presentation
    (frame input-context stream x y &key event))
(defgeneric frame-input-context-button-press-handler
    (frame stream button-press-event))
(defgeneric frame-input-context-track-pointer
    (frame input-context stream event))
(defgeneric frame-document-highlighted-presentation
    (frame presentation input-context window-context x y stream))
(defgeneric frame-drag-and-drop-feedback
    (frame presentation stream initial-x initial-y new-x new-y state))
(defgeneric frame-drag-and-drop-highlighting
    (frame presentation stream state))

;;;; 28.4
(defgeneric default-frame-top-level
    (frame &key command-parser command-unparser partial-command-parser prompt))
(defgeneric read-frame-command (frame &key stream))
(defgeneric execute-frame-command (frame command))
(defgeneric run-frame-top-level (frame &key &allow-other-keys))
(defgeneric command-enabled (command-name frame))
(defgeneric (setf command-enabled) (enabled command-name frame))
(defgeneric (setf command-name) (enabled command-name frame))
(defgeneric display-command-menu (frame stream &key command-table
                                        initial-spacing row-wise max-width
                                        max-height n-rows n-columns
                                        cell-align-x cell-align-y)
  (:documentation "Display the command table associated with
`command-table' on `stream' by calling
`display-command-table-menu'. If no command table is
provided, (frame-command-table frame) will be used.

The arguments `initial-spacing', `row-wise',
`max-width', `max-height', `n-rows', `n-columns', `cell-align-x',
and `cell-align-y' are as for `formatting-item-list'."))

;;;; 28.5.2 Frame Manager Operations

(define-accessor frame-manager (frame-manager frame))
(defgeneric frame-manager-frames (frame-manager))
(defgeneric adopt-frame (frame-manager frame))
(defgeneric disown-frame (frame-manager frame))
(defgeneric frame-state (frame))
(defgeneric enable-frame (frame))
(defgeneric disable-frame (frame))
(defgeneric shrink-frame (frame))

(defgeneric note-frame-enabled (frame-manager frame))
(defgeneric note-frame-disabled (frame-manager frame))
(defgeneric note-frame-iconified (frame-manager frame))
(defgeneric note-frame-deiconified (frame-manager frame))
(defgeneric note-command-enabled (frame-manager frame command-name))
(defgeneric note-command-disabled (frame-manager frame command-name))

(defgeneric note-frame-pretty-name-changed (frame-manager frame new-name)
  (:documentation "McCLIM extension: Notify client that the pretty
name of FRAME, managed by FRAME-MANAGER, changed to NEW-NAME."))

(defgeneric note-frame-icon-changed (frame-manager frame new-icon)
  (:documentation "McCLIM extension: Notify client that the icon of
FRAME, managed by FRAME-MANAGER, changed to NEW-ICON."))

(defgeneric note-frame-command-table-changed (frame-manager frame new-command-table)
  (:documentation "McCLIM extension: Notify client that the command-table of
FRAME, managed by FRAME-MANAGER, changed to NEW-COMMAND-TABLE."))

(defgeneric frame-manager-notify-user
    (framem message-string
     &key frame associated-window title
       documentation exit-boxes name style text-style))

(defgeneric generate-panes (frame-manager frame))
(defgeneric find-pane-for-frame (frame-manager frame))

;;; 28.5.3 Frame Manager Settings

(defgeneric (setf client-setting) (value frame setting))
(defgeneric reset-frame (frame &rest client-settings))


;;;; 29.2
;;;;
;;;; FIXME: should we have &key &allow-other-keys here, to cause
;;;; initarg checking?  Probably.
(defgeneric make-pane-1 (realizer frame abstract-class-name &rest initargs))
(defgeneric reinitialize-pane (pane &rest initargs)
  (:method (pane &rest initargs)
    (apply #'reinitialize-instance pane initargs)))

;;;; 29.2.2 Pane Properties

(defgeneric pane-frame (pane))
(defgeneric pane-name (pane))
(defgeneric pane-foreground (pane))
(defgeneric pane-background (pane))
(defgeneric pane-text-style (pane))

;;;; 29.3.3 Scroller Pane Classes

(defgeneric pane-viewport (pane))
(defgeneric pane-viewport-region (pane))
(defgeneric pane-scroller (pane))
(defgeneric scroll-extent (pane x y))
(defgeneric scroll-quantum (pane)
  (:documentation "Returns the number of pixels respresenting a 'line', used
to computed distance to scroll in response to mouse wheel events."))

(deftype scroll-bar-spec () '(member t :both :vertical :horizontal nil))

;;;; 29.3.4 The Layout Protocol


(defgeneric compose-space (node &key width height)
  (:documentation "~
During the space composition pass nodes are asked about their space requirements
with optional proposed defaults WIDTH and HEIGHT. The returned space requirement
is used by the caller to synthesize space requirements and layout of its scions.

Returns a SPACE-REQUIREMENT object."))

(defgeneric allocate-space (node width height)
  (:documentation "~
During the space allocation pass scions are informed about their new size. The
caller ensures that each node has sufficient size to accomodate dimensions."))

(defgeneric change-space-requirements (pane &rest space-req-keys
                                            &key resize-frame
                                                 width min-width min-height
                                                 height max-width max-height))

(defgeneric note-space-requirements-changed (sheet pane))

(pledge :macro changing-space-requirements (&key resize-frame layout) &body body)

;;;; 29.4.4 CLIM Stream Pane Functions

(defgeneric window-clear (window))
(defgeneric window-refresh (window))
(defgeneric window-viewport (window))
(defgeneric window-erase-viewport (window))
;;; The setter is McCLIM extension.
(define-accessor window-viewport-position (x y window))

;;; 29.4.5 Creating a Standalone CLIM Window
(declfun open-window-stream
  (&key port left top right bottom width height foreground background text-style
        (vertical-spacing 2) end-of-line-action end-of-page-action output-record
        (draw t) (record t) (initial-cursor-visibility :off) text-margin save-under
        input-buffer (scroll-bars :vertical) borders label))


;;; 30.3 Basic gadgets

(define-accessor gadget-id (value gadget))
(define-accessor gadget-client (value gadget))
(defgeneric gadget-armed-callback (gadget))
(defgeneric gadget-disarmed-callback (gadget))
(defgeneric armed-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric disarmed-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric gadget-active-p (gadget))
(defgeneric gadget-armed-p (gadget))
(defgeneric activate-gadget (gadget))
(defgeneric deactivate-gadget (gadget))
(defgeneric note-gadget-activated (client gadget))
(defgeneric note-gadget-deactivated (client gadget))
(defgeneric gadget-value (gadget))
(defgeneric (setf gadget-value) (value value-gadget &key invoke-callback))
(defgeneric gadget-value-changed-callback (gadget))
(defgeneric value-changed-callback (gadget client gadget-id value)
  (:argument-precedence-order client gadget-id gadget value))
(defgeneric gadget-activate-callback (gadget))
(defgeneric activate-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric gadget-orientation (gadget))
(define-accessor gadget-label (value gadget))
(define-accessor gadget-label-align-x (value gadget))
(define-accessor gadget-label-align-y (value gadget))
(define-accessor gadget-min-value (value gadget))
(define-accessor gadget-max-value (value gadget))
(defgeneric gadget-range (gadget)
  (:documentation
   "Returns the difference of the maximum and minimum value of RANGE-GADGET."))
(defgeneric gadget-range* (gadget)
  (:documentation
   "Returns the minimum and maximum value of RANGE-GADGET as two values."))

;;; 30.4 Abstract gadgets

(defgeneric push-button-show-as-default (gadget))
(defgeneric toggle-button-indicator-type (gadget))
(defgeneric scroll-bar-drag-callback (gadget))
(defgeneric scroll-bar-scroll-to-top-callback (gadget))
(defgeneric scroll-bar-scroll-to-bottom-callback (gadget))
(defgeneric scroll-bar-scroll-up-line-callback (gadget))
(defgeneric scroll-bar-scroll-up-page-callback (gadget))
(defgeneric scroll-bar-scroll-down-line-callback (gadget))
(defgeneric scroll-bar-scroll-down-page-callback (gadget))
(defgeneric scroll-to-top-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric scroll-to-bottom-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric scroll-up-line-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric scroll-up-page-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric scroll-down-line-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric scroll-down-page-callback (gadget client gadget-id)
  (:argument-precedence-order client gadget-id gadget))
(defgeneric gadget-show-value-p (gadget))
(defgeneric slider-drag-callback (slider))
(defgeneric drag-callback (gadget client gadget-id value)
  (:argument-precedence-order client gadget-id gadget value))
(define-accessor radio-box-current-selection (value gadget))
(defgeneric radio-box-selections (gadget))
(define-accessor check-box-current-selection (value gadget))
(defgeneric check-box-selections (gadget))



;;; E.0 Drawing backend protocols (generalization of the postscript backend)
(declmacro with-output-to-drawing-stream (stream-var backend destination &rest args))
(defgeneric invoke-with-output-to-drawing-stream (continuation backend destination &key &allow-other-keys)
  (:argument-precedence-order backend destination continuation))

;;; E.1

(declmacro with-output-to-postscript-stream
  ((stream-var stream
    &key device-type multi-page scale-to-fit orientation header-comments)
  &body body))

(defgeneric new-page (stream))



;; Used in stream-input.lisp, defined in frames.lisp
(defgeneric frame-event-queue (frame))

;;; Used in presentations.lisp, defined in commands.lisp

(defgeneric presentation-translators (command-table))

;;; ----------------------------------------------------------------------

(defgeneric output-record-basline (record)
  (:documentation
   "Returns two values: the baseline of an output record and a boolean
indicating if this baseline is definitive. McCLIM addition."))



#||

Further undeclared functions

  FRAME-EVENT-QUEUE FRAME-EXIT PANE-FRAME ALLOCATE-SPACE COMPOSE-SPACE
  FIND-INNERMOST-APPLICABLE-PRESENTATION HIGHLIGHT-PRESENTATION-1
  PANE-DISPLAY-FUNCTION PANE-DISPLAY-TIME PANE-NAME PRESENTATION-OBJECT
  PRESENTATION-TYPE SPACE-REQUIREMENT-HEIGHT SPACE-REQUIREMENT-WIDTH
  THROW-HIGHLIGHTED-PRESENTATION WINDOW-CLEAR

  (SETF GADGET-MAX-VALUE) (SETF GADGET-MIN-VALUE) (SETF SCROLL-BAR-THUMB-SIZE)
  SLOT-ACCESSOR-NAME::|CLIM-INTERNALS CLIENT slot READER|
  FORMAT-CHILDREN GADGET-VALUE MAKE-MENU-BAR TABLE-PANE-NUMBER MEDIUM
  WITH-GRAPHICS-STATE TEXT-STYLE-CHARACTER-WIDTH SCROLL-EXTENT

||#
