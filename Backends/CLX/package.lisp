;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

(in-package #:common-lisp-user)

(defpackage #:clim-xcommon
  (:use #:clim #:clim-lisp)
  (:export #:keysym-port-mixin
           #:keysym-to-keysym-name
           #:modifier-mapping
           #:keysym-name-to-keysym
           #:x-event-state-modifiers
           #:x-keysym-to-clim-modifiers
           #:x-event-to-key-name-and-modifiers))

(defpackage #:clim-clx
  (:use #:clim #:clim-lisp #:clim-backend #:clime #:mcclim-truetype)
  (:import-from #:climi
                #:+alt-key+
                ;;
                #:port-text-style-mappings
                #:port-event-process
                #:port-grafts
                #:device-transformation
                ;;
                #:ensure-gethash
                #:clamp
                #:get-environment-variable
                #:map-repeated-sequence
                #:do-sequence
                #:with-transformed-position
                #:with-transformed-positions
                #:with-transformed-distance
                #:with-transformed-angles
                #:with-medium-options
                #:line-style-effective-thickness
                #:line-style-effective-dashes
                ;; designs
                #:standard-flipping-ink
                #:standard-opacity
                #:over-compositum
                #:uniform-compositum
                ;;
                #:pixmap
                #:top-level-sheet-mixin
                #:unmanaged-sheet-mixin
                #:top-level-sheet-pane
                #:unmanaged-top-level-sheet-pane
                #:menu-frame
                ;;
                #:frame-managers        ;used as slot
                #:top-level-sheet       ;used as slot
                #:medium-device-region
                #:draw-image
                #:height                ;this seems bogus
                #:width                 ;dito
                #:coordinate=
                #:get-transformation
                ;;
                #:medium-miter-limit
                #:medium-text-transformation
                ;; classes
                #:window-destroy-event
                #:pointer-grab-enter-event
                #:pointer-grab-leave-event
                #:pointer-ungrab-leave-event
                #:pointer-ungrab-enter-event
                #:device-font-text-style
                ;; drawing utils
                #:round-coordinate
                #:with-round-positions
                #:with-round-coordinates
                ;; utils
                #:maybe-funcall
                #:if-let
                #:when-let
                #:when-let*)
  (:import-from #:climi
                #:event-listen-or-wait
                #:sheet-mirror-geometry
                #:with-standard-rectangle*
                #:assoc-value)
  (:export
   #:clx-port
   #:clx-render-port
   #:clx-port-display
   #:clx-medium
   #:clx-render-medium
   #:initialize-clx
   #:clx-port-screen
   #:clx-graft
   #:clx-port-window
   #:port-find-all-font-families))
