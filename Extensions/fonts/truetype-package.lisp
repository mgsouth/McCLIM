(defpackage #:mcclim-truetype
  (:use #:climi #:clim #:clim-lisp #:climb #:clime)
  (:import-from :alexandria
                #:ensure-gethash
                #:when-let
                #:if-let
                #:minf
                #:maxf
                #:assoc-value
                #:read-file-into-byte-vector
                #:maphash-values)
  ;; Fontconfig
  (:export #:*truetype-font-path*
           #:*families/faces*
           #:*zpb-font-lock*
           #:truetype-device-font-name
           #:fontconfig-font-name
           #:make-truetype-device-font-name
           #:make-fontconfig-font-name
           #:find-fontconfig-font
           #:invoke-with-truetype-path-restart)
  ;; Implementation classes
  (:export #:truetype-font
           #:truetype-font-family
           #:truetype-face
           #:cached-truetype-font)
  ;; Atlas implementgation
  (:export #:font-glyph-info
           #:font-ascent
           #:font-descent
           #:font-generate-glyph
           #:glyph-pixarray
           #:font-string-glyph-codes)
  ;; Glyph metrics
  (:export #:glyph-info
           #:glyph-info-id
           #:glyph-info-pixarray
           ;; bearings
           #:glyph-info-left
           #:glyph-info-top
           #:glyph-info-right
           #:glyph-info-bottom
           ;;
           #:glyph-info-width
           #:glyph-info-height
           ;; horizontal/vertical advance width/height
           #:glyph-info-advance-hx
           #:glyph-info-advance-hy
           #:glyph-info-advance-vx
           #:glyph-info-advance-vy
           ;; effective origin and advance width/height
           #:glyph-info-origin-x
           #:glyph-info-origin-y
           #:glyph-info-advance-dx
           #:glyph-info-advance-dy)
  ;; Consumer exports
  (:export #:ttf-port-mixin
           #:ttf-medium-mixin))
