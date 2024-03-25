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
  (:export #:font-ascent
           #:font-descent
           #:font-glyph-info
           #:font-glyph-info*
           #:font-generate-glyph
           #:string-glyph-codes
           #:char-glyph-code
           #:glyph-code-char)
  ;; Glyph metrics
  (:export #:glyph-info
           #:glyph-info-id
           #:glyph-info-pixarray
           ;; bearings
           #:glyph-info-left
           #:glyph-info-top
           ;; glyph dimensions
           #:glyph-info-width
           #:glyph-info-height
           ;; original advances (vy is hacked to line heigh)
           #:glyph-info-advance-hx
           #:glyph-info-advance-vy
           ;; effective origin and advance width/height
           #:glyph-info-origin-x
           #:glyph-info-origin-y
           #:glyph-info-advance-dx
           #:glyph-info-advance-dy)
  ;; Consumer exports
  (:export #:ttf-port-mixin
           #:ttf-medium-mixin))
