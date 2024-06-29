(in-package #:asdf-user)

(defsystem "clim-postscript-font"
  :depends-on ("clim")
  :serial t
  :components ((:module "font"
                :components ((:file "package")
                             (:file "encoding")
                             (:file "font")
                             (:file "afm")
                             (:file "standard-metrics")))))
