(in-package #:asdf-user)

(defsystem "mcclim-render"
  :description "Support for raster images McCLIM."
  :depends-on ("alexandria"
               "cl-vectors"
               "clim"
               "mcclim-fonts/truetype")
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "utilities")
               (:file "image")
               (:file "vectors")
               (:module "render"
                :serial t
                :components ((:file "prim-arc")))
               (:module "backend"
                :serial t
                :components ((:file "mirror")
                             (:file "medium")
                             (:file "pixmap")
                             (:file "port")))))
