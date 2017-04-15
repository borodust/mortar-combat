(cl:defpackage :mortar-combat.def
  (:use :cl :asdf))
(cl:in-package :mortar-combat.def)


(defsystem mortar-combat
  :description "Multiplayer first-person shooter with mortars"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (log4cl bodge-blobs cl-bodge)
  :serial t
  :components ((:file "packages")
               (:file "main")))
