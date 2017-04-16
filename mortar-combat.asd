(cl:defpackage :mortar-combat.def
  (:use :cl :asdf))
(cl:in-package :mortar-combat.def)


(defsystem mortar-combat
  :description "Multiplayer first-person shooter with mortars"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (log4cl uiop cl-muth bodge-blobs cl-bodge)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "utils")
               (:file "camera")
               (:file "ball")
               (:file "mortar")
               (:file "dude")
               (:file "shaders/dude")
               (:file "shaders/passthru")
               (:file "main")))


(defsystem mortar-combat/distrib
  :description "Mortar Combat distribution"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (bodge-blobs mortar-combat cl-bodge/distribution)
  :serial t
  :components ((:file "mortar-combat.dist")))
