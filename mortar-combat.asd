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
  :pathname "client/src/"
  :components ((:file "packages")
               (:file "utils")
               (:file "camera")
               (:file "room")
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
  :pathname "client/"
  :components ((:file "mortar-combat.dist")))


(defsystem mortar-combat/proxy
  :description "Proxy server for Mortar Combat"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (log4cl cl-muth cl-conspack usocket flexi-streams
                      cl-bodge/engine cl-bodge/utils ironclad uuid)
  :serial t
  :pathname "proxy/"
  :components ((:file "packages")
               (:file "utils")
               (:file "arena")
               (:file "peer")
               (:file "proxy")
               (:file "commands")))
