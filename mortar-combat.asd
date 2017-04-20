(cl:defpackage :mortar-combat.def
  (:use :cl :asdf))
(cl:in-package :mortar-combat.def)


(defsystem mortar-combat/common
  :description "Common code between Mortar Combat client and server"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (cl-bodge/utils cl-conspack)
  :serial t
  :pathname "common/"
  :components ((:file "packages")
               (:file "process-command")))


(defsystem mortar-combat
  :description "Multiplayer first-person shooter with mortars"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (log4cl uiop cl-muth bodge-blobs cl-bodge usocket
                      mortar-combat/common)
  :serial t
  :pathname "client/src/"
  :components ((:file "packages")
               (:file "utils")
               (:file "keymap")
               (:file "camera")
               (:file "room")
               (:file "ball")
               (:file "mortar")
               (:file "dude")
               (:file "shaders/dude")
               (:file "shaders/passthru")
               (:file "main")
               (:file "connector")))


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
  :depends-on (log4cl cl-muth cl-async flexi-streams mortar-combat/common
                      cl-bodge/engine cl-bodge/utils ironclad uuid)
  :serial t
  :pathname "proxy/"
  :components ((:file "packages")
               (:file "utils")
               (:file "arena")
               (:file "peer")
               (:file "proxy")
               (:file "commands")))
