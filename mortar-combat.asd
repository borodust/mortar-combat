(cl:defpackage :mortar-combat.def
  (:use :cl :asdf))
(cl:in-package :mortar-combat.def)


(defsystem mortar-combat/common
  :description "Common code between Mortar Combat client and server"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (cl-bodge/utils cl-bodge/network)
  :serial t
  :pathname "common/"
  :components ((:file "packages")
               (:file "messages")))


(defsystem mortar-combat
  :description "Multiplayer first-person shooter with mortars"
  :version "0.0.1"
  :author "Pavel Korolev"
  :mailto "dev@borodust.org"
  :license "GPLv3"
  :depends-on (log4cl uiop cl-muth bodge-blobs cl-bodge mortar-combat/common)
  :serial t
  :pathname "client/src/"
  :components ((:file "packages")
               (:file "utils")
               (:file "events")
               (:file "player")
               (:file "proxy")
               (:file "arena")
               (:file "camera")
               (:file "room")
               (:file "mortar")
               (:file "dude")
               (:file "ball")
               (:file "shaders/dude")
               (:file "shaders/passthru")
               (:file "messages")
               (:file "connector")
               (:file "game-server")
               (:file "game-client")
               (:file "ui")
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
  :depends-on (log4cl cl-muth flexi-streams mortar-combat/common
                      cl-bodge/engine cl-bodge/utils cl-bodge/network
                      ironclad uuid)
  :serial t
  :pathname "proxy/"
  :components ((:file "packages")
               (:file "utils")
               (:file "arena")
               (:file "peer")
               (:file "proxy")
               (:file "messages")))
