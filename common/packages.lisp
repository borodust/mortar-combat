(in-package :mortar-combat.def)


(defpackage :mortar-combat.common
  (:use :cl :ge.ng :ge.util)
  (:export process-command
           encode-message
           decode-message))
