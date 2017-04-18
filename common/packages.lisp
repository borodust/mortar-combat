(in-package :mortar-combat.def)


(defpackage :mortar-combat.common
  (:use :cl :ge.util)
  (:export +ok-reply+
           process-command
           encode-message
           decode-message))
