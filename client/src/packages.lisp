(cl:in-package :mortar-combat.def)


(defpackage :mortar-combat
  (:use :cl :ge :ge.util :mortar-combat.common)
  (:export start
           stop))
