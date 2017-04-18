(in-package :mortar-combat.def)


(defpackage :mortar-combat.proxy
  (:use :cl :ge.ng :ge.util :mortar-combat.common)
  (:export start
           stop))
