(in-package :mortar-combat.def)


(defpackage :mortar-combat.proxy
  (:use :cl :ge.ng :ge.net :ge.util :mortar-combat.common)
  (:export start
           stop))
