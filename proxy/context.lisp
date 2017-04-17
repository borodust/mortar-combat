(in-package :mortar-combat.proxy)


(defclass mortar-combat-context ()
  ((arenas :initform (make-hash-table :test #'equal))))
