(in-package :mortar-combat.proxy)


(defmethod process-command ((command (eql :version)) message)
  (list :command :version
        :version +server-version+))
