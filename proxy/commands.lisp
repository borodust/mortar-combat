(in-package :mortar-combat.proxy)


(defmethod process-command ((command (eql :version)) message)
  (list :command :version
        :version +server-version+))


(defmethod process-command ((command (eql :identify)) message)
  (let* ((reg (peer-registry-of *system*))
         (peer (or *peer* (register-peer reg *connection* (format nil "~A" (getf message :name))))))
    (list :command :identified
          :name (name-of peer)
          :id (id-of peer))))


(defmethod process-command ((command (eql :get-arena-list)) message))


(defmethod process-command ((command (eql :create-arena)) message))


(defmethod process-command ((command (eql :join-arena)) message))
