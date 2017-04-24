(in-package :mortar-combat.proxy)


(defmacro when-peer-identified (&body body)
  `(if *peer*
       (progn ,@body)
       (list :command :error
             :type :unidentified-peer
             :text "Unauthorized access. Identify first.")))


(defmethod process-command ((command (eql :version)) message)
  (list :command :version
        :version +server-version+))


(defmethod process-command ((command (eql :identify)) message)
  (let* ((name (getf message :name))
         (reg (peer-registry-of *system*))
         (peer (or *peer* (register-peer reg *connection* (format nil "~A" name)))))
    (if peer
        (progn
          (log:debug "Peer '~A' identified" name)
          (list :command :identified
                :name (name-of peer)
                :id (id-of peer)))
        (list :command :error
              :type :name-unavailable
              :text (format nil "Name '~A' taken" name)))))


(defmethod process-command ((command (eql :get-arena-list)) message)
  (list :command :arena-list
        :list (list-arena-names (arena-registry-of *system*))))


(defmethod process-command ((command (eql :create-arena)) message)
  (when-peer-identified
    (with-message (name) message
      (let ((reg (arena-registry-of *system*)))
      (if-let ((assigned-arena (find-arena-by-peer reg *peer*)))
        (list :command :error
              :type :already-in-arena
              :text (format nil "Already assigned to '~A' arena" (name-of assigned-arena)))
        (if-let ((arena (register-arena reg (format nil "~A" name) *peer*)))
          +ok-reply+
          (list :command :error
                :type :arena-exist
                :text (format nil "Arena with name '~A' exists" name))))))))


(defmethod process-command ((command (eql :join-arena)) message)
  (when-peer-identified
    (with-message (name) message
      (let* ((reg (arena-registry-of *system*))
             (client-arena (find-arena-by-peer reg *peer*))
             (requested-arena (find-arena-by-name reg name)))
        (cond
           ((and client-arena (or (eq *peer* (server-of client-arena))
                                  (not (eq client-arena requested-arena))))
           (list :command :error
                 :type :already-in-arena
                 :text (format nil "Already joined '~A' arena" (name-of requested-arena))))
          ((null requested-arena)
           (list :command :error
                 :type :arena-not-found
                 :text (format nil "Arena with name '~A' not found" name)))
          ;; fixme: add client limit
          (t (prog1 +ok-reply+
               (add-arena-client reg requested-arena *peer*))))))))


(defmethod process-command ((command (eql :exit-arena)) message)
  (when-peer-identified
   (remove-peer-from-arena (arena-registry-of *system*) *peer*)
   +ok-reply+))


(defmethod process-command ((command (eql :register-game-stream)) message)
  (with-message (peer-id) message
    (let ((reg (peer-registry-of *system*)))
      (if-let ((peer (find-peer-by-id reg peer-id)))
        (prog1 +ok-reply+
          (update-peer-proxy-connection reg peer *connection*))
        (list :command :error
              :type :peer-not-found
              :text (format nil "Peer with id ~A not found" peer-id))))))
