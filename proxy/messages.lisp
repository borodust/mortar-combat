(in-package :mortar-combat.proxy)


(declaim (special *channel*
                  *message*))


(defun disconnect-peer (proxy stream)
  (let* ((peer-reg (peer-registry-of proxy))
         (arena-reg (arena-registry-of proxy)))
    (when-let ((peer (find-peer-by-stream peer-reg stream)))
      (when (eq stream (stream-of(channel-of peer)))
        (remove-arena-by-server arena-reg peer)
        (remove-peer peer-reg peer)
        (log:debug "Peer '~A' disconnected" (name-of peer))))))


(define-destructor server-channel (system (stream stream-of))
  (disconnect-peer system stream))


(defmethod receive-message :around ((this server-channel) message)
  (with-slots (system) this
    (let* ((*peer* (find-peer-by-stream (peer-registry-of this) (stream-of this)))
           (*system* system)
           (*channel* this)
           (*message* message))
      (log-errors
        (call-next-method)))))


(defun reply-with (class &rest initargs &key &allow-other-keys)
  (send-message *channel* (apply #'make-reply-for *message* class initargs)))


(defun ack ()
  (acknowledge *message* *channel*))


(defmacro when-peer-identified (&body body)
  `(if *peer*
       (progn ,@body)
       (reply-with 'unauthorized-access-error)))


(defmethod receive-message ((this server-channel) (message version-request))
  (reply-with 'version-response :version +server-version+))


(defmethod receive-message ((this server-channel) (message identification-request))
  (let* ((name (getf message :name))
         (reg (peer-registry-of *system*))
         (peer (or *peer* (register-peer reg *system* (format nil "~A" name)))))
    (if peer
        (progn
          (log:debug "Peer '~A' identified" name)
          (reply-with 'identification-response
                      :peer-id  (id-of peer)
                      :name (name-of peer)))
        (reply-with 'unavailable-name-error
                    :text (format nil "Name '~A' taken" name)))))


(defmethod receive-message ((this server-channel) (message arena-list-request))
  (reply-with 'arena-list-response
              :arena-list (list-arena-names (arena-registry-of *system*))))


(defmethod receive-message ((this server-channel) (message arena-creation-request))
  (when-peer-identified
    (with-message-fields (name) message
      (let ((reg (arena-registry-of *system*)))
        (if-let ((assigned-arena (find-arena-by-peer reg *peer*)))
          (reply-with 'already-in-arena-error
                      :text (format nil "Already assigned to '~A' arena" (name-of assigned-arena)))
          (if-let ((arena (register-arena reg (format nil "~A" name) *peer*)))
            (ack)
            (reply-with 'arena-exists-error
                        :text (format nil "Arena with name '~A' exists" name))))))))


(defmethod receive-message ((this server-channel) (message arena-joining-request))
  (when-peer-identified
    (with-message-fields (name) message
      (let* ((reg (arena-registry-of *system*))
             (client-arena (find-arena-by-peer reg *peer*))
             (requested-arena (find-arena-by-name reg name)))
        (cond
          ((and client-arena (or (eq *peer* (server-of client-arena))
                                 (not (eq client-arena requested-arena))))
           (reply-with 'already-in-arena-error
                       :text (format nil "Already joined '~A' arena" (name-of requested-arena))))
          ((null requested-arena)
           (reply-with 'arena-not-found-error
                       :text (format nil "Arena with name '~A' not found" name)))
          ;; fixme: add client limit
          (t (add-arena-client reg requested-arena *peer*)
             (ack)))))))


(defmethod receive-message ((this server-channel) (message arena-exiting-request))
  (when-peer-identified
    (remove-peer-from-arena (arena-registry-of *system*) *peer*)
    (ack)))


(defmethod receive-message ((this server-channel) (message relay-request))
  (with-message-fields (peer data) message
    (let ((reg (peer-registry-of *system*)))
      (if-let ((target-peer (find-peer-by-name reg peer)))
        (send-message (channel-of target-peer) message)
        (reply-with 'peer-not-found-error
                    :text (format nil "Peer with name ~A not found" peer))))))
