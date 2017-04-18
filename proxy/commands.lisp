(in-package :mortar-combat.proxy)


(define-constant +ok-reply+ (list :command :ok)
  :test #'equal)


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
        (list :command :identified
              :name (name-of peer)
              :id (id-of peer))
        (list :command :error
              :type :name-unavailable
              :text (format nil "Name '~A' taken" name)))))


(defmethod process-command ((command (eql :get-arena-list)) message)
  (list :command :arena-list
        :list (list-arena-names (arena-registry-of *system*))))


(defmethod process-command ((command (eql :create-arena)) message)
  (when-peer-identified
    (destructuring-bind (&key name &allow-other-keys) message
      (if-let ((arena (register-arena (arena-registry-of *system*) (format nil "~A" name) *peer*)))
        +ok-reply+
        (list :command :error
              :type :arena-exist
              :text (format nil "Arena with name '~A' exists" name))))))


(defmethod process-command ((command (eql :join-arena)) message)
  (when-peer-identified
    (destructuring-bind (&key name &allow-other-keys) message
      (let ((reg (arena-registry-of *system*)))
        (if-let ((arena (find-arena-by-name reg name)))
          ;; fixme: add client limit
          (prog1 +ok-reply+
            (add-arena-client reg arena *peer*))
          (list :command :error
                :type :arena-not-found
                :text (format nil "Arena with name '~A' not found" name)))))))
