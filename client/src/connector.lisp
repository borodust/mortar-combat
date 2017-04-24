(in-package :mortar-combat)


(define-constant +supported-server-version+ 1)

(declaim (special *message*
                  *connector*))

(defstruct (server-identity
             (:constructor make-server-identity (id name)))
  (id nil :read-only t)
  (name nil :read-only t))


(defun connection-stream-of (connector)
  (usocket:socket-stream (connection-of connector)))


(defclass connector (lockable disposable dispatcher)
  ((enabled-p :initform t)
   (connection :initform nil :reader connection-of)
   (message-counter :initform 0)
   (message-table :initform (make-hash-table :test 'eql))))


(defmethod initialize-instance :after ((this connector) &key host port)
  (with-slots (connection message-table enabled-p) this
    (setf connection  (usocket:socket-connect host port
                                              :element-type '(unsigned-byte 8)
                                              :timeout 30))
    (in-new-thread "connector-thread"
      (loop while enabled-p
         do (log-errors
              (handler-case
                  (progn
                    (usocket:wait-for-input connection)
                    (when enabled-p
                      (let* ((stream (connection-stream-of this))
                             (message (decode-message stream))
                             (*connector* this))
                        (if-let ((reply-id (getf message :reply-for)))
                          (with-instance-lock-held (this)
                            (if-let ((handler (gethash reply-id message-table)))
                              (progn
                                (remhash reply-id message-table)
                                (funcall handler message))
                              (log:error "Handler not found for message with id ~A" reply-id)))
                          (when-let ((reply (process-command (getf message :command) message)))
                            (encode-message reply stream)
                            (force-output stream))))))
                (end-of-file ()
                  (setf enabled-p nil)
                  (log:debug "Disconnected from server"))))
         finally (usocket:socket-close connection)))))


(defun disconnect-from-server (connector)
  (with-slots (enabled-p connection) connector
    (when enabled-p
      (usocket:socket-close connection)
      (setf enabled-p nil))))


(defun check-response (message expected-command)
  (let ((command (getf message :command)))
    (when (eq command :error)
      (error "Server error of type ~A: ~A" (getf message :type) (getf message :text)))
    (unless (eq command expected-command)
      (error "Unexpected command received from server: wanted ~A, but ~A received"
             expected-command command))))


(defun send-command (connector &rest properties &key &allow-other-keys)
  (let ((stream (connection-stream-of connector)))
    (encode-message properties stream)
    (finish-output stream)))


(defmacro with-response ((&rest properties) command-name &body body)
  `(with-message (,@properties) *message*
     (check-response *message* ,command-name)
     ,@body))


(defmethod dispatch ((this connector) (task function) invariant &rest keys
                     &key &allow-other-keys)
  (with-slots (enabled-p message-table message-counter) this
    (with-instance-lock-held (this)
      (flet ((response-callback (message)
               (let ((*message* message))
                 (funcall task))))
        (if enabled-p
            (let ((next-id (incf message-counter)))
              (unless (getf keys :no-reply)
                (setf (gethash next-id message-table) #'response-callback))
              (apply #'send-command this :message-id next-id keys))
            (response-callback (list :command :error
                                     :type :disconnected
                                     :text "Disconnected from server")))))))


(defun server-version (connector)
  (-> (connector :command :version) ()
    (with-response (version) :version
      version)))


(defun identify (connector name)
  (-> (connector :command :identify :name name) ()
    (with-response (id name) :identified
      (make-server-identity id name))))


(defun create-arena (connector name)
  (-> (connector :command :create-arena :name name) ()
    (with-response () :ok)))


(defun join-arena (connector name)
  (-> (connector :command :join-arena :name name) ()
    (with-response () :ok)))


(defun leave-arena (connector)
  (-> (connector :command :exit-arena) ()
    (with-response () :ok)))


(defun get-arena-list (connector)
  (-> (connector :command :get-arena-list) ()
    (with-response (list) :arena-list
      list)))


(defun register-game-stream (connector peer-id)
  (-> (connector :command :register-game-stream :peer-id peer-id) ()
    (with-response () :ok)))


(defun ping-peer (connector)
  (-> (connector :command :ping) ()
    (with-response () :ok)))


(defmethod process-command ((command (eql :ping)) message)
  +ok-reply+)
