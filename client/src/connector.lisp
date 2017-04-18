(in-package :mortar-combat)


(define-constant +supported-server-version+ 1)

(declaim (special *message*))

(defstruct (server-identity
             (:constructor make-server-identity (id name)))
  (id nil :read-only t)
  (name nil :read-only t))


(defun connection-stream-of (connector)
  (usocket:socket-stream (connection-of connector)))


(defclass connector (lockable disposable dispatcher)
  ((enabled-p :initform t)
   (connection :initarg :connection :reader connection-of)
   (message-counter :initform 0)
   (message-table :initform (make-hash-table :test 'eql))))


(defmethod initialize-instance :after ((this connector) &key)
  (with-slots (connection message-table enabled-p) this
    (in-new-thread "connector-thread"
      (loop while enabled-p
         do (log-errors
              (usocket:wait-for-input connection)
              (let ((message (decode-message (connection-stream-of this))))
                (if-let ((reply-id (getf message :reply-for)))
                  (with-instance-lock-held (this)
                    (if-let ((handler (gethash reply-id message-table)))
                      (progn
                        (remhash reply-id message-table)
                        (funcall handler message))
                      (log:error "Handler not found for message with id ~A" reply-id)))
                  (encode-message (process-command (getf message :command) message)
                                  (connection-stream-of this)))))
         finally (usocket:socket-close connection)))))


(defun connect-to-server (host port)
  (make-instance 'connector
                 :connection (usocket:socket-connect host port
                                                     :element-type '(unsigned-byte 8)
                                                     :timeout 30)))


(defun disconnect-from-server (connector)
  (with-slots (enabled-p) connector
    (setf enabled-p nil)))


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


(defmacro with-response (command-name (&rest properties) response &body body)
  `(destructuring-bind (&key ,@properties &allow-other-keys) ,response
     (check-response ,response ,command-name)
     ,@body))


(defmethod dispatch ((this connector) (task function) invariant &rest keys
                     &key &allow-other-keys)
  (with-slots (message-table message-counter) this
    (with-instance-lock-held (this)
      (let ((next-id (incf message-counter)))
        (flet ((response-callback (message)
                 (let ((*message* message))
                   (funcall task))))
          (setf (gethash next-id message-table) #'response-callback)
          (apply #'send-command this :message-id next-id keys))))))


(defun server-version (connector)
  (-> (connector :command :version) ()
    (with-response :version (version) *message*
      version)))


(defun identify (connector name)
  (-> (connector :command :identify :name name) ()
    (with-response :identified (id name) *message*
      (make-server-identity id name))))


(defun create-arena (connector name)
  (-> (connector :command :create-arena :name name) ()
    (with-response :ok () *message*)))


(defun join-arena (connector name)
  (-> (connector :command :join-arena :name name) ()
    (with-response :ok () *message*)))


(defun get-arena-list (connector)
  (-> (connector :command :get-arena-list) ()
    (with-response :arena-list (list) *message*
      list)))


(defun register-game-stream (connector peer-id)
  (-> (connector :command :register-game-stream :peer-id peer-id) ()
    (with-response :ok () *message*)))


(defun ping-peer (connector)
  (-> (connector :command :ping) ()
    (with-response :ok () *message*)))


(defmethod process-command ((command (eql :ping)) message)
  +ok-reply+)
