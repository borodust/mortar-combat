(in-package :mortar-combat.proxy)


(defvar *main-latch* (mt:make-latch))


(define-constant +server-version+ 1)


(defgeneric process-command (command message)
  (:method (command message)
    (list :command :error
          :type :unknown-command
          :text "Unknown command")))


(defclass mortar-combat-proxy (thread-bound-system)
  ((proxy-socket :initform nil)
   (info-socket :initform nil)))


(defun process-request (this connection)
  ;; fixme: record connection's last communication timestamp
  ;;        to autoclose idle connections
  (run (-> (this) ()
         (let ((stream (usocket:socket-stream connection)))
           (when (listen stream)
             ;; fixme: make async
             (let ((message (conspack:decode-stream stream)))
               (when (listp message)
                 (conspack:encode (process-command (getf message :command) message)
                                  :stream stream)
                 (force-output stream))))))))


(defmethod initialize-system :after ((this mortar-combat-proxy))
  (with-slots (proxy-socket info-socket) this
    (setf proxy-socket (usocket:socket-listen #(127 0 0 1) 8222
                                              :element-type '(unsigned-byte 8))
          info-socket (usocket:socket-listen #(127 0 0 1) 8778
                                             :element-type '(unsigned-byte 8)))
    (in-new-thread "socket-listener"
      (let ((sockets (list proxy-socket info-socket)))
        (loop while (enabledp this) do
             (loop for connection in (cddr (usocket:wait-for-input sockets))
                when (usocket:socket-state connection)
                do (process-request this connection))
             (cond
               ((usocket:socket-state info-socket)
                (push (usocket:socket-accept info-socket) (cddr sockets)))))))))


(defmethod make-system-context ((this mortar-combat-proxy))
  (make-instance 'mortar-combat-context))


(defmethod discard-system :before ((this mortar-combat-proxy))
  (with-slots (proxy-socket info-socket) this
    (usocket:socket-close proxy-socket)
    (usocket:socket-close info-socket)))


(defun start ()
  (startup '(:engine (:systems (mortar-combat-proxy)))))


(defun stop ()
  (in-new-thread "exit-thread"
    (shutdown)))


(defun main (args)
  (declare (ignore args))
  (start)
  (mt:wait-for-latch *main-latch*))
