(in-package :mortar-combat.proxy)

(declaim (special *peer*))
(declaim (special *connection*))

(defvar *main-latch* (mt:make-latch))


(define-constant +server-version+ 1)
(define-constant +routing-buffer-size+ (* 64 1024))


(defgeneric process-command (command message)
  (:method (command message)
    (list :command :error
          :type :unknown-command
          :text "Unknown command")))


(defmethod process-command :around (command message)
  (append (list :reply-for (getf message :message-id)) (call-next-method)))


(defclass mortar-combat-proxy (enableable generic-system)
  ((proxy-socket :initform nil)
   (peer-registry :initform (make-instance 'peer-registry) :reader peer-registry-of)
   (arenas :initform (make-hash-table :test #'equal) :reader arena-list-of)
   (routing-buffer :initform (make-array +routing-buffer-size+
                                         :element-type '(unsigned-byte 8)))
   (info-socket :initform nil)))


(defun reply-to (message)
  (handler-case
      (process-command (getf message :command) message)
    (serious-condition ()
      '(:command :error
        :type :unhandled-error
        :text "Error during command execution"))))


(defun process-request ()
  ;; fixme: record connection's last communication timestamp
  ;;        to autoclose idle connections
  (let ((stream (usocket:socket-stream *connection*)))
    (when (listen stream)
      ;; fixme: make async: read available chunk, don't wait for more
      (let ((message (conspack:decode-stream stream)))
        (when (listp message)
          (conspack:encode (reply-to message) :stream stream)
          (force-output stream))))))


(defun route-stream ())


(defun process-input ()
  (if (and *peer* (eq (proxy-connection-of *peer*) *connection*))
      (route-stream)
      (process-request)))


(defmethod initialize-system :after ((this mortar-combat-proxy))
  (with-slots (proxy-socket info-socket peer-registry) this
    (setf proxy-socket (usocket:socket-listen #(127 0 0 1) 8222
                                              :element-type '(unsigned-byte 8))
          info-socket (usocket:socket-listen #(127 0 0 1) 8778
                                             :element-type '(unsigned-byte 8)))
    (in-new-thread "socket-listener"
      (let ((sockets (list proxy-socket info-socket))
            (*system* this))
        (flet ((%accept (passive-socket)
                 (push (usocket:socket-accept passive-socket) (cddr sockets))))
          (loop while (enabledp this) do
               (log-errors
                 (loop for rest-connections on (cdr (usocket:wait-for-input sockets))
                    for *connection* = (second rest-connections)
                    when (and *connection* (usocket:socket-state *connection*))
                    do (let ((*peer* (find-peer-by-property (peer-registry-of this) *connection*)))
                         (when (process-input)
                           (pop (cdr rest-connections)))))
                 (cond
                   ((usocket:socket-state info-socket) (%accept info-socket))
                   ((usocket:socket-state proxy-socket) (%accept proxy-socket))))))))))


(defmethod make-system-context ((this mortar-combat-proxy))
  (make-instance 'mortar-combat-context))


(defmethod discard-system :before ((this mortar-combat-proxy))
  (with-slots (proxy-socket info-socket) this
    (usocket:socket-close proxy-socket)
    (usocket:socket-close info-socket)))


(defun start ()
  (startup '(:engine (:systems (mortar-combat-proxy)))))


(defun stop ()
  (shutdown))


(defun main (args)
  (declare (ignore args))
  (start)
  (mt:wait-for-latch *main-latch*))
