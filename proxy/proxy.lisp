(in-package :mortar-combat.proxy)

(declaim (special *peer*))
(declaim (special *connection*))

(defvar *main-latch* (mt:make-latch))


(define-constant +server-version+ 1)
(define-constant +routing-buffer-size+ (* 64 1024))
(define-constant +client-socket-timeout+ (* 5 60))


(defclass mortar-combat-proxy (enableable generic-system)
  ((proxy-server :initform nil)
   (peer-registry :initform (make-instance 'peer-registry) :reader peer-registry-of)
   (arena-registry :initform (make-instance 'arena-registry) :reader arena-registry-of)
   (routing-buffer :initform (make-array +routing-buffer-size+
                                         :element-type '(unsigned-byte 8)))
   (info-server :initform nil)))


(defun reply-to (message)
  (process-command (getf message :command) message))


(defun process-request (stream)
  (let ((message (decode-message stream)))
    (when (listp message)
      (encode-message (reply-to message) stream)
      (force-output stream))))


(defun pour-stream (source-stream destination-stream)
  (with-slots (routing-buffer) *system*
    (when (and source-stream destination-stream)
      (loop with buf-len = (length routing-buffer)
         for bytes-read = (read-sequence routing-buffer source-stream)
         do (write-sequence routing-buffer destination-stream :end bytes-read)
         while (= bytes-read buf-len))
      (force-output destination-stream))))


(defun route-stream (stream)
  (when-let ((arena (find-arena-by-peer (arena-registry-of *system*) *peer*)))
    (let ((arena-server (server-of arena)))
      (flet ((wrap-into-stream (peer)
               ;; fixme: find a way to avoid stream instantiating
               (when-let ((socket (proxy-connection-of peer)))
                 (make-instance 'as:async-output-stream :socket socket))))
        (if (eq arena-server *peer*)
            (loop for client in (clients-of arena)
               do (pour-stream stream (wrap-into-stream client)))
            (pour-stream stream (wrap-into-stream arena-server)))))))


(defgeneric process-condition (condition)
  (:method (condition)
    (log:error "Unhandled event ~A: ~A" (type-of condition) condition)))


(defun disconnect-peer (connection)
  (let* ((proxy (engine-system 'mortar-combat-proxy))
         (peer-reg (peer-registry-of proxy))
         (arena-reg (arena-registry-of proxy)))
    (when-let ((peer (find-peer-by-property peer-reg connection)))
      (remove-arena-by-server arena-reg peer)
      (remove-peer peer-reg peer)
      (log:debug "Peer '~A' disconnected" (name-of peer)))))


(defmethod process-condition ((condi as:socket-eof))
  (disconnect-peer (as:socket condi)))


(defmethod process-condition ((condi as:socket-reset))
  (log:warn "Peer disconnected unexpectedly"))


(defmethod initialize-system :after ((this mortar-combat-proxy))
  (with-slots (proxy-server info-server peer-registry) this
    (labels ((on-accept (socket)
               (when-let ((connection-timeout (property :connection-timeout)))
                 (as:set-socket-timeouts socket connection-timeout nil)))
             (process-input (socket stream)
               (let* ((*system* this)
                      (*connection* socket)
                      (*peer* (find-peer-by-property (peer-registry-of this) *connection*)))
                 (log-errors
                   (if (and *peer* (eq (proxy-connection-of *peer*) *connection*))
                       (route-stream stream)
                       (process-request stream)))))
             (make-server (host port)
               (as:tcp-server host port #'process-input
                              :event-cb #'process-condition
                              :connect-cb #'on-accept
                              :stream t)))
      (in-new-thread "connector-thread"
        (as:with-event-loop ()
          (setf proxy-server (make-server "127.0.0.1" 8222)
                info-server (make-server "127.0.0.1" 8778)))))))


(defmethod make-system-context ((this mortar-combat-proxy))
  (make-instance 'mortar-combat-context))


(defmethod discard-system :before ((this mortar-combat-proxy))
  (with-slots (proxy-server info-server) this
    (as:close-tcp-server proxy-server)
    (as:close-tcp-server info-server)))


(defun start ()
  (startup '(:engine (:systems (mortar-combat-proxy)
                      :log-level :debug))))


(defun stop ()
  (shutdown))


(defun main (args)
  (declare (ignore args))
  (start)
  (mt:wait-for-latch *main-latch*))
