(in-package :mortar-combat)


(declaim (special *connector*))

(defstruct (server-identity
             (:constructor make-server-identity (id name)))
  (id nil :read-only t)
  (name nil :read-only t))


(defclass connector-channel (dispatching-channel) ())


(defclass connector ()
  ((connection :initarg :connection :reader connection-of)))


(definline connector-channel-of (connector)
  (channel-of (connection-of connector)))


(defun connect-to-server (host port channel-class-or-ctor)
  (flet ((make-connector-channel (stream)
           (etypecase channel-class-or-ctor
             (function (funcall channel-class-or-ctor stream))
             ((or standard-class symbol) (make-instance channel-class-or-ctor :stream stream)))))
    (>> (connect-flow host port #'make-connector-channel)
        (instantly (client)
          (make-instance 'connector :connection client)))))


(defun disconnect-from-server (connector)
  (with-slots (connection) connector))


(defun server-version (connector-channel)
  (>> (message-flow connector-channel (make-message 'version-request))
      (instantly (message)
        (version-response-version message))))


(defun identify (connector-channel name)
  (>> (message-flow connector-channel
                    (make-message 'identification-request :name name))
      (instantly (message)
        (make-server-identity (identification-response-peer-id message)
                              (identification-response-name message)))))


(defun create-arena (connector-channel name)
  (message-flow connector-channel
                (make-message 'arena-creation-request :name name)))


(defun join-arena (connector-channel name)
  (message-flow connector-channel
                (make-message 'arena-joining-request :name name)))


(defun leave-arena (connector-channel)
  (message-flow connector-channel
                (make-message 'arena-exiting-request)))


(defun get-arena-list (connector-channel)
  (>> (message-flow connector-channel
                    (make-message 'arena-list-request))
      (instantly (message)
        (arena-list-response-arena-list message))))


(defun ping-peer (connector-channel)
  (message-flow connector-channel
                (make-message 'ping)))


(defmethod receive-message ((channel connector-channel) (message ping))
  (message-flow channel (make-message 'pong)))


(defun relaying-flow (connector-channel message)
  (message-flow connector-channel
                (make-message 'relay-request :data message)))
