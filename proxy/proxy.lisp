(in-package :mortar-combat.proxy)

(declaim (special *peer*))

(defvar *main-latch* (mt:make-latch))

(define-constant +server-version+ 1)


(defclass mortar-combat-proxy (enableable generic-system)
  ((server :initform nil)
   (peer-registry :initform (make-instance 'peer-registry) :reader peer-registry-of)
   (arena-registry :initform (make-instance 'arena-registry) :reader arena-registry-of))
  (:default-initargs :depends-on '(network-system)))


(defmethod initialize-system :after ((this mortar-combat-proxy))
  (with-slots (server peer-registry) this
    (run (>> (accept-flow "0.0.0.0" 8778 (lambda (stream)
                                           (make-instance 'server-channel
                                                          :stream stream
                                                          :system this)))
             (instantly (s)
               (setf server s))))))


(defmethod make-system-context ((this mortar-combat-proxy))
  (make-instance 'mortar-combat-context))


(defmethod discard-system :before ((this mortar-combat-proxy))
  (with-slots (server) this
    (dispose server)))


(defun start ()
  (startup '(:engine (:systems (mortar-combat-proxy)
                      :log-level :debug))))


(defun stop ()
  (shutdown))


(defun main (args)
  (declare (ignore args))
  (start)
  (mt:wait-for-latch *main-latch*))
