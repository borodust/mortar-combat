(in-package :mortar-combat.proxy)


(defclass peer ()
  ((id :initarg :id :reader id-of)
   (name :initarg :name :reader name-of)
   (info-connection :initarg :info-connection :reader info-connection-of)
   (proxy-connection :initform nil :reader proxy-connection-of)))


(defclass peer-registry ()
  ((peer-table :initform (make-hash-table :test #'equal))
   (peer-by-id :initform (make-hash-table :test #'equal))))


(defun register-peer (registry connection name)
  (with-slots (peer-table peer-by-id) registry
    (with-hash-entries ((peer-by-info connection)
                        (peer-by-name name))
        peer-table
      (when peer-by-info
        (error "Peer was already registered for provided connection ~A" peer-by-info))
      (unless peer-by-name
        (let* ((id (loop for id = (make-random-uuid)
                      while (gethash id peer-by-id)
                      finally (return id)))
               (peer (make-instance 'peer
                                    :id id
                                    :name name
                                    :info-connection connection)))
          (setf peer-by-info peer
                peer-by-name peer
                (gethash id peer-by-id) peer)
          peer)))))


(defun find-peer-by-property (registry value)
  (with-slots (peer-table) registry
    (gethash value peer-table)))


(defun find-peer-by-id (registry id)
  (with-slots (peer-by-id) registry
    (gethash id peer-by-id)))


(defun remove-peer (registry peer)
  (with-slots (peer-table peer-by-id) registry
    (remhash (info-connection-of peer) peer-table)
    (remhash (proxy-connection-of peer) peer-table)
    (remhash (name-of peer) peer-table)
    (remhash (id-of peer) peer-by-id)))


(defun update-peer-proxy-connection (registry peer proxy-connection)
  (with-slots (peer-table) registry
    (remhash (proxy-connection-of peer) peer-table)
    (setf (gethash proxy-connection peer-table) peer))
  (with-slots ((peer-proxy proxy-connection) proxy-stream) peer
    (setf peer-proxy proxy-connection)))
