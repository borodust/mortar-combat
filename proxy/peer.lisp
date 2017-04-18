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
    (with-hash-entries ((info connection)
                        (peer-by-name name))
        peer-table
      (when info
        (error "Peer was already registered for provided connection ~A" info))
      (unless peer-by-name
        (let* ((id (loop for id = (make-random-uuid)
                      while (gethash id peer-by-id)
                      finally (return id)))
               (peer (make-instance 'peer
                                    :id id
                                    :name name
                                    :info-connection connection)))
          (setf info peer
                name peer
                (gethash id peer-by-id) peer)
          peer)))))


(defun find-peer-by-property (registry value)
  (with-slots (peer-table) registry
    (gethash value peer-table)))


(defun find-peer-by-id (registry id)
  (with-slots (peer-by-id) registry
    (gethash id peer-by-id)))


(defun update-peer-proxy-connection (registry peer proxy-connection)
  (with-slots (peer-table) registry
    (with-slots ((peer-proxy proxy-connection)) peer
      (remhash (proxy-connection-of peer) peer-table)
      (setf peer-proxy proxy-connection
            (gethash proxy-connection peer-table) peer))))
