(in-package :mortar-combat.proxy)


(defclass peer ()
  ((id :initarg :id :reader id-of)
   (name :initarg :name :reader name-of)
   (channel :initform nil :reader channel-of)))


(defclass peer-registry ()
  ((peer-table :initform (make-hash-table :test #'equal))
   (peer-by-id :initform (make-hash-table :test #'equal))))


(defun register-peer (registry channel name)
  (with-slots (peer-table peer-by-id) registry
    (with-hash-entries ((peer-by-info channel)
                        (peer-by-name name)
                        (peer-by-stream (stream-of channel)))
        peer-table
      (when peer-by-info
        (error "Peer was already registered for provided channel ~A" peer-by-info))
      (unless peer-by-name
        (let* ((id (loop for id = (make-random-uuid)
                      while (gethash id peer-by-id)
                      finally (return id)))
               (peer (make-instance 'peer
                                    :id id
                                    :name name
                                    :channel channel)))
          (setf peer-by-info peer
                peer-by-name peer
                peer-by-stream peer
                (gethash id peer-by-id) peer)
          peer)))))


(defun %find-peer-by-property (registry value)
  (with-slots (peer-table) registry
    (gethash value peer-table)))


(defun find-peer-by-id (registry id)
  (with-slots (peer-by-id) registry
    (gethash id peer-by-id)))


(defun find-peer-by-name (registry name)
  (%find-peer-by-property registry name))


(defun find-peer-by-stream (registry stream)
  (%find-peer-by-property registry stream))


(defun remove-peer (registry peer)
  (with-slots (peer-table peer-by-id) registry
    (remhash (channel-of peer) peer-table)
    (remhash (name-of peer) peer-table)
    (remhash (id-of peer) peer-by-id)))
