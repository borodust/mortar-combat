(in-package :mortar-combat.proxy)


(defclass arena ()
  ((name :initarg :name :reader name-of)
   (server :initarg :server :reader server-of)
   (clients :initform nil :reader clients-of)))


(defclass arena-registry ()
  ((arena-table :initform (make-hash-table :test 'equal))
   (arena-by-name :initform (make-hash-table :test 'equal))))


(defun register-arena (registry name server)
  (with-slots (arena-table arena-by-name) registry
    (with-hash-entries ((by-name name)) arena-by-name
      (with-hash-entries ((by-server server)) arena-table
        (when by-server
          (error "Provided server already assigned to different arena"))
        (unless by-name
          (let ((arena (make-instance 'arena :name name :server server)))
            (setf by-name arena
                  by-server arena)
            arena))))))


(defun list-arena-names (registry)
  (with-slots (arena-by-name) registry
    (loop for key being the hash-key of arena-by-name
       collect key)))


(defun add-arena-client (registry arena client)
  (with-slots (clients) arena
    (push client clients))
  (with-slots (arena-table) registry
    (setf (gethash client arena-table) arena)))


(defun remove-arena-by-server (registry server)
  (with-slots (arena-table arena-by-name) registry
    (when-let ((arena (find-arena-by-peer registry server)))
      (when (eq (server-of arena) server)
        (dolist (client (clients-of arena))
          (remhash client arena-table))
        (remhash (server-of arena) arena-table)
        (remhash arena-by-name (name-of arena))))))


(defun find-arena-by-peer (registry peer)
  (with-slots (arena-table) registry
    (gethash peer arena-table)))


(defun find-arena-by-name (registry name)
  (with-slots (arena-by-name) registry
    (gethash name arena-by-name)))
