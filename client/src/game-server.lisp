(in-package :mortar-combat)


(defclass game-server (connector)
  ((players :initform (make-hash-table :test 'equal)))
  (:default-initargs :host "localhost" :port 8222))


(defun make-game-server ()
  (make-instance 'game-server))


(defmethod process-command ((command (eql :register-player)) message)
  (with-slots (players) *connector*
    (with-message (name) message
      (with-instance-lock-held (*connector*)
        (let ((player (make-instance 'player)))
          (setf (gethash name players) player)
          (post (make-player-added-event player) (events))))))
  nil)


(defmethod process-command ((command (eql :player-info)) message)
  (with-slots (players) *connector*
    (with-message (name position rotation) message
      (with-instance-lock-held (*connector*)
        (when-let ((player (gethash name players)))
          (setf (position-of player) (sequence->vec2 position)
                (rotation-of player) (sequence->vec2 rotation))))))
  nil)
