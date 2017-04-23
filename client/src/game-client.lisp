(in-package :mortar-combat)


(defclass game-client (connector)
  ((arena :initarg :arena))
  (:default-initargs :host (property :server-address "127.0.0.1")
    :port (property :proxy-server-port 8222)))


(defun make-game-client (arena)
  (make-instance 'game-client :arena arena))


(defun register-player (client name)
  (run (-> (client :command :register-player
                   :name name)
           ())))


(defun send-player-info (client player)
  (let ((pos (position-of player))
        (rot (rotation-of player)))
    (run (-> (client :command :player-info
                     :no-reply t
                     :name (name-of player)
                     :timestamp (real-time-seconds)
                     :movement (movement-of player)
                     :position (list (x pos) (y pos))
                     :rotation (list (x rot) (y rot)))
             ()))))


(defmethod process-command ((command (eql :game-state)) message)
  (post (make-game-state-updated (getf message :state) (getf message :timestamp)) (events))
  nil)
