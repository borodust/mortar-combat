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


(defun send-shot-info (client player)
  (run (-> (client :command :shot-info
                   :no-reply t
                   :name (name-of player)
                   :timestamp (real-time-seconds))
           ())))


(defmethod process-command ((command (eql :game-state)) message)
  (post 'game-state-updated
        :state (getf message :state)
        :timestamp (getf message :timestamp))
  nil)


(defmethod process-command ((command (eql :server-shot-info)) message)
  (with-slots (arena) *connector*
    (with-message (player-name) message
      (when-let ((dude (find-dude arena player-name)))
        (post 'trigger-pulled :player dude))))
  nil)


(defmethod process-command ((command (eql :server-hit-info)) message)
  (with-slots (arena) *connector*
    (with-message (player-name) message
      (let ((dude (find-dude arena player-name))
            (player (player-of arena)))
        (cond
          (dude (register-hit arena dude))
          ((equal player-name (name-of player)) (register-hit arena player))))))
  nil)
