(in-package :mortar-combat)


(defclass game-client (connector-channel)
  ((arena :initarg :arena))
  (:default-initargs :host (property :server-address "127.0.0.1")
    :port (property :proxy-server-port 8222)))


(defun make-game-client (arena)
  (make-instance 'game-client :arena arena))


(defun register-player (client name)
  (run (relaying-flow client (make-message 'register-player :name name))))


(defun send-player-info (client player)
  (let ((pos (position-of player))
        (rot (rotation-of player)))
    (run (relaying-flow client (make-message 'player-info
                                            :name (name-of player)
                                            :timestamp (real-time-seconds)
                                            :movement (movement-of player)
                                            :position (list (x pos) (y pos))
                                            :rotation (list (x rot) (y rot)))))))


(defun send-shot-info (client player)
  (run (relaying-flow client (make-message 'shot-info
                                          :name (name-of player)
                                          :timestamp (real-time-seconds)))))


(defmethod receive-message ((client game-client) (message game-state))
  (post 'game-state-updated
        :state (game-state-state message)
        :timestamp (game-state-timestamp message))
  (call-next-method))


(defmethod receive-message ((client game-client) (message server-shot-info))
  (with-slots (arena) client
    (let ((player-name (server-shot-info-player-name message)))
      (when-let ((dude (find-dude arena player-name)))
        (post 'trigger-pulled :player dude))))
  (call-next-method))


(defmethod receive-message ((client game-client) (message server-hit-info))
  (with-slots (arena) client
    (let* ((player-name (server-hit-info-player-name message))
           (dude (find-dude arena player-name))
           (player (player-of arena)))
      (cond
        (dude (register-hit arena dude))
        ((equal player-name (name-of player)) (register-hit arena player)))))
  (call-next-method))
