(in-package :mortar-combat)


(defclass game-client (connector) ()
  (:default-initargs :host "localhost" :port 8222))


(defun make-game-client ()
  (make-instance 'game-client))


(defun register-player (client name)
  (run (-> (client :command :register-player
                   :name name)
           ())))


(defun send-player-info (client name player)
  (let ((pos (position-of player))
        (rot (rotation-of player)))
    (run (-> (client :command :player-info
                     :name name
                     :position (list (x pos) (y pos))
                     :rotation (list (x rot) (y rot)))
             ()))))
