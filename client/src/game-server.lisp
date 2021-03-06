(in-package :mortar-combat)


(defclass game-server (subscribing connector)
  ((arena :initarg :arena))
  (:default-initargs :host (property :server-address "127.0.0.1")
    :port (property :proxy-server-port 8222)))


(defun make-game-server (arena)
  (make-instance 'game-server :arena arena))


(defun broadcast-shot-info (server player)
  (run (-> (server :command :server-shot-info
                   :no-reply t
                   :player-name (name-of player))
           ())))


(defun broadcast-hit-info (server player)
  (run (-> (server :command :server-hit-info
                   :no-reply t
                   :player-name (name-of player))
           ())))


(defmethod initialize-instance :after ((this game-server) &key)
  (with-slots (arena) this
    (flet ((broadcast-shot (ev)
             (broadcast-shot-info this (player-from ev)))
           (broadcast-hit (ev)
             (let ((player (player-from ev)))
               (register-hit arena player)
               (broadcast-hit-info this player))))
      (add-event-handler this 'trigger-pulled #'broadcast-shot)
      (add-event-handler this 'hit-detected #'broadcast-hit)
      (employ-subscriber this))))


(defmethod process-command ((command (eql :register-player)) message)
  (with-slots (arena) *connector*
    (with-message (name) message
      (let ((player (make-instance 'proxy :name name)))
        (add-dude arena player)
        (post 'player-added :player player))))
  nil)


(defmethod process-command ((command (eql :player-info)) message)
  (with-slots (arena) *connector*
    (with-message (name position rotation timestamp movement) message
      (when-let ((player (find-dude arena name)))
        (update-proxy player
                      (sequence->vec2 position)
                      (sequence->vec2 rotation)
                      timestamp
                      movement))))
  nil)


(defmethod process-command ((command (eql :shot-info)) message)
  (with-slots (arena) *connector*
    (with-message (name) message
      (when-let ((player (find-dude arena name)))
        (post (make-trigger-pulled player) (events)))))
  nil)


(defun broadcast-game-state (server)
  (with-slots (arena) server
    (flet ((player-info (p)
             (let ((pos (position-of p))
                   (rot (rotation-of p)))
             (list :name (name-of p)
                   :position (list (x pos) (y pos))
                   :rotation (list (x rot) (y rot))
                   :movement (movement-of p)))))
      (let ((proxies (mapcar #'player-info (cons (player-of arena) (dudes-of arena)))))
        (run (-> (server :command :game-state
                         :no-reply t
                         :timestamp (real-time-seconds)
                         :state (list :player-list proxies))
                 ()))))))
