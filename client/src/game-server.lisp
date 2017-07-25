(in-package :mortar-combat)


(defclass game-server-channel (subscribing connector-channel)
  ((arena :initarg :arena)))


(defun make-game-server-channel (stream arena)
  (make-instance 'game-server-channel :stream stream :arena arena))


(defun broadcast-shot-info (server player)
  (run (relaying-flow server (make-message 'server-shot-info
                                          :player-name (name-of player)))))


(defun broadcast-hit-info (server player)
  (run (relaying-flow server (make-message 'server-hit-info
                                          :player-name (name-of player)))))


(defmethod initialize-instance :after ((this game-server-channel) &key)
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


(defmethod receive-message ((server game-server-channel) (message register-player))
  (with-slots (arena) server
    (let* ((name (register-player-name message))
           (player (make-instance 'proxy :name name)))
      (add-dude arena player)
      (post 'player-added :player player)))
  (call-next-method))


(defmethod receive-message ((server game-server-channel) (message player-info))
  (with-slots (arena) server
    (let ((name (player-info-name message))
          (position (player-info-position message))
          (rotation (player-info-rotation message))
          (timestamp (player-info-timestamp message))
          (movement (player-info-movement message)))
      (when-let ((player (find-dude arena name)))
        (update-proxy player
                      (sequence->vec2 position)
                      (sequence->vec2 rotation)
                      timestamp
                      movement))))
  (call-next-method))


(defmethod receive-message ((server game-server-channel) (message shot-info))
  (with-slots (arena) server
    (let ((name (shot-info-name message)))
      (when-let ((player (find-dude arena name)))
        (post (make-trigger-pulled player)))))
  (call-next-method))


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
        (run (relaying-flow server
                           (make-message 'game-state
                                         :timestamp (real-time-seconds)
                                         :state (list :player-list proxies))))))))
