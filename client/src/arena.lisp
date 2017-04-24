(in-package :mortar-combat)


(defclass arena (lockable subscriber)
  ((player :reader player-of)
   (dudes :initform (make-hash-table :test 'equal))
   (score-table :initform (make-hash-table :test 'equal))))


(defun dudes-of (arena)
  (with-slots (dudes) arena
    (loop for dude being the hash-value of dudes
       collect dude)))


(defun add-dude (arena dude)
  (with-slots (dudes) arena
    (with-instance-lock-held (arena)
      (setf (gethash (name-of dude) dudes) dude))))


(defun find-dude (arena name)
  (with-slots (dudes) arena
    (with-instance-lock-held (arena)
      (gethash name dudes))))


(defun register-hit (arena player)
  (with-slots (score-table) arena
    (with-instance-lock-held (arena)
      (incf (gethash (name-of player) score-table 0)))))


(defun score (arena)
  (with-slots (score-table) arena
    (with-instance-lock-held (arena)
      (loop for name being the hash-key of score-table
         using (hash-value score)
         collect (cons name score)))))


(defun update-game-state (this state timestamp)
  (with-slots (dudes player) this
    (dolist (dude-state (getf state :player-list))
      (with-instance-lock-held (this)
        (let* ((dude-name (getf dude-state :name)))
          (unless (equal dude-name (name-of player))
            (let ((dude (gethash dude-name dudes)))
              (unless dude
                (setf dude (make-instance 'proxy :name dude-name)
                      (gethash dude-name dudes) dude)
                (post (make-player-added dude) (events)))
              (update-proxy dude
                            (sequence->vec2 (getf dude-state :position))
                            (sequence->vec2 (getf dude-state :rotation))
                            timestamp
                            (getf dude-state :movement)))))))))


(defun shoot-ball (player)
  (let ((pos (position-of player)))
    (run (>> (assembly-flow 'ball-model
                            :owner player
                            :position (vec3 (+ (x pos) 1.0) 10.0 (- (y pos)))
                            :force (mult (gaze-of player) 20000))
             (-> ((mortar-combat)) (ball)
               (let ((group (find-node (root-of (scene-of *system*)) :ball-group)))
                 (adopt group ball)))))))


(defmethod initialize-instance :after ((this arena) &key player-name)
  (with-slots (player dudes) this
    (unless player-name
      (error "Player name should be provided for arena"))
    (setf player (make-instance 'player :name player-name))
    (flet ((add-player (ev)
             (run (>> (assembly-flow 'dude-model
                                     :player (player-from ev)
                                     :color (vec3 0.9 0.4 0.4))
                      (-> ((mortar-combat)) (dude)
                        (let ((dude-group (find-node (root-of (scene-of *system*)) :dude-group)))
                          (adopt dude-group dude))))))
           (game-state-updated (ev)
             (update-game-state this (state-from ev) (timestamp-from ev)))
           (shoot (ev)
             (shoot-ball (player-from ev))))
      (register-event-handler 'trigger-pulled #'shoot)
      (register-event-handler 'player-added #'add-player)
      (register-event-handler 'game-state-updated #'game-state-updated))))
