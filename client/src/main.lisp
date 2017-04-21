(in-package :mortar-combat)


(define-constant +framestep+ 0.017)
(define-constant +player-speed+ 20.0)
(defvar *main-latch* (mt:make-latch))


(defclass mortar-combat (enableable generic-system dispatcher)
  ((scene :initform nil :reader scene-of)
   (task-queue :initform nil)

   (remote-server :initform nil)
   (game-client :initform nil)
   (game-server :initform nil)
   (identity :initform nil)

   (keymap :initform nil)
   (player :initform nil))
  (:default-initargs :depends-on '(event-system
                                   graphics-system
                                   physics-system
                                   audio-system)))


(definline mortar-combat ()
  (engine-system 'mortar-combat))


(defun connect (name)
  (with-slots (remote-server identity) (mortar-combat)
    (unless remote-server
      (let ((server (make-instance 'connector :host "127.0.0.1" :port 8778)))
        (setf remote-server server)
        (run (>> (identify server name)
                 (instantly (id)
                   (setf identity id))))))))


(defun create-combat-arena (name)
  (with-slots (remote-server identity game-server) (mortar-combat)
    (let ((server (make-game-server)))
      (run (>> (register-game-stream server (server-identity-id identity))
               (create-arena remote-server name)
               (instantly ()
                 (setf game-server server)))))))


(defun join-combat-arena (name)
  (with-slots (remote-server identity game-client) (mortar-combat)
    (let ((client (make-game-client)))
      (run (>> (register-game-stream client (server-identity-id identity))
               (join-arena remote-server name)
               (instantly ()
                 (setf game-client client))))
      (register-player client (server-identity-name identity)))))


(defun ping-game-server ()
  (with-slots (game-client) (mortar-combat)
    (let ((start (real-time-seconds)))
      (run (>> (ping-peer game-client)
               (instantly ()
                 (log:info "Ping: ~Fs" (- (real-time-seconds) start))))))))


(defun scenegraph-flow (player)
  (scenegraph
   (transform-node
    ((projection-node :aspect (/ 800 600))
     ((player-camera :player player)
      (room-model)
      ((scene-node :name :ball-group))
      ((scene-node :name :dude-group)))))))


(defmethod dispatch ((this mortar-combat) (task function) invariant &key)
  (with-slots (task-queue) this
    (push-task task task-queue)))


(defun shoot-ball (scene player)
  (let ((pos (position-of player)))
    (run (>> (assembly-flow 'ball-model
                            :position (vec3 (+ (x pos) 1.0) 10.0 (- (y pos)))
                            :force (mult (gaze-of player) 10000))
             (-> ((mortar-combat)) (ball)
               (let ((group (find-node (root-of scene) :ball-group)))
                 (adopt group ball)))))))


(defun send-client-data (this)
  (with-slots (game-client identity player) this
    (when game-client
      (send-player-info game-client
                        (server-identity-name identity)
                        player))))


(defmethod initialize-system :after ((this mortar-combat))
  (with-slots (scene player keymap task-queue game-client identity) this
    (register-resource-loader (make-resource-loader (asset-path "font.brf")
                                                    (asset-path "dude-and-mortar.brf")))
    (register-event-classes (events) 'player-added-event)
    (setf player (make-instance 'player)
          keymap (make-instance 'keymap)
          task-queue (make-task-queue))

    (let ((prev-x nil)
          (prev-y nil)
          (movement-keys))
      (labels ((rotate-camera (x y)
                 (when (and prev-x prev-y)
                   (let ((ax (/ (- y prev-y) 1000))
                         (ay (/ (- x prev-x) 1000)))
                     (look-at player ax (- ay))))
                 (setf prev-x x
                       prev-y y))
               (key-velocity (key)
                 (case key
                   (:w (vec2 0.0 +player-speed+))
                   (:s (vec2 0.0 (- +player-speed+)))
                   (:a (vec2 (- +player-speed+) 0.0))
                   (:d (vec2 +player-speed+ 0.0))))
               (update-velocity ()
                 (setf (player-velocity player) (reduce #'add movement-keys
                                                        :key #'key-velocity
                                                        :initial-value (vec2))))
               (update-buttons (button)
                 (lambda (state)
                   (case state
                     (:pressed (push button movement-keys))
                     (:released (deletef movement-keys button)))
                   (update-velocity)))
               (shoot (state)
                 (when (eq :pressed state)
                   (shoot-ball scene player))))

        (bind-cursor keymap #'rotate-camera)

        (bind-button keymap :w (update-buttons :w))
        (bind-button keymap :s (update-buttons :s))
        (bind-button keymap :a (update-buttons :a))
        (bind-button keymap :d (update-buttons :d))
        (bind-button keymap :mouse-left #'shoot)))

    (enable-keymap keymap)

    (run (>> (-> ((host)) ()
               (lock-cursor)
               (setf (viewport-title) "Mortar Combat")
               (setf (viewport-size) (vec2 800 600)))
             (-> ((physics)) ()
               (setf (gravity) (vec3 0.0 -9.81 0.0)))
             (-> ((graphics)) ()
               (gl:viewport 0 0 800 600))
             (scenegraph-flow player)
             (instantly (scenegraph-root)
               (setf scene (make-scene (make-pass-chain (make-simulation-pass)
                                                        (make-rendering-pass))
                                       scenegraph-root))
               (let ((dudes (find-node (root-of scene) :dude-group)))
                 (subscribe-body-to (player-added-event (player)) (events)
                   (run (>> (assembly-flow 'dude-model
                                           :player player
                                           :color (vec3 0.9 0.4 0.4))
                            (-> this (dude)
                              (adopt dudes dude)))))))
             (concurrently ()
               (let (looped-flow)
                 (setf looped-flow (>> (instantly ()
                                         (let ((*system* this))
                                           (drain task-queue))
                                         (send-client-data this))
                                       (-> ((physics)) ()
                                         (observe-universe +framestep+))
                                       (scene-processing-flow scene)
                                       (instantly ()
                                         (when (enabledp this)
                                           (run looped-flow)))))
                 (run looped-flow)))))))


(defun start (configuration-path)
  (startup configuration-path (uiop:pathname-directory-pathname configuration-path)))


(defun stop ()
  (shutdown))


(define-event-handler on-exit viewport-hiding-event (ev)
  (in-new-thread "exit-thread"
    (stop)))


(defun main (args)
  (start (merge-pathnames (second args) (uiop:getcwd)))
  (mt:wait-for-latch *main-latch*))
