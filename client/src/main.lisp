(in-package :mortar-combat)


(define-constant +framestep+ 0.017)
(defvar *main-latch* (mt:make-latch))


(defclass mortar-combat (enableable generic-system dispatcher)
  ((scene :initform nil :reader scene-of)
   (task-queue :initform nil)

   (remote-server :initform nil)
   (game-client :initform nil)
   (game-server :initform nil)
   (identity :initform nil)

   (keymap :initform nil)
   (arena :initform nil :reader arena-of))
  (:default-initargs :depends-on '(event-system
                                   graphics-system
                                   physics-system
                                   audio-system)))


(definline mortar-combat ()
  (engine-system 'mortar-combat))


(defun connect (name)
  (with-slots (remote-server identity) (mortar-combat)
    (unless remote-server
      (let ((server (make-instance 'connector
                                   :host (property :server-address "127.0.0.1")
                                   :port (property :info-server-port 8778))))
        (setf remote-server server)
        (run (>> (identify server name)
                 (instantly (id)
                   (setf identity id))))))))


(defun update-player-camera (scene arena)
  (let ((cam (find-node (root-of scene) :camera)))
    (setf (player-of cam) (player-of arena))))


(defun create-combat-arena (name)
  (with-slots (remote-server identity game-server arena scene) (mortar-combat)
    (let* ((new-arena (make-instance 'arena :player-name (server-identity-name identity)))
           (server (make-game-server new-arena)))
      (run (>> (register-game-stream server (server-identity-id identity))
               (create-arena remote-server name)
               (-> ((mortar-combat)) ()
                   (setf arena new-arena
                         game-server server)
                   (update-player-camera scene arena)))))))


(defun join-combat-arena (name)
  (with-slots (remote-server identity game-client arena scene) (mortar-combat)
    (let* ((new-arena (make-instance 'arena :player-name (server-identity-name identity)))
           (client (make-game-client new-arena)))
      (run (>> (register-game-stream client (server-identity-id identity))
               (join-arena remote-server name)
               (-> ((mortar-combat)) ()
                 (setf arena new-arena
                       game-client client)
                 (update-player-camera scene arena)
                 (register-player client (server-identity-name identity))))))))


(defun ping-game-server ()
  (with-slots (game-client) (mortar-combat)
    (let ((start (real-time-seconds)))
      (run (>> (ping-peer game-client)
               (instantly ()
                 (log:info "Ping: ~Fs" (- (real-time-seconds) start))))))))


(defun scenegraph-flow ()
  (scenegraph
   (transform-node
    ((projection-node :aspect (/ 800 600))
     ((player-camera :name :camera)
      (room-model)
      ((scene-node :name :ball-group))
      ((scene-node :name :dude-group)))))))


(defmethod dispatch ((this mortar-combat) (task function) invariant &key)
  (with-slots (task-queue) this
    (push-task task task-queue)))


(defun send-client-data (this)
  (with-slots (game-client identity arena) this
    (when game-client
      (send-player-info game-client (player-of arena)))))


(defun broadcast-arena-state (this)
  (with-slots (game-server) this
    (when game-server
      (broadcast-game-state game-server))))


(defmethod initialize-system :after ((this mortar-combat))
  (with-slots (scene keymap task-queue game-client identity) this
    (register-resource-loader (make-resource-loader (asset-path "font.brf")
                                                    (asset-path "dude-and-mortar.brf")))
    (register-event-classes (events)
                            'player-added
                            'game-state-updated
                            'camera-rotated
                            'velocity-changed
                            'trigger-pulled)
    (setf keymap (make-instance 'keymap)
          task-queue (make-task-queue))
    (let ((prev-x nil)
          (prev-y nil)
          (movement-keys)
          (eve (events)))
      (labels ((rotate-camera (x y)
                 (when (and prev-x prev-y)
                   (let ((ax (/ (- y prev-y) 1000))
                         (ay (/ (- x prev-x) 1000)))
                     (post (make-camera-rotated ax (- ay)) eve)))
                 (setf prev-x x
                       prev-y y))
               (key-velocity (key)
                 (case key
                   (:w (vec2 0.0 +player-speed+))
                   (:s (vec2 0.0 (- +player-speed+)))
                   (:a (vec2 (- +player-speed+) 0.0))
                   (:d (vec2 +player-speed+ 0.0))))
               (update-velocity ()
                 (post (make-velocity-changed (reduce #'add movement-keys
                                                      :key #'key-velocity
                                                      :initial-value (vec2)))
                       eve))
               (update-buttons (button)
                 (lambda (state)
                   (case state
                     (:pressed (push button movement-keys))
                     (:released (deletef movement-keys button)))
                   (update-velocity)))
               (shoot (state)
                 (when (eq :pressed state)
                   (post (make-trigger-pulled) eve))))

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
             (scenegraph-flow)
             (instantly (scenegraph-root)
               (setf scene (make-scene (make-pass-chain (make-simulation-pass)
                                                        (make-rendering-pass))
                                       scenegraph-root)))
             (concurrently ()
               (let ((accumulated-time 0)
                     start looped-flow)
                 (setf looped-flow
                       (>> (instantly ()
                             (setf start (real-time-seconds))
                             (let ((*system* this))
                               (drain task-queue))
                             (when (> accumulated-time 0.05)
                               (send-client-data this)
                               (broadcast-arena-state this)
                               (setf accumulated-time 0)))
                           (-> ((physics)) ()
                             (observe-universe +framestep+))
                           (scene-processing-flow scene)
                           (instantly ()
                             (incf accumulated-time (- (real-time-seconds) start))
                             (when (enabledp this)
                               (run looped-flow)))))
                 (run looped-flow)))))))


(defmethod discard-system ((this mortar-combat))
  (with-slots (remote-server game-client game-server arena) this
    (dolist (server (list remote-server game-client game-server))
      (when server
        (disconnect-from-server server)))
    (setf remote-server nil
          game-client nil
          game-server nil)
    (when arena
      (dispose arena)
      (setf arena nil))))


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
