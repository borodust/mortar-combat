(in-package :mortar-combat)


(define-constant +world-step+ 0.016)
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
               (assembly-flow 'player-node :player (player-of new-arena))
               (-> ((mortar-combat)) (player)
                   (setf arena new-arena
                         game-server server)
                   (update-player-camera scene arena)
                   (adopt (find-node (root-of scene) :dude-group) player))
               (-> ((host)) ()
                 (lock-cursor)))))))


(define-event-handler on-arena-create new-arena-requested (ev name)
  (create-combat-arena name))


(defun join-combat-arena (name)
  (with-slots (remote-server identity game-client arena scene) (mortar-combat)
    (let* ((new-arena (make-instance 'arena :player-name (server-identity-name identity)))
           (client (make-game-client new-arena)))
      (run (>> (register-game-stream client (server-identity-id identity))
               (join-arena remote-server name)
               (assembly-flow 'player-node :player (player-of new-arena))
               (-> ((mortar-combat)) (player)
                 (setf arena new-arena
                       game-client client)
                 (update-player-camera scene arena)
                 (adopt (find-node (root-of scene) :dude-group) player)
                 (register-player client (server-identity-name identity)))
               (-> ((host)) ()
                 (lock-cursor)))))))


(defun load-arena-list ()
  (with-slots (remote-server) (mortar-combat)
    (get-arena-list remote-server)))


(define-event-handler on-arena-join arena-join-requested (ev name)
  (join-combat-arena name))


(defun ping-game-server ()
  (with-slots (game-client) (mortar-combat)
    (let ((start (real-time-seconds)))
      (run (>> (ping-peer game-client)
               (instantly ()
                 (log:info "Ping: ~Fs" (- (real-time-seconds) start))))))))


(defun scenegraph-flow ()
  (>> (resource-flow (font-resource-name "NotoSansUI-Regular.ttf"))
      (->> (font)
        (scenegraph
         (transform-node
          ((scene-node :name :arena)
           ((projection-node :aspect (/ 800 600))
            ((player-camera :name :camera)
             (room-model)
             ((scene-node :name :ball-group))
             ((scene-node :name :dude-group)))))
          ((interactive-board-node :width 800 :height 600 :font font :name :ui)))))))


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
  (with-slots (scene keymap task-queue game-client identity arena) this
    (register-resource-loader (make-resource-loader (asset-path "font.brf")
                                                    (asset-path "dude-and-mortar.brf")))
    (register-event-classes (events)
                            'player-added
                            'game-state-updated
                            'camera-rotated
                            'movement-changed
                            'trigger-pulled
                            'exit-requested
                            'new-arena-requested
                            'arena-join-requested
                            'arena-leave-requested
                            'hit-detected)
    (register-poiu-events (events))
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
               (update-movement ()
                 (flet ((set-equal (this that)
                          (null (set-difference this that))))
                   (let* ((first (first movement-keys))
                          (second (second movement-keys))
                          (both (list first second))
                          (direction (cond
                                       ((null first) nil)
                                       ((null second) (case first
                                                        (:w :north)
                                                        (:a :west)
                                                        (:s :south)
                                                        (:d :east)))
                                       (second (switch (both :test #'set-equal)
                                                 ('(:w :a) :north-west)
                                                 ('(:w :d) :north-east)
                                                 ('(:s :a) :south-west)
                                                 ('(:s :d) :south-east))))))
                     (when arena
                       (post (make-movement-changed (player-of arena) direction) eve)))))
               (update-buttons (button)
                 (lambda (state)
                   (case state
                     (:pressed (push button movement-keys))
                     (:released (deletef movement-keys button)))
                   (update-movement)))
               (shoot (state)
                 (when (and (eq :pressed state) arena)
                   (let ((player (player-of arena)))
                     (when game-client
                       (send-shot-info game-client player))
                     (post (make-trigger-pulled player) eve)))))
        (bind-cursor keymap #'rotate-camera)
        (bind-button keymap :w (update-buttons :w))
        (bind-button keymap :s (update-buttons :s))
        (bind-button keymap :a (update-buttons :a))
        (bind-button keymap :d (update-buttons :d))
        (bind-button keymap :mouse-left #'shoot)))

    (enable-keymap keymap)

    (run (>> (-> ((host)) ()
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
                                       scenegraph-root))
               (let ((board (find-node (root-of scene) :ui)))
                 (make-ui board))
               (let ((accumulated-packet-time 0)
                     (accumulated-step-time 0)
                     start looped-flow)
                 (setf looped-flow
                       (>> (instantly ()
                             (setf start (real-time-seconds))
                             (let ((*system* this))
                               (drain task-queue))
                             (when (> accumulated-packet-time 0.05)
                               (send-client-data this)
                               (broadcast-arena-state this)
                               (setf accumulated-packet-time 0)))
                           (-> ((physics)) ()
                             (when (> accumulated-step-time (- +world-step+ 0.02))
                               (observe-universe +world-step+)
                               (setf accumulated-step-time 0)))
                           (scene-processing-flow scene)
                           (instantly ()
                             (let ((delta (- (real-time-seconds) start)))
                               (incf accumulated-packet-time delta)
                               (incf accumulated-step-time delta))
                             (when (enabledp this)
                               (run looped-flow)))))
                 (run looped-flow)))))))


(defmethod discard-system :before ((this mortar-combat))
  (with-slots (scene remote-server game-client game-server arena) this
    (dolist (server (list remote-server game-client game-server))
      (when server
        (disconnect-from-server server)))
    ;; fixme: dispose scene after all
    #++(dispose scene)
    (setf remote-server nil
          game-client nil
          game-server nil)
    (when arena
      (dispose arena)
      (setf arena nil))))


(defun start (configuration-path)
  (startup configuration-path (uiop:pathname-directory-pathname configuration-path)))


(defun stop ()
  (shutdown)
  (mt:open-latch *main-latch*))


(define-event-handler on-close viewport-hiding-event (ev)
  (post (make-exit-requested) (events)))


(define-event-handler on-exit exit-requested (ev)
  (in-new-thread "exit-thread"
    (stop)))


(defun main (args)
  (start (merge-pathnames (second args) (uiop:getcwd)))
  (mt:wait-for-latch *main-latch*))
