(in-package :mortar-combat)


(define-constant +framestep+ 0.016)
(defvar *main-latch* (mt:make-latch))



(defclass mortar-combat (enableable generic-system)
  ((scene :initform nil)
   (keymap :initform (make-instance 'keymap))
   (player :initform nil))
  (:default-initargs :depends-on '(graphics-system
                                   physics-system
                                   audio-system)))


(definline mortar-combat ()
  (engine-system 'mortar-combat))


(defun scenegraph-flow (player)
  (scenegraph
   (transform-node
    ((projection-node :aspect (/ 800 600))
     ((player-camera :player player)
      (room-model)
      (ball-model)
      ((transform-node :translation (vec3 4.0 0.0 0.0))
       (mortar-model)
       ((dude-model :color (vec3 0.9 0.4 0.4)))))))))


(defmethod initialize-system :after ((this mortar-combat))
  (with-slots (scene player keymap) this
    (register-resource-loader (make-resource-loader (asset-path "font.brf")
                                                    (asset-path "dude-and-mortar.brf")))
    (setf player (make-instance 'player))

    (let ((prev-x nil)
          (prev-y nil)
          (movement-keys))
      (labels ((rotate-camera (x y)
                 (when (and prev-x prev-y)
                   (let ((ax (/ (- y prev-y) 1000))
                         (ay (/ (- x prev-x) 1000)))
                     (look-at player ax ay)))
                 (setf prev-x x
                       prev-y y))
               (key-velocity (key)
                 (case key
                   (:w (vec2 0.0 -10.0))
                   (:s (vec2 0.0 10.0))
                   (:a (vec2 -10.0 0.0))
                   (:d (vec2 10.0 0.0))))
               (update-velocity ()
                 (setf (player-velocity player) (reduce #'add movement-keys
                                                        :key #'key-velocity
                                                        :initial-value (vec2))))
               (update-buttons (button)
                 (lambda (state)
                   (case state
                     (:pressed (push button movement-keys))
                     (:released (deletef movement-keys button)))
                   (update-velocity))))
        (bind-cursor keymap #'rotate-camera)
        (bind-button keymap :w (update-buttons :w))
        (bind-button keymap :s (update-buttons :s))
        (bind-button keymap :a (update-buttons :a))
        (bind-button keymap :d (update-buttons :d))))

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
                                       scenegraph-root)))
             (concurrently ()
               (let (looped-flow)
                 (setf looped-flow (>> (-> ((physics)) ()
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
  (in-new-thread-waiting
      (stop)))


(defun main (args)
  (start (merge-pathnames (second args) (uiop:getcwd)))
  (mt:wait-for-latch *main-latch*))
