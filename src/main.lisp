(in-package :mortar-combat)


(defvar *main-latch* (mt:make-latch))


(defclass mortar-combat (enableable generic-system)
  ((scene :initform nil))
  (:default-initargs :depends-on '(graphics-system
                                   physics-system
                                   audio-system)))


(definline mortar-combat ()
  (engine-system 'mortar-combat))


(defun scenegraph-flow ()
  (scenegraph
   (transform-node
    ((projection-node :aspect (/ 800 600))
     (player-camera
      ((transform-node :translation (vec3 -4.0 0.0 0.0))
       ((dude-model :color (vec3 0.2 0.2 0.4) :animation-name "Strafing.animation.1")))
      ((transform-node :translation (vec3 4.0 0.0 0.0))
       ((dude-model :color (vec3 0.4 0.2 0.2) :animation-name "Running.animation.0"))))))))


(defmethod initialize-system :after ((this mortar-combat))
  (with-slots (scene) this
    (register-resource-loader (make-resource-loader (asset-path "font.brf")
                                                    (asset-path "dude-and-mortar.brf")))
    (run (>> (-> ((host)) ()
               (setf (viewport-title) "Mortar Combat")
               (setf (viewport-size) (vec2 800 600)))
             (-> ((physics)) ()
               (setf (gravity) (vec3 0.0 -9.81 0.0)))
             (scenegraph-flow)
             (instantly (scenegraph-root)
               (setf scene (make-scene (make-pass-chain (make-simulation-pass)
                                                        (make-rendering-pass))
                                       scenegraph-root)))
             (concurrently ()
               (let (looped-flow)
                 (setf looped-flow (>> (scene-processing-flow scene)
                                       (instantly ()
                                         (when (enabledp this)
                                           (run looped-flow)))))
                 (run looped-flow)))))))


(defun start (configuration-path)
  (startup configuration-path (uiop:pathname-directory-pathname configuration-path)))


(defun stop ()
  (shutdown))


(defun main (args)
  (start (merge-pathnames (second args) (uiop:getcwd)))
  (mt:wait-for-latch *main-latch*))
