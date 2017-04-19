(in-package :mortar-combat)


;;;
;;;
;;;
(defclass dude-mesh (mesh-node)
  ((light :initform (make-directional-light-source
                     (vec3 -0.57735026 -0.57735026 -0.57735026)
                     (vec4 0.2 0.2 0.2 1.0)
                     (vec4 0.8 0.8 0.8 1.0)
                     "dLight"))
   (color :initform (vec3 0.4 0.2 0.2) :initarg :color)
   (mesh-asset :initarg :mesh)
   (program :initarg :program)))


(defmethod make-node-mesh ((this dude-mesh))
  (with-slots (mesh-asset) this
    (mesh-asset-mesh mesh-asset)))


(defmethod scene-pass ((this dude-mesh) (pass rendering-pass) input)
  (with-slots (mesh-asset light program color) this
    (with-active-shading-program (program)
      (setf (program-uniform-variable program "modelViewProjection") (model-view-projection-matrix)
            (program-uniform-variable program "normalTransform") (mat4->mat3 (mult *view-matrix*
                                                                                   *model-matrix*))
            (program-uniform-variable program "baseColor") color)
      (apply-light-source light program)
      (loop for (name . offset) across (mesh-asset-bones mesh-asset)
         for idx = 0 then (1+ idx) do
           (setf (program-uniform-variable program (format nil "bones[~d].transform" idx))
                 (mult (bone-transform name) offset)))
      (call-next-method))))


;;;
;;;
;;;
(defclass dude-model (model)
  ((mesh :initform nil)
   (program :initform nil)
   (run-animation :initform nil)
   (rest-animation :initform nil)
   (strafe-animation :initform nil)
   (color :initarg :color)
   (skeleton :initform nil)))


(defmethod initialize-instance :after ((this dude-model) &key))


(defmethod initialization-flow ((this dude-model) &key)
  (with-slots (mesh animation skeleton program
                    strafe-animation run-animation rest-animation)
      this
    (>> (resource-flow "mesh.Dude" "Stickman"
                       "animation.Resting"
                       "animation.Running"
                       "animation.Strafing"
                       (shading-program-resource-name "dude-program"))
        (instantly (m s rest run strafe p)
          (setf mesh m
                skeleton s
                strafe-animation strafe
                run-animation run
                rest-animation rest
                program p))
        (call-next-method)
        (instantly ()
          (let ((node (find-node (model-root-of this) :dude-animation)))
            (subscribe-body-to (keyboard-event (key state)) (events)
              (case key
                (:w (case state
                      (:pressed (play-node-animation node run-animation 0.15))
                      (:released (play-node-animation node rest-animation 0.15))))
                (:d (case state
                      (:pressed  (play-node-animation node strafe-animation 0.15))
                      (:released (play-node-animation node rest-animation 0.15)))))))))))


(defmethod model-graph-assembly-flow ((this dude-model))
  (with-slots (skeleton mesh program color rest-animation) this
    (scenegraph
     ((animation-node :initial-animation rest-animation :name :dude-animation)
      ((animated-skeleton-node :root-bone skeleton)
       ((dude-mesh :mesh mesh :program program :color color)))))))
