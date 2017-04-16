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
   (animation :initform nil)
   (color :initarg :color)
   (animation-name :initarg :animation-name)
   (skeleton :initform nil)))


(defmethod initialization-flow ((this dude-model) &key)
  (with-slots (mesh animation skeleton program animation-name) this
    (>> (resource-flow "Dude.1" "Stickman" animation-name
                       (shading-program-resource-name "dude-program"))
        (instantly (m s a p)
          (setf mesh m
                skeleton s
                animation a
                program p)
          (start-animation animation t))
        (call-next-method))))


(defmethod model-graph-assembly-flow ((this dude-model))
  (with-slots (animation skeleton control-skeleton mesh program color) this
    (scenegraph
     ((animation-node :frames animation)
      ((animated-skeleton-node :root-bone skeleton)
       ((dude-mesh :mesh mesh :program program :color color)))))))
