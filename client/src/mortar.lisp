(in-package :mortar-combat)


;;;
;;;
;;;
(defclass mortar-mesh (mesh-node)
  ((light :initform (make-directional-light-source
                     (vec3 -0.57735026 -0.57735026 -0.57735026)
                     (vec4 0.2 0.2 0.2 1.0)
                     (vec4 0.8 0.8 0.8 1.0)
                     "dLight"))
   (color :initform (vec3 1.0 1.0 1.0))
   (mesh-asset :initarg :mesh)
   (program :initarg :program)))


(defmethod make-node-mesh ((this mortar-mesh))
  (with-slots (mesh-asset) this
    (mesh-asset-mesh mesh-asset)))


(defmethod scene-pass ((this mortar-mesh) (pass rendering-pass) input)
  (with-slots (mesh-asset light program color) this
    (with-active-shading-program (program)
      (setf (program-uniform-variable program "modelViewProjection") (model-view-projection-matrix)
            (program-uniform-variable program "normalTransform") (mat4->mat3 *model-matrix*)
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
(defclass mortar-model (model)
  ((mesh :initform nil)
   (program :initform nil)
   (animation :initform nil)
   (skeleton :initform nil)))


(defmethod initialization-flow ((this mortar-model) &key)
  (with-slots (mesh animation skeleton program) this
    (>> (resource-flow "mesh.Mortar" "MortarSkeleton"
                       "animation.MortarRest"
                       (shading-program-resource-name "dude-program"))
        (instantly ((m s a p))
          (setf mesh m
                skeleton s
                animation a
                program p))
        (call-next-method))))


(defmethod model-graph-assembly-flow ((this mortar-model))
  (with-slots (animation skeleton mesh program color) this
    (scenegraph
     ((animation-node :initial-animation animation)
      ((animated-skeleton-node :root-bone skeleton)
       ((mortar-mesh :mesh mesh :program program)))))))
