(in-package :mortar-combat)


;;;
;;;
;;;
(defclass ball-mesh (mesh-node)
  ((light :initform (make-directional-light-source
                     (vec3 -0.57735026 -0.57735026 -0.57735026)
                     (vec4 0.2 0.2 0.2 1.0)
                     (vec4 0.8 0.8 0.8 1.0)
                     "dLight"))
   (color :initform (vec3 0.3 0.3 0.3))
   (mesh-asset :initarg :mesh)
   (program :initarg :program)))


(defmethod make-node-mesh ((this ball-mesh))
  (with-slots (mesh-asset) this
    (mesh-asset-mesh mesh-asset)))


(defmethod scene-pass ((this ball-mesh) (pass rendering-pass) input)
  (with-slots (light program color) this
    (with-active-shading-program (program)
      (setf (program-uniform-variable program "modelViewProjection") (model-view-projection-matrix)
            (program-uniform-variable program "normalTransform") (mat4->mat3 *model-matrix*)
            (program-uniform-variable program "baseColor") color)
      (apply-light-source light program)
      (call-next-method))))


;;;
;;;
;;;
(defclass ball-model (model)
  ((mesh :initform nil)
   (program :initform nil)))


(defmethod initialization-flow ((this ball-model) &key)
  (with-slots (mesh program) this
    (>> (resource-flow "mesh.Ball" (shading-program-resource-name "passthru-program"))
        (instantly (m p)
          (setf mesh m
                program p))
        (call-next-method))))


(defmethod model-graph-assembly-flow ((this ball-model))
  (with-slots (mesh program) this
    (scenegraph
     ((ball-mesh :mesh mesh :program program)))))
