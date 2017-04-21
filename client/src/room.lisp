(in-package :mortar-combat)


(defclass wall-geom (collidable plane-geom) ())
(defclass room-floor (disposable)
  (walls))


(defmethod initialize-instance :after ((this room-floor) &key)
  (with-slots (walls) this
    (setf walls (list (make-instance 'wall-geom :normal (vec3 0.0 1.0 0.0))

                      (make-instance 'wall-geom :normal (vec3 0.0 0.0 -1.0) :offset -100.0)
                      (make-instance 'wall-geom :normal (vec3 -1.0 0.0 0.0) :offset -100.0)
                      (make-instance 'wall-geom :normal (vec3 0.0 0.0 1.0) :offset -100.0)
                      (make-instance 'wall-geom :normal (vec3 1.0 0.0 0.0) :offset -100.0)))))


(define-destructor room-floor (walls)
  (dolist (geom walls)
    (dispose geom)))



;;;
;;;
;;;
(defclass room-mesh (mesh-node)
  ((light :initform (make-directional-light-source
                     (vec3 -0.21821788 -0.8728715 0.43643576)
                     (vec4 0.2 0.2 0.2 1.0)
                     (vec4 0.8 0.8 0.8 1.0)
                     "dLight"))
   (color :initform (vec3 0.9 0.9 0.9))
   (mesh-asset :initarg :mesh)
   (program :initarg :program)))


(defmethod make-node-mesh ((this room-mesh))
  (with-slots (mesh-asset) this
    (mesh-asset-mesh mesh-asset)))


(defmethod scene-pass ((this room-mesh) (pass rendering-pass) input)
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
(defclass room-model (model)
  ((floor :initform nil)
   (mesh :initform nil)
   (program :initform nil)))


(defmethod initialization-flow ((this room-model) &key)
  (with-slots (program floor mesh) this
    (>> (resource-flow "mesh.Room" (shading-program-resource-name "passthru-program"))
        (instantly (m p)
          (setf program p
                mesh m))
        (-> ((physics)) ()
          (setf floor (make-instance 'room-floor)))
        (call-next-method))))


(defmethod discard-node :before ((this room-model))
  (with-slots (floor) this
    (dispose floor)))


(defmethod model-graph-assembly-flow ((this room-model))
  (with-slots (program mesh) this
    (scenegraph
     ((room-mesh :program program :mesh mesh)))))
