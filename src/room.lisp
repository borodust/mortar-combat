(in-package :mortar-combat)


;;;
;;;
;;;
(defclass room-mesh (mesh-node)
  ((light :initform (make-directional-light-source
                     (vec3 -0.57735026 -0.57735026 -0.57735026)
                     (vec4 0.2 0.2 0.2 1.0)
                     (vec4 0.8 0.8 0.8 1.0)
                     "dLight"))
   (color :initform (vec3 0.9 0.9 0.9))
   (program :initarg :program)))


(defmethod make-node-mesh ((this room-mesh))
  (with-disposable ((vbuf (make-array-buffer #2a((50.0 0.0 50.0)
                                                 (50.0 0.0 -50.0)
                                                 (-50.0 0.0 50.0)
                                                 (-50.0 0.0 -50.0))))
                    (nbuf (make-array-buffer #2a((0.0 1.0 0.0) (0.0 1.0 0.0)
                                                 (0.0 1.0 0.0) (0.0 1.0 0.0)))))
    (let ((mesh (make-mesh 4 :triangle-strip)))
      (attach-array-buffer vbuf mesh 0)
      (attach-array-buffer nbuf mesh 1)
      mesh)))


(defmethod scene-pass ((this room-mesh) (pass rendering-pass) input)
  (with-slots (light program color) this
    (with-active-shading-program (program)
      (setf (program-uniform-variable program "modelViewProjection") (model-view-projection-matrix)
            (program-uniform-variable program "normalTransform") (mat4->mat3 (mult *view-matrix*
                                                                                   *model-matrix*))
            (program-uniform-variable program "baseColor") color)
      (apply-light-source light program)
      (call-next-method))))


;;;
;;;
;;;
(defclass room-model (model)
  ((mesh :initform nil)
   (program :initform nil)))


(defmethod initialization-flow ((this room-model) &key)
  (with-slots (program) this
    (>> (resource-flow (shading-program-resource-name "passthru-program"))
        (instantly (p)
          (setf program p))
        (call-next-method))))


(defmethod model-graph-assembly-flow ((this room-model))
  (with-slots (program) this
    (scenegraph
     ((room-mesh :program program)))))
