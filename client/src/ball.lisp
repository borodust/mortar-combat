(in-package :mortar-combat)


(defclass ball-geom (ownable collidable sphere-geom)
  ((body :initarg :body :accessor body-of)))


(defmethod collide ((this ball-geom) (that ball-geom))
  nil)


(defmethod filter-contacts (contacts (this ball-geom) (that dude-bounds))
  (unless (or (eq (owner-of this) (owner-of that))
              (< (vector-length (linear-velocity-of (body-of this))) 10))
    (post (make-hit-detected (owner-of that)) (events)))
  contacts)


(defmethod filter-contacts (contacts (that dude-bounds) (this ball-geom))
  (filter-contacts contacts this that))


;;
(defclass ball-body (disposable)
  (body geom))


(defmethod initialize-instance :after ((this ball-body) &key position force owner)
  (with-slots (body geom) this
    (setf body (make-rigid-body)
          geom (make-instance 'ball-geom
                              :body body
                              :owner owner
                              :radius (/ 1.025 2)))
    (when force
      (apply-force body force))
    (when position
      (setf (position-of body) position))
    (bind-geom geom body)))


(defmethod transform-of ((this ball-body))
  (with-slots (body) this
    (transform-of body)))


(define-destructor ball-body (body geom)
  (dispose geom)
  (dispose body))

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
  (body mesh program))


(defmethod initialization-flow ((this ball-model) &key position force owner)
  (with-slots (body mesh program) this
    (>> (resource-flow "mesh.Ball" (shading-program-resource-name "passthru-program"))
        (instantly (m p)
          (setf mesh m
                program p))
        (-> ((physics)) ()
          (setf body (make-instance 'ball-body
                                    :owner owner
                                    :position position
                                    :force force)))
        (call-next-method))))


(defmethod discard-node :before ((this ball-model))
  (with-slots (body) this
    (dispose body)))


(defmethod scene-pass ((this ball-model) (pass simulation-pass) input)
  (with-slots (body) this
    (let ((result (transform-of body)))
      (call-next-method)
      result)))


(defmethod scene-pass ((this ball-model) (pass rendering-pass) ball-transform)
  (let ((*model-matrix* ball-transform))
    (call-next-method)))


(defmethod model-graph-assembly-flow ((this ball-model))
  (with-slots (mesh program) this
    (scenegraph
     ((ball-mesh :mesh mesh :program program)))))
