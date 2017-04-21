(in-package :mortar-combat)


(defvar *dude-bounds-initial-position* (vec4 0.0 7.5 0.0 1.0))
(defvar *dude-bounds-initial-rotation* (euler-angles->mat4 (vec4 (/ pi 2) 0.0 0.0)))

(defclass dude-bounds (collidable cylinder-geom) ())


(defclass dude-body (disposable)
  (bounds))


(defmethod initialize-instance :after ((this dude-body) &key)
  (with-slots (bounds) this
    (setf bounds (make-instance 'dude-bounds
                                :radius 2.0
                                :length 13.0))
    (setf (position-of bounds) (vec3 (x *dude-bounds-initial-position*)
                                     (y *dude-bounds-initial-position*)
                                     (z *dude-bounds-initial-position*))
          (rotation-of bounds) (mat4->mat3 *dude-bounds-initial-rotation*))))


(define-destructor dude-body (bounds)
  (dispose bounds))


(defmethod (setf position-of) ((value vec3) (this dude-body))
  (with-slots (bounds) this
    (setf (position-of bounds) value)))


(defmethod transform-of ((this dude-body))
  (with-slots (bounds) this
    (transform-of bounds)))


(defmethod rotation-of ((this dude-body))
  (with-slots (bounds) this
    (rotation-of bounds)))

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
(defclass dude-model (model)
  ((mesh :initform nil)
   (body :initform nil)
   (player :initarg :player)
   (program :initform nil)
   (run-animation :initform nil)
   (rest-animation :initform nil)
   (strafe-animation :initform nil)
   (color :initarg :color)
   (skeleton :initform nil)))


(defmethod initialize-instance :after ((this dude-model) &key))


(defmethod initialization-flow ((this dude-model) &key)
  (with-slots (mesh animation skeleton program body
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
        (-> ((physics)) ()
          (setf body (make-instance 'dude-body)))
        (call-next-method))))


(defmethod discard-node ((this dude-model))
  (with-slots (body) this
    (dispose body)))


(defmethod model-graph-assembly-flow ((this dude-model))
  (with-slots (skeleton mesh program color rest-animation) this
    (scenegraph
     (scene-node
      ((animation-node :initial-animation rest-animation :name :dude-animation)
       ((animated-skeleton-node :root-bone skeleton)
        ((dude-mesh :mesh mesh :program program :color color))))
      (mortar-model)))))


(defmethod scene-pass ((this dude-model) (pass simulation-pass) input)
  (with-slots (body) this
    (let* ((pos (mult *model-matrix* *dude-bounds-initial-position*))
           (w (w pos)))
      (flet ((w/ (v)
               (/ v w)))
        (setf (position-of body) (vec3 (w/ (x pos))
                                       (w/ (y pos))
                                       (w/ (z pos)))))))
    (call-next-method))


(defmethod scene-pass ((this dude-model) pass input)
  (with-slots (player) this
    (let* ((pos (position-of player))
           (rot (rotation-of player))
           (*model-matrix* (mult *model-matrix*
                                 (translation-mat4 (x pos) 0.0 (- (y pos)))
                                 (euler-angles->mat4 (vec3 0.0 (y rot) 0.0)))))
      (call-next-method))))
