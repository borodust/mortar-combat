(in-package :mortar-combat)


(defclass player-camera (camera-node)
  ((player :initarg :player)
   (front-gaze :initform (vec3 0.0 0.0 -1.0))))


(defmethod scene-pass ((this player-camera) pass input)
  (with-slots (player front-gaze) this
    (let* ((pos (position-of player))
           (rotation (rotation-of player)))
      (setf (transform-of this) (mult (euler-angles->mat4 (vec3 (- (x rotation)) (- (y rotation)) 0.0))
                                      (translation-mat4 (- (x pos)) -13.0 (y pos))))))
  (call-next-method))
