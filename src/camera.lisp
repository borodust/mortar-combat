(in-package :mortar-combat)


(defclass player-camera (camera-node) ())


(defmethod scene-pass ((this player-camera) pass input)
  (setf (transform-of this) (mult (translation-mat4 0 -5 -30)
                                  (euler-angles->mat4 (vec3 (/ pi -2) 0.0 0.0))))
  (call-next-method))
