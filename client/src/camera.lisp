(in-package :mortar-combat)


(defclass player-camera (camera-node) ())


(defmethod scene-pass ((this player-camera) pass input)
  (setf (transform-of this) (mult (translation-mat4 0 -4 -50)
                                  (euler-angles->mat4 (vec3 (/ pi 4) (/ pi 8) 0))))
  (call-next-method))
