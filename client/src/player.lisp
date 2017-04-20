(in-package :mortar-combat)


(defvar *forward-gaze* (vec3 0.0 0.0 -1.0))


(defclass player ()
  ((position :initform (vec2 0.0 18.0))
   (rotation :initform (vec2 0.0 0.0) :reader rotation-of)

   (updated-at :initform (real-time-seconds))
   (velocity :initform (vec2 0.0 0.0))))


(defun flush-position (player)
  (with-slots (position updated-at velocity rotation) player
    (let ((now (real-time-seconds)))
      (setf position (add position (mult (angle->mat2 (y rotation))
                                         velocity
                                         (- now updated-at)))
            updated-at now))))


(defmethod position-of ((this player))
  (with-slots (position) this
    (flush-position this)
    position))


(defun (setf player-velocity) (vec2 player)
  (with-slots (position velocity) player
    (flush-position player)
    (setf velocity vec2)))


(defun look-at (player x-angle y-angle)
  (with-slots (rotation) player
    (incf (y rotation) y-angle)
    (incf (x rotation) x-angle)))
