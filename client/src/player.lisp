(in-package :mortar-combat)

(define-constant +player-speed+ 20.0)

(defvar *forward-gaze* (vec3 0.0 0.0 -1.0))


(defgeneric position-of (player))
(defgeneric rotation-of (player))
(defgeneric name-of (player))

(defclass player (subscriber)
  ((name :initarg :name :initform (error ":name missing") :reader name-of)
   (position :initform (vec2)) ; f(x,y) field space = f(x,-z) global space
   (rotation :initform (vec2) :accessor rotation-of)

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


(defun gaze-of (player)
  "In global coords"
  (with-slots (rotation) player
    (normalize (rotate *forward-gaze* (euler-angles->quat (x rotation) (y rotation) 0.0)))))


(defun (setf velocity-of) (vec2 player)
  (with-slots (position velocity) player
    (flush-position player)
    (setf velocity vec2)))


(defun look-at (player x-angle y-angle)
  (with-slots (rotation) player
    (incf (y rotation) y-angle)
    (incf (x rotation) x-angle)))


(defmethod initialize-instance :after ((this player) &key)
  (flet ((update-velocity (ev)
           (setf (velocity-of this) (velocity-from ev)))
         (update-rotation (ev)
           (look-at this (ax-from ev) (ay-from ev))))
    (register-event-handler 'camera-rotated #'update-rotation)
    (register-event-handler 'velocity-changed #'update-velocity)))
