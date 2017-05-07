(in-package :mortar-combat)


(defclass proxy (disposable)
  ((name :initarg :name :initform (error ":name missing") :reader name-of)
   (movement :initform nil :reader movement-of)
   (position :initform (vec2)) ; f(x,y) field space = f(x,-z) global space
   (rotation :initform (vec2) :reader rotation-of)
   (updated-at :initform (real-time-seconds))

   (next-position :initform (vec2))
   (next-rotation :initform (vec2))
   (next-at :initform (real-time-seconds))

   (correction :initform 0.0)))


(defun proxy-lerp-factor (proxy)
  (with-slots (updated-at next-at correction) proxy
    (let* ((now (- (real-time-seconds) correction))
           (delta (- next-at updated-at)))
      (if (= delta 0.0) 1.0 (/ (- now next-at) delta)))))


(defmethod position-of ((this proxy))
  (with-slots (position next-position) this
    (lerp position next-position (proxy-lerp-factor this))))


(defmethod rotation-of ((this proxy))
  (with-slots (rotation next-rotation) this
    (lerp rotation next-rotation (proxy-lerp-factor this))))


(defmethod gaze-of ((this proxy))
  (with-slots (next-rotation) this
    (calc-gaze next-rotation)))


(defun update-proxy (proxy pos rot timestamp movement)
  (with-slots (next-position
               next-at position updated-at
               correction rotation next-rotation
               (this-movement movement))
      proxy
    (unless (eq this-movement movement)
      (setf this-movement movement)
      (post 'movement-changed :player proxy :direction movement))
    (setf correction (- (real-time-seconds) timestamp)

          position (position-of proxy)
          rotation (rotation-of proxy)
          updated-at next-at

          next-position pos
          next-rotation rot
          next-at timestamp)))
