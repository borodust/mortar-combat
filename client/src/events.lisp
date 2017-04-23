(in-package :mortar-combat)


(defclass subscriber (disposable)
  ((callbacks :initform (cons nil nil))))


(defun register-event-handler (class handler)
  (declare (special *callbacks*))
  (push (cons class (subscribe-to class handler (events))) *callbacks*))


(defmethod initialize-instance :around ((this subscriber) &key)
  (with-slots (callbacks) this
    (let ((*callbacks* (list)))
      (declare (special *callbacks*))
      (call-next-method)
      (rplacd callbacks *callbacks*))))


(define-destructor subscriber (callbacks)
  (loop with eve = (events)
     for (class . cb) in (cdr callbacks)
     do (unsubscribe-from class cb eve)))


;;
(defevent player-added ()
  (player))


(defevent game-state-updated ()
  (state timestamp))


(defevent camera-rotated ()
  (ax ay))


(defevent movement-changed ()
  (direction))


(defevent trigger-pulled () ())
