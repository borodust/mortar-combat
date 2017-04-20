(in-package :mortar-combat)


;; fixme: don't forget to make thread safe for generalization
(defclass keymap (lockable)
  ((callbacks :initform nil)
   (cursor-action :initform nil)
   (key-table :initform (make-hash-table :test 'eq))))


(defun enable-keymap (keymap)
  (with-slots (callbacks cursor-action key-table) keymap
    (when callbacks
      (error "Keymap already enabled"))
    (let ((eve (events)))
      (flet ((register-callback (class action)
               (push (cons class (subscribe-to class action eve)) callbacks))
             (process-key-event (ev)
               (when-let ((action (gethash (key-from ev) key-table)))
                 (funcall action (state-from ev))))
             (process-button-event (ev)
               (when-let ((action (gethash (button-from ev) key-table)))
                 (funcall action (state-from ev))))
             (process-cursor-event (ev)
               (when cursor-action
                 (funcall cursor-action (x-from ev) (y-from ev)))))
        (register-callback 'keyboard-event #'process-key-event)
        (register-callback 'mouse-event #'process-button-event)
        (register-callback 'cursor-event #'process-cursor-event)))))


(defun disable-keymap (keymap)
  (with-slots (callbacks) keymap
    (unless callbacks
      (error "Keymap already disabled"))
    (loop with eve = (events)
       for (class . cb) in callbacks
       do (unsubscribe-from class cb eve))
    (setf callbacks nil)))


(defun bind-button (keymap button action)
  (with-slots (key-table) keymap
    (setf (gethash button key-table) action)))


(defun bind-cursor (keymap action)
  (with-slots (cursor-action) keymap
    (setf cursor-action action)))
