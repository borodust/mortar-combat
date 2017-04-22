(in-package :mortar-combat.common)


(define-constant +ok-reply+ (list :command :ok)
  :test #'equal)


(defmacro with-message ((&rest properties) message &body body)
  `(destructuring-bind (&key ,@properties &allow-other-keys) ,message
     ,@body))


(defgeneric process-command (command message)
  (:method (command message)
    (list :command :error
          :type :unknown-command
          :text "Unknown command")))


(defmethod process-command :around (command message)
  (handler-case
      (when-let ((reply (call-next-method)))
        (nconc (list :reply-for (getf message :message-id)) reply))
    (serious-condition ()
      '(:command :error
        :type :unhandled-error
        :text "Error during command execution"))))


(defun encode-message (message stream)
  (conspack:encode message :stream stream))


(defun decode-message (stream)
  (conspack:decode-stream stream))
