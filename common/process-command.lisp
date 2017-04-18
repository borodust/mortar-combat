(in-package :mortar-combat.common)


(defgeneric process-command (command message)
  (:method (command message)
    (list :command :error
          :type :unknown-command
          :text "Unknown command")))


(defmethod process-command :around (command message)
  (append (list :reply-for (getf message :message-id))
          (handler-case
              (call-next-method)
            (serious-condition ()
              '(:command :error
                :type :unhandled-error
                :text "Error during command execution")))))


(defun encode-message (message stream)
  (conspack:encode message :stream stream))


(defun decode-message (stream)
  (conspack:decode-stream stream))
