(in-package :mortar-combat.proxy)


(defun make-random-uuid ()
  (format nil "~A" (uuid:make-v5-uuid uuid:+namespace-x500+
                                      (ironclad:byte-array-to-hex-string
                                       (ironclad:make-random-salt)))))



(defclass server-channel (channel)
  ((system :initarg :system :initform (error ":system missing"))))
