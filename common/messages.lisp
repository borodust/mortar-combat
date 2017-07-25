(in-package :mortar-combat.common)


(defmessage ack-message (reply-message))


(defmessage version-request (identified-message))


(defmessage version-response (reply-message)
  version)


(defmessage identification-request (identified-message)
  name)


(defmessage identification-response (reply-message)
  peer-id name)


(defmessage arena-list-request (identified-message))


(defmessage arena-list-response (reply-message)
  arena-list)


(defmessage arena-creation-request (identified-message)
  name)


(defmessage arena-joining-request (identified-message)
  name)


(defmessage arena-exiting-request (identified-message))


(defmessage ping (identified-message))


(defmessage pong (reply-message))


(defmessage relay-request ()
  peer data)


;; errors

(defmessage unavailable-name-error (error-message))


(defmessage (unauthorized-access-error
             (:init :text "Unauthorized access. Identify first."))
    (error-message))


(defmessage already-in-arena-error (error-message))


(defmessage arena-exists-error (error-message))


(defmessage arena-not-found-error (error-message))


(defmessage peer-not-found-error (error-message))
