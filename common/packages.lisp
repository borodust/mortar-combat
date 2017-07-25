(in-package :mortar-combat.def)


(ge.util:define-package :mortar-combat.common
  (:use :cl :ge.util :ge.net)
  (:export ack-response
           version-request

           version-response
           version-response-version

           identification-request
           identification-response
           identification-response-peer-id
           identification-response-name

           arena-list-request

           arena-list-response
           arena-list-response-arena-list

           arena-creation-request
           arena-joining-request
           arena-exiting-request

           relay-request
           relay-request-data

           ping
           pong

           unavailable-name-error
           unauthorized-access-error
           already-in-arena-error
           arena-exists-error
           arena-not-found-error
           peer-not-found-error))
