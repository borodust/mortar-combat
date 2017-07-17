(in-package :mortar-combat.def)


(ge.util:define-package :mortar-combat.common
  (:use :cl :ge.util :ge.net)
  (:export ack-response
           version-request
           version-response
           identification-request
           identification-response
           arena-list-request
           arena-list-response
           arena-creation-request
           arena-joining-request
           arena-exiting-request
           relay-request

           unavailable-name-error
           unauthorized-access-error
           already-in-arena-error
           arena-exists-error
           arena-not-found-error
           peer-not-found-error))
