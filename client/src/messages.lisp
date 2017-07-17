(in-package :mortar-combat)


(defmessage server-shot-info ()
  player-name)


(defmessage server-hit-info ()
  player-name)


(defmessage register-player ()
  name)


(defmessage player-info ()
  name
  position
  rotation
  timestamp
  movement)


(defmessage shot-info ()
  name)


(defmessage game-state ()
  timestamp
  state)
