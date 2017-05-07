(in-package :mortar-combat)


;;
(defevent player-added ()
  (player))


(defevent game-state-updated ()
  (state timestamp))


(defevent camera-rotated ()
  (ax ay))


(defevent movement-changed ()
  (player direction))


(defevent trigger-pulled ()
  (player))


(defevent exit-requested () ())


(defevent new-arena-requested ()
  (name))


(defevent arena-join-requested ()
  (name))


(defevent arena-leave-requested () ())


(defevent hit-detected ()
  (player))
