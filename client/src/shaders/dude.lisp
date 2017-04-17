(in-package :mortar-combat)


(define-shading-program dude-program
  :vertex-shader "dude.v.glsl"
  :fragment-shader "passthru.f.glsl")
