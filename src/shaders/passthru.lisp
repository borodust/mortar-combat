(in-package :mortar-combat)


(define-shading-program passthru-program
  :vertex-shader "passthru.v.glsl"
  :fragment-shader "passthru.f.glsl")
