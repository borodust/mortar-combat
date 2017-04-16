#version 410 core

#include <lighting>

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec3 vNormal;

out gl_PerVertex {
  vec4 gl_Position;
};

out v_PerVertex {
  vec4 color;
};

uniform mat4 modelViewProjection;
uniform mat3 normalTransform;
uniform DirectionalLight dLight;
uniform vec3 baseColor;

void main() {
  color = computeLight(vec4(baseColor, 1.0), normalTransform * vNormal, dLight);
  gl_Position = modelViewProjection * vec4(vPosition, 1.0);
}
