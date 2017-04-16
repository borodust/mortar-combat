#version 410 core

#include <lighting>
#include <skinning>

layout(location = 0) in vec3 vPosition;
layout(location = 1) in vec3 vNormal;

layout(location = 2) in vec4 vWeights;
layout(location = 3) in ivec4 vBones;


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
  mat4 weightedTransform = weightedTransform(vBones, vWeights);
  color = computeLight(vec4(baseColor, 1.0),
                       normalTransform * mat3(weightedTransform) * vNormal,
                       dLight);
  gl_Position = modelViewProjection * weightedTransform * vec4(vPosition, 1.0);
}
