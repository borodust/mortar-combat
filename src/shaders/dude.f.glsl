#version 410 core

out vec4 fColor;

in v_PerVertex {
  vec4 color;
};

void main() {
  fColor = color;
}
