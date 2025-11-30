#version 330 core

out vec4 fragColor;

uniform vec4 color;

void main() {
    fragColor = color;
}

// It only applies the color.
// Can be used for antialiasing later.
