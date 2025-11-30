#version 330 core

in float vLineDistance;
out vec4 fragColor;

uniform vec4 color;
uniform float dashLength;  // Length of each dash in pixels
uniform float gapLength;   // Length of each gap in pixels

void main() {
    // Calculate position within dash pattern
    float patternLength = dashLength + gapLength;
    float position = mod(vLineDistance, patternLength);
    
    // Discard pixels in gaps
    if (position > dashLength) {
        discard;  // Don't draw this pixel
    }
    
    fragColor = color;
}
