#version 330 core
layout(points) in;
layout(triangle_strip, max_vertices = 4) out;

uniform vec2 viewportSize;
uniform float radius_px;

out vec2 texCoord;

void main() {
    vec4 center = gl_in[0].gl_Position;
    
    // Convert pixel radius to NDC space
    vec2 pixelSize = 2.0 / viewportSize;
    vec2 offset = radius_px * pixelSize;
    
    // Bottom-left
    gl_Position = center + vec4(-offset.x, -offset.y, 0.0, 0.0);
    texCoord = vec2(-1.0, -1.0);
    EmitVertex();
    
    // Bottom-right
    gl_Position = center + vec4(offset.x, -offset.y, 0.0, 0.0);
    texCoord = vec2(1.0, -1.0);
    EmitVertex();
    
    // Top-left
    gl_Position = center + vec4(-offset.x, offset.y, 0.0, 0.0);
    texCoord = vec2(-1.0, 1.0);
    EmitVertex();
    
    // Top-right
    gl_Position = center + vec4(offset.x, offset.y, 0.0, 0.0);
    texCoord = vec2(1.0, 1.0);
    EmitVertex();
    
    EndPrimitive();
}
