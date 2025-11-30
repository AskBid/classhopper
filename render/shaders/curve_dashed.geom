#version 330 core

layout(lines) in;
layout(triangle_strip, max_vertices = 4) out;

uniform vec2 viewportSize;
uniform float thickness;

out float vLineDistance;  // NEW: distance along the line

void main() {
    vec4 p0 = gl_in[0].gl_Position;
    vec4 p1 = gl_in[1].gl_Position;
    
    vec2 ndc0 = p0.xy / p0.w;
    vec2 ndc1 = p1.xy / p1.w;
    
    vec2 screen0 = ndc0 * viewportSize * 0.5;
    vec2 screen1 = ndc1 * viewportSize * 0.5;
    
    // Calculate line length in screen space
    float lineLength = length(screen1 - screen0);
    
    vec2 dir = normalize(screen1 - screen0);
    vec2 normal = vec2(-dir.y, dir.x);
    
    vec2 offset = normal * thickness * 0.5;
    vec2 ndcOffset = offset / (viewportSize * 0.5);
    
    // Bottom-left (start of line)
    gl_Position = vec4(ndc0 - ndcOffset, p0.z / p0.w, 1.0);
    vLineDistance = 0.0;  // Start
    EmitVertex();
    
    // Top-left
    gl_Position = vec4(ndc0 + ndcOffset, p0.z / p0.w, 1.0);
    vLineDistance = 0.0;
    EmitVertex();
    
    // Bottom-right (end of line)
    gl_Position = vec4(ndc1 - ndcOffset, p1.z / p1.w, 1.0);
    vLineDistance = lineLength;  // End
    EmitVertex();
    
    // Top-right
    gl_Position = vec4(ndc1 + ndcOffset, p1.z / p1.w, 1.0);
    vLineDistance = lineLength;
    EmitVertex();
    
    EndPrimitive();
}
