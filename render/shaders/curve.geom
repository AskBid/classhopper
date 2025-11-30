#version 330 core

layout(lines) in;
layout(triangle_strip, max_vertices = 4) out;
// "Give me line segments (2 vertices each)"
// "I'll output up to 4 vertices as a triangle strip"
// This makes a quad: vertex 0,1,2,3 forms 2 triangles

uniform vec2 viewportSize;
uniform float thickness;
// The keyword uniform means: "This value comes from outside 
// the shader (from your Haskell code) and stays constant for
// the entire draw call.

void main() {
    // Get the two line endpoints in clip space
    vec4 p0 = gl_in[0].gl_Position;
    vec4 p1 = gl_in[1].gl_Position;
    
    // Convert to normalized device coordinates (NDC)
    vec2 ndc0 = p0.xy / p0.w;
    vec2 ndc1 = p1.xy / p1.w;
    
    // Convert to screen space
    vec2 screen0 = ndc0 * viewportSize * 0.5;
    vec2 screen1 = ndc1 * viewportSize * 0.5;
    
    // Get line direction and normal in screen space
    vec2 dir = normalize(screen1 - screen0);
    vec2 normal = vec2(-dir.y, dir.x);
    
    // Calculate offset in screen space (half thickness on each side)
    vec2 offset = normal * thickness * 0.5;
    
    // Convert offset back to NDC space
    vec2 ndcOffset = offset / (viewportSize * 0.5);
    
    // Emit four vertices to form a quad
    // Bottom-left
    gl_Position = vec4(ndc0 - ndcOffset, p0.z / p0.w, 1.0);
    EmitVertex();
    
    // Top-left
    gl_Position = vec4(ndc0 + ndcOffset, p0.z / p0.w, 1.0);
    EmitVertex();
    
    // Bottom-right
    gl_Position = vec4(ndc1 - ndcOffset, p1.z / p1.w, 1.0);
    EmitVertex();
    
    // Top-right
    gl_Position = vec4(ndc1 + ndcOffset, p1.z / p1.w, 1.0);
    EmitVertex();
    
    EndPrimitive();
}
