#version 330 core
in vec2 texCoord;

uniform vec4 color;
uniform float thickness;
uniform float radius_px;

out vec4 fragColor;

void main() {
    float dist = length(texCoord);
    
    // Calculate inner and outer radius in normalized space
    float outerRadius = 1.0;
    float innerRadius = 1.0 - (thickness / radius_px);
    
    // Discard pixels outside the circle
    if (dist > outerRadius) {
        discard;
    }
    
    // Discard pixels inside the inner circle (to create ring)
    if (dist < innerRadius) {
        discard;
    }
    
    // Anti-aliasing on edges
    float edgeWidth = 1.5 / radius_px;
    
    // Smooth outer edge
    float outerAlpha = 1.0 - smoothstep(outerRadius - edgeWidth, outerRadius, dist);
    
    // Smooth inner edge
    float innerAlpha = smoothstep(innerRadius, innerRadius + edgeWidth, dist);
    
    float alpha = outerAlpha * innerAlpha;
    
    fragColor = vec4(color.rgb, color.a * alpha);
}
