#version 330 core
// layout specifies how data is organized/connected 
// between shader stages or from your CPU code.
// The data at attribute location 0 in my VBO should 
// be read as a vec3 called 'position'"
layout(location = 0) in vec3 position;

uniform mat4 mvpMatrix;

void main() {
    gl_Position = mvpMatrix * vec4(position, 1.0);
}

// 1. Take the vertex position from the VBO
// 2. Transform it by the MVP matrix (model-view-
//    projection)
// 3. Pass it along to the next stage (geometry shader)
//
// It's **not modifying the geometry at all**
// just transforming from world space to clip space 
// (standard camera transformation).

// Input: Raw 3D positions from your VBO 
// Output: Transformed positions ready for 
//         screen projection
// No geometry change: Still the same points, 
// just in a different coordinate system

