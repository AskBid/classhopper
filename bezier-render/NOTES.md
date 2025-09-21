# DSL2 Vs GLFW 

DSL2 is more for multimedia apps, like videogames, but we need only OpenGL.
So GLFW-b is more lightweight solution.

### imgui-hs plain the field

Dear ImGui (available in Haskell via the imgui-hs package) uses OpenGL to render buttons and other UI elements

GLFW lacks built-in support for loading images or fonts, requiring external libraries like JuicyPixels (for images) or freetype2 (for fonts) to create textured buttons or labels in OpenGL. Giving SDL2 a slight advantage here

Dear ImGui changes the equation by providing a ready-made GUI toolkit that renders buttons, text, and other widgets using OpenGL, with built-in font rendering and basic styling. Let’s evaluate how it affects SDL2’s advantage and GLFW’s disadvantage:

### Still slight advatange of SDL not requiring extra image loader

SDL2’s sdl2-image makes this slightly easier by providing a direct API to load images and create textures, while GLFW requires JuicyPixels (or similar) plus manual OpenGL texture setup.

Conclusion: ImGui negates SDL2’s font-loading advantage, as ImGui handles text rendering itself. However, SDL2 retains a slight edge for image-based buttons due to sdl2-image’s simplicity compared to GLFW’s need for an external image loader.

# FRP for Classhopper?

[Reactive-Banana](https://wiki.haskell.org/Reactive-banana)

[Hackge Reactive-banana](https://hackage.haskell.org/package/reactive-banana)
