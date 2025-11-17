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

# GPipe

Would have been nice to use, but it turns out that it is not mantained and it was giving difficulties with more updated packages.

Without this you have to use OpenGL package which is basically an haskelly wrapper on top of the 1:1 OpengGL bindings that go under the name of OpenGLRaw.

In the end you decided to go this way as (OpenGL) as you don't need doing much anyway, so there are little advantages over using typesafe GPipe. And you can extend to OpenGLRaw for more specific not covered tasks.. this way also allows you to go OpenGL 4.x rather than 3.3 and build your own shaders for meshing NURBS surfaces. As that is of course not covered from the retrograde GPipe.

Of course with OpenGL package everything is still thought as a C++ program, so little Haskell like despite the wrapper .. but in the end there is no escape and you can always build on top of it your own haskell like definitions perhaps have you own GPipe one day.
