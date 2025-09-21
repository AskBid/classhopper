{-# LANGUAGE OverloadedStrings #-}

module Interface where

import SDL
import Control.Monad (unless)
import qualified Graphics.Rendering.OpenGL as GL
import SDL.Video.OpenGL
-- import Graphics.Rendering.OpenGL.GL.BeginEnd
-- import Graphics.Rendering.OpenGL (PolygonMode(Line))
import qualified Drawing as D

launchWindow :: IO ()
launchWindow = do 
  initializeAll

  let defOpenGL = defaultOpenGL { glProfile = Core Normal 3 3 }

  window <- createWindow "Classhopper 3D"
    defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL }
  -- window it’s just a blank OS-managed surface.
  glContext <- glCreateContext window
  glMakeCurrent window glContext
  -- renderer <- createRenderer window (-1) defaultRenderer 
  -- renderer is the actual drawing entity.
  appLoop window
  destroyWindow window

appLoop :: Window -> IO ()
appLoop window = do
  events <- pollEvents
  let quitRequested = any (isQuit . eventPayload) events

  -- Rendering step
  GL.clearColor GL.$= GL.Color4 0.43 0.43 0.47 1
  GL.clear [GL.ColorBuffer]

  -- OpenGL drawing code goes here
  D.drawing

  glSwapWindow window 
  -- OpenGL don’t draw directly to the screen. Uses two buffers:
  -- Back buffer → where you draw the current frame off-screen.
  -- Front buffer → what the user is currently seeing.
  -- You do all your glClear, glDrawArrays, glDrawElements, etc. into the back buffer.
  -- glSwapWindow swap the back and front buffers.

  unless quitRequested (appLoop window)

isQuit :: EventPayload -> Bool
isQuit e =
  case e of
    QuitEvent -> True
    KeyboardEvent keyboardEvent ->
      keyboardEventKeyMotion keyboardEvent == Pressed &&
      keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
    _ -> False

