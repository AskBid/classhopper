{-# LANGUAGE OverloadedStrings #-}

module Interface where

import SDL
import Control.Monad (unless)
import qualified Graphics.Rendering.OpenGL as GL
import SDL.Video.OpenGL
import qualified Curve.BersteinBFs as SPline
-- import Graphics.Rendering.OpenGL.GL.BeginEnd
-- import Graphics.Rendering.OpenGL (PolygonMode(Line))

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

  -- TODO: OpenGL drawing code goes here
  GL.renderPrimitive GL.Lines $
    pathLines $ SPline.evaluateCrv [(-0.5, -0.5), (0.0, 0.5), (0.5, 0.0)] 4
  GL.renderPrimitive GL.Lines $
    pathLines $ SPline.evaluateCrv [(-0.5, -0.5), (0.0, 0.5), (0.5, 0.0)] 14
  -- GL.renderPrimitive GL.Lines $ do 
  --   GL.vertex $ GL.Vertex2 (-0.5 :: GL.GLfloat) (-0.5 :: GL.GLfloat)
  --   GL.vertex $ GL.Vertex2 (0.0 :: GL.GLfloat) (0.6 :: GL.GLfloat)
  --   GL.vertex $ GL.Vertex2 (0.1 :: GL.GLfloat) (0.7 :: GL.GLfloat)
  --   GL.vertex $ GL.Vertex2 (0.5 :: GL.GLfloat) (0.5 :: GL.GLfloat)

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

pathLines :: [SPline.Point] -> IO ()
pathLines []          = return ()
pathLines ((x,y):pts) = do 
  GL.vertex $ GL.Vertex2 (x :: GL.GLfloat) (y :: GL.GLfloat) 
  pathLines' pts
  where 
    pathLines' [] = pathLines []
    pathLines' [(x,y)] = GL.vertex $ GL.Vertex2 (x :: GL.GLfloat) (y :: GL.GLfloat)
    pathLines' ((x,y):pts) = do 
      GL.vertex $ GL.Vertex2 (x :: GL.GLfloat) (y :: GL.GLfloat)
      GL.vertex $ GL.Vertex2 (x :: GL.GLfloat) (y :: GL.GLfloat)
      pathLines' pts
