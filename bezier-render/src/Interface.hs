{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Graphics.UI.GLFW
import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import Prelude hiding (init)
import Data.IORef

import qualified Drawing as D

launchWindow :: IO ()
launchWindow = do
  -- Initialize GLFW
  initSuccess <- init
  if not initSuccess
    then error "Failed to initialize GLFW"
    else do
      window <- createWindow 800 600 "Classhopper 3D" Nothing Nothing
      case window of
        Nothing -> error "Failed to create GLFW window"
        Just win -> do
          makeContextCurrent (Just win)
          swapInterval 1

          -- rotation state
          rotZ <- newIORef 0.0
          setKeyCallback win (Just (keyHandler rotZ))
          
          appLoop win rotZ
          destroyWindow win
          terminate

appLoop :: Window -> IORef GL.GLfloat -> IO ()
appLoop window rotZRef = do
  shouldClose <- windowShouldClose window
  unless shouldClose $ do
    pollEvents
    GL.clearColor GL.$= GL.Color4 0.43 0.43 0.47 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    setupOrtho   

    GL.viewport GL.$= (GL.Position 0 0, GL.Size 800 600)
    rotZ <- readIORef rotZRef
    setAssonometricView rotZ
    D.drawing

    swapBuffers window
    appLoop window rotZRef

setupOrtho :: IO ()
setupOrtho = do
  GL.matrixMode GL.$= GL.Projection
  GL.loadIdentity
  -- left, right, bottom, top, near, far
  GL.ortho (-2) 2 (-2) 2 (-2) 2
  GL.matrixMode GL.$= GL.Modelview 0
  GL.loadIdentity

setAssonometricView :: GL.GLfloat -> IO ()
setAssonometricView rotZ = do
  GL.loadIdentity
  GL.rotate 45 (GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat)
  GL.rotate 45 (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat)
  GL.rotate rotZ (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat)

-- Key handler: modify rotation angle
keyHandler :: IORef GL.GLfloat -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyHandler rotZRef _win key scancode action _mods = 
  when (action == KeyState'Pressed || action == KeyState'Repeating) $
    case key of
      Key'Right -> modifyIORef' rotZRef (+5)
      Key'Left  -> modifyIORef' rotZRef (subtract 5)
      _         -> return ()
