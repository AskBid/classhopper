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
      windowHint (WindowHint'DepthBits (Just 24))
      window <- createWindow 800 600 "Classhopper 3D" Nothing Nothing
      case window of
        Nothing -> error "Failed to create GLFW window"
        Just win -> do
          makeContextCurrent (Just win)
          swapInterval 1

          -- Check depth buffer bits
          depthBits <- GL.get GL.depthBits
          putStrLn $ "Depth buffer bits: " ++ show depthBits

          -- rotation state
          rotView <- newIORef mkRotationView
          setKeyCallback win (Just (keyHandler rotView))
          
          appLoop win rotView
          destroyWindow win
          terminate

appLoop :: Window -> IORef RotationView -> IO ()
appLoop window rotZRef = do
  shouldClose <- windowShouldClose window
  unless shouldClose $ do
    pollEvents
    GL.clearColor GL.$= GL.Color4 0.43 0.43 0.47 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- Configure depth testing
    GL.depthFunc GL.$= Just GL.Less
    GL.depthMask GL.$= GL.Enabled

    setupOrtho   

    GL.viewport GL.$= (GL.Position 0 0, GL.Size 800 600)
    rotView <- readIORef rotZRef
    setAssonometricView rotView
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

setAssonometricView :: RotationView -> IO ()
setAssonometricView (rotX, _ , rotZ) = do
  GL.loadIdentity
  GL.rotate (45 + rotX) (GL.Vector3 1 0 0 :: GL.Vector3 GL.GLfloat)
  GL.rotate 45   (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat)
  GL.rotate rotZ (GL.Vector3 0 0 1 :: GL.Vector3 GL.GLfloat)

-- Key handler: modify rotation angle
keyHandler :: IORef RotationView 
           -> Window 
           -> Key 
           -> Int 
           -> KeyState 
           -> ModifierKeys 
           -> IO ()
keyHandler rotZRef _win key scancode action _mods = 
  when (action == KeyState'Pressed || action == KeyState'Repeating) $
    case key of
      Key'Right -> modifyIORef' rotZRef (\(x,y,z) -> (x,y,z+5))
      Key'Left  -> modifyIORef' rotZRef (\(x,y,z) -> (x,y,z-5))
      Key'Up    -> modifyIORef' rotZRef (\(x,y,z) -> (x+5,y,z))
      Key'Down  -> modifyIORef' rotZRef (\(x,y,z) -> (x-5,y,z))
      _         -> return ()

type RotationView = (GL.GLfloat, GL.GLfloat, GL.GLfloat)

mkRotationView :: RotationView
mkRotationView = (0.0, 0.0, 0.0)
