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
          -- Sets the callback to use when the framebuffer's size changes. 
          -- See glfwSetFramebufferSizeCallback
          setFramebufferSizeCallback win (Just adjustViewportAndProjection)

          makeContextCurrent (Just win)
          swapInterval 1

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

    (width, height) <- getFramebufferSize window
    adjustViewportAndProjection window width height
    setupOrtho width height

    rotView <- readIORef rotZRef
    setAssonometricView rotView
    D.drawing

    swapBuffers window
    appLoop window rotZRef

setupOrtho :: Int -> Int -> IO ()
setupOrtho width height = do
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

-- keep a fixed orthographic "world" box, and compute a centered viewport that 
-- preserves that box's aspect
adjustViewportAndProjection :: Window -> Int -> Int -> IO ()
adjustViewportAndProjection _ winW winH = do
  let w = max 1 winW
      h = max 1 winH
      -- define your fixed "world" extents here (currently -2..2 in both 
      -- axes -> width=4, height=4 -> aspect = 1)
      worldW = 4.0 :: Double
      worldH = 4.0 :: Double
      worldAspect = worldW / worldH
      winWf = fromIntegral w :: Double
      winHf = fromIntegral h :: Double
      winAspect = winWf / winHf

      (vpW, vpH)
        | winAspect >= worldAspect = (round (winHf * worldAspect), round winHf)
        | otherwise                = (round winWf, round (winWf / worldAspect))
      vpX = (w - vpW) `div` 2
      vpY = (h - vpH) `div` 2

  -- set the centered viewport (letterbox)
  GL.viewport GL.$= (GL.Position (fromIntegral vpX) (fromIntegral vpY),
                     GL.Size (fromIntegral vpW) (fromIntegral vpH))
