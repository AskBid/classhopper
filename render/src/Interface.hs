{-# LANGUAGE OverloadedStrings #-}

module Interface where

import Graphics.UI.GLFW
import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Prelude hiding (init)
import Data.IORef
import Control.Monad.Writer (runWriterT)
import Data.Functor.Identity (Identity(..), runIdentity)

import qualified Drawing as D
import qualified Scene as SC
import OpenFile (openIGES, fromIgesSceneToScene)

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
              
          igesScene <- openIGES fileLocation

          scene <- fromIgesSceneToScene igesScene

          setupOrtho 800 600 -- initial ortho cube
          appLoop win scene rotView 

          destroyWindow win
          terminate

appLoop :: Window -> SC.Scene -> IORef RotationView -> IO ()
appLoop window scene rotZRef = do
  shouldClose <- windowShouldClose window
  unless shouldClose $ do
    pollEvents
    GL.clearColor GL.$= GL.Color4 0.43 0.43 0.47 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- Configure depth testing
    GL.depthFunc $= Just GL.Less
    GL.depthMask $= GL.Enabled 

    rotView <- readIORef rotZRef
    setAssonometricView rotView

    D.render scene

    swapBuffers window
    appLoop window scene rotZRef

setupOrtho :: Int -> Int -> IO ()
setupOrtho width height = do
  let ratio = fromIntegral width / fromIntegral height
      zoom = 0.1
      wOrth = fromIntegral width * zoom -- zoom * ratio / 2
      hOrth = fromIntegral height * zoom -- zoom * ratio / 2 
  GL.matrixMode $= GL.Projection
  GL.loadIdentity
  -- left, right, bottom, top, near, far
  GL.ortho (-wOrth) wOrth (-hOrth) hOrth (-4000) 4000
  GL.matrixMode $= GL.Modelview 0
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

-- keep a fixed orthographic "world" box, and compute a centered 
-- viewport that preserves that box's aspect. 
adjustViewportAndProjection :: Window -> Int -> Int -> IO ()
adjustViewportAndProjection _ winW winH = do
  setupOrtho winW winH
  GL.viewport $= ( GL.Position 0 0
                 , GL.Size (fromIntegral winW) (fromIntegral winH)
                 )


fileLocation :: FilePath
-- fileLocation = "../file-translator/iges-examples/A-pill_Classhopper.igs"
fileLocation = "../file-translator/iges-examples/saddle.igs"
-- fileLocation = "../file-translator/iges-examples/NegativeEdgeFix_WiP_220913.igs"
-- fileLocation = "../file-translator/iges-examples/4Classhopper_trimmed.igs"
-- fileLocation = "../file-translator/iges-examples/A-Pill_fillet_srfs_fromRhino.igs"
-- fileLocation = "../file-translator/iges-examples/hp/1srf_5spansU.igs"
-- fileLocation = "../file-translator/iges-examples/hp/1srf_5spansU8V_trimmed.igs"
-- fileLocation = "../file-translator/iges-examples/saddlenostitches.igs"
-- fileLocation = "../file-translator/iges-examples/saddle_stitch2.igs"
-- fileLocation = "../file-translator/iges-examples/saddle_selected_errors.igs"
-- fileLocation = "../file-translator/iges-examples/saddle_selected_errors2.igs"
-- fileLocation = "../file-translator/iges-examples/saddle_selected_errors3.igs"
-- fileLocation = "../file-translator/iges-examples/saddle_stitch.igs"
-- fileLocation = "../file-translator/iges-examples/saddle_errors.igs"
-- fileLocation = "../file-translator/iges-examples/hp/1srf_normal_trimmed.igs"
-- fileLocation = "../file-translator/iges-examples/221110_Previous IM lights.igs"
