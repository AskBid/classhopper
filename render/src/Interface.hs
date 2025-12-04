{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Interface where

import Graphics.UI.GLFW
import Control.Monad (unless, when)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import Prelude hiding (init)
import Data.IORef
import Control.Monad.Writer (runWriterT)
import Data.Functor.Identity (Identity(..), runIdentity)
import Linear.Matrix (M44, identity, (!*!), mkTransformation)
import Linear.V3 
import Linear.Projection (ortho)
import Linear.Quaternion (Quaternion, axisAngle)

-- import qualified Drawing as D
import qualified Scene as SC
import OpenFile (openIGES, fromIgesSceneToScene)
import Shader.Curve (loadCurveShader, loadDashedCurveShader)
import Shader.Common (ShaderProgram(..))
import Render.Common (RenderContext(..))
import Render.Scene

-- | ShaderProgram could become plural in the future
data AppState = AppState
  { asCurveShader       :: ShaderProgram
  , asDashedCurveShader :: ShaderProgram
  , asRotationView  :: IORef RotationView
  , asZoom          :: IORef Float
  , asWindowSize    :: IORef (Int, Int)
  , asScene         :: SC.Scene
  }

-- | launches windows and initializes OpenGL, AppState and loop.
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
          
          -- IORefs
          rotViewRef <- newIORef mkAxonometricView
          zoomRef <- newIORef 0.1
          sizeRef <- newIORef (800, 600)
          
          -- CallBacks
          setKeyCallback win (Just (keyHandler rotViewRef))
          -- ^ Purpose: Tells GLFW which function to call whenever a keyboard 
          -- key event happens.
          -- win → the GLFW window you’re attaching the callback to.
          setFramebufferSizeCallback win (Just $ adjustViewportAndProjection sizeRef)
          -- Sets the callback to use when the framebuffer (window)'s size changes. 
          setScrollCallback win (Just $ scrollHandler zoomRef)
          -- Handles mouse wheel / trackpad scroll events.

          igesScene <- openIGES fileLocation
          scene <- fromIgesSceneToScene igesScene

          GL.depthFunc $= Just GL.Less
          -- ^ **Enables depth testing** - objects closer to the camera 
          -- cover objects farther away. @Less@ is the type of formula used.

          GL.viewport $= (GL.Position 0 0, GL.Size 800 600)
          -- set initial viewport (the GL canvas).
          setupOpenGLopaque

          curveShaderProgram <- loadCurveShader 
            "shaders/curve.vert"
            "shaders/curve.geom"
            "shaders/curve.frag"

          dashedCurveShaderProgram <- loadDashedCurveShader
            "shaders/curve.vert"
            "shaders/curve_dashed.geom"
            "shaders/curve_dashed.frag"

          let appState = AppState
                { asCurveShader       = curveShaderProgram
                , asDashedCurveShader = dashedCurveShaderProgram
                , asRotationView  = rotViewRef
                , asZoom          = zoomRef
                , asWindowSize    = sizeRef
                , asScene         = scene
                }

          appLoop win appState

          destroyWindow win
          terminate

appLoop :: Window -> AppState -> IO ()
appLoop window AppState{..} = do
  shouldClose <- windowShouldClose window
  unless shouldClose $ do
    -- Process all pending window/input events right now, 
    -- and call any callbacks that were registered
    pollEvents
    
    -- clear buffers
    GL.clearColor $= GL.Color4 0.43 0.43 0.47 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- Read current state
    rotView <- readIORef asRotationView
    zoom <- readIORef asZoom
    (width, height) <- readIORef asWindowSize

    let mvpMatrix = buildMVPMatrix rotView zoom width height
        ctx = RenderContext
                { rcCurveShader = asCurveShader
                , rcDashedCurveShader = asDashedCurveShader 
                , rcMVPMatrix = mvpMatrix
                , rcViewportSize = (width, height)
        }

    renderScene ctx asScene

    swapBuffers window
    appLoop window AppState{..}

-- Update viewport when window is resized
adjustViewportAndProjection :: IORef (Int, Int) -> Window -> Int -> Int -> IO ()
adjustViewportAndProjection sizeRef _win winW winH = do
  -- Update the viewport (OpenGL's rendering area)
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral winW) (fromIntegral winH))
  -- Store new window size (will be used next frame to rebuild MVP)
  writeIORef sizeRef (winW, winH)

newtype RotationView = RotationView (Deg360, Deg360, Deg360)
-- Type wrapper for clarity
newtype Deg360 = Deg360 Float
  deriving (Show, Eq)
-- Extract the float value
floatDegreeRot :: Deg360 -> Float
floatDegreeRot (Deg360 x) = x

-- | Creates a Quaternion which is a rotation rappresentation that 
-- calculates better with computation and the OpenGL stuff.
buildRotation :: RotationView -> Quaternion Float
buildRotation (RotationView (Deg360 rotX, Deg360 rotY, Deg360 rotZ)) = 
  rotZQuat * rotYQuat * rotXQuat
  where
    toRadians degrees = degrees * pi / 180
    rotXQuat = axisAngle (V3 1 0 0) (toRadians rotX)
    rotYQuat = axisAngle (V3 0 1 0) (toRadians rotY)
    rotZQuat = axisAngle (V3 0 0 1) (toRadians rotZ)
    -- axisAngle creates a Quaternion from an angle rotation.
    -- Quaternion w (V3 x y z) : compact way to represent 3D rotation.

mkAxonometricView :: RotationView
mkAxonometricView = RotationView (Deg360 45, Deg360 0, Deg360 45)

mkTopDownView :: RotationView
mkTopDownView = RotationView (Deg360 0, Deg360 0, Deg360 0)

mkFrontView :: RotationView
mkFrontView = RotationView (Deg360 90, Deg360 0, Deg360 0)

buildMVPMatrix :: RotationView -> Float -> Int -> Int -> M44 Float
buildMVPMatrix rotation zoom width height = 
  model !*! view !*! projection
  where
    ratioWoH = toEnum width / toEnum height :: Float
    dimension = 100 * zoom :: Float
    wOrth = dimension
    hOrth = dimension / ratioWoH
    -- zRange = max wOrth hOrth
    projection = ortho (-wOrth) wOrth (-hOrth) hOrth (-4000) 4000
    view = mkTransformation (buildRotation rotation) (V3 0 0 0)
    model = identity
    -- Model: Identity (no per-object transform)
    -- don't move scale rotate.. as all is already in world coordinates

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
      Key'Right -> modifyIORef' rotZRef rotateRight
      Key'Left  -> modifyIORef' rotZRef rotateLeft
      Key'Up    -> modifyIORef' rotZRef rotateUp
      Key'Down  -> modifyIORef' rotZRef rotateDown
      _         -> return ()
  where 
    rotateRight (RotationView (x, y, Deg360 z)) = 
      RotationView (x, y, Deg360 (z+5))
    rotateLeft (RotationView (x, y, Deg360 z)) = 
      RotationView (x, y, Deg360 (z-5))
    rotateUp (RotationView (Deg360 x, y, z)) = 
      RotationView (Deg360 (x+5), y, z)
    rotateDown (RotationView (Deg360 x, y, z)) = 
      RotationView (Deg360 (x-5), y, z)

scrollHandler :: IORef Float -> Window -> Double -> Double -> IO ()
scrollHandler zoomRef _ _ yoffset = modifyIORef zoomRef (+ realToFrac yoffset)


----------
-- TEMP
----------
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
