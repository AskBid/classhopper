{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Map
import Control.Lens
import GHC.Float (double2Float)

-- import qualified Drawing as D
import qualified Scene.Scene as SC
import Scene.Class (GeometryHandle(showHandle))
import Scene.Common 
import Scene.WorldRefs (mkWorldRefs)
import Scene.GLBox -- only to dev for now.

import File.Translate.Iges.Scene (fromIgesSceneToScene)

import Shader.Curve (loadCurveShader, loadDashedCurveShader)
import Shader.CV (loadCVShader)
import Shader.Common (ShaderProgram(..))

import Render.Common 
import Render.Scene 
import Render.CV (renderCV)
import Render.Curve
import Render.Color
import Render.WorldRefs (renderWorldRefs)

import Geometry.Point
import Geometry.Surface
import Geometry.Type

import qualified Geometry.File.TranslatorApp as IGES

newtype RotationView = RotationView (Deg360, Deg360, Deg360)
-- Type wrapper for clarity
newtype Deg360 = Deg360 Float
  deriving (Show, Eq)
-- Extract the float value
floatDegreeRot :: Deg360 -> Float
floatDegreeRot (Deg360 x) = x

-- | ShaderProgram could become plural in the future
data AppState = AppState
  { _asRotationView  :: IORef RotationView
  , _asZoom          :: IORef Float
  , _asScene         :: SC.Scene
  }
makeLenses ''AppState 

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
          zoomRef <- newIORef 1
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

          igesScene <- IGES.openFile fileLocation
          print igesScene
          scene <- fromIgesSceneToScene igesScene

          let geomSrfs = elems $ scene ^. SC.geometrySRFS 
          -- (axes, grid) <- mkWorldRefs 1 100 -- (bboxDiagonal sceneBBox)
              scene' = scene & SC.sceneBox .~ SC.findSceneBBox geomSrfs 

          -------------------------------------------
          -- TEMP start: 
          -- follows some experimental operation
          -- prolly to deldete in production
          -------------------------------------------
          -- take first surface available
          let headSafe [] = 
                ( ObjectId 0
                , SC.GeometrySurface  
                       ( ObjectId 0 ) 
                       ( Surface 
                           [] 
                           [] 
                           Bezier 
                           Bezier 
                           (Irrational [[]]) 
                           [] 
                           []
                           (BBox (V3 0 0 0) (V3 0 0 0))
                       ) 
                )
              headSafe (a:_) = a
              (idNoNeed, oneGeoSrf) = headSafe $ toList $ scene ^. SC.geometrySRFS
          -- show Handle of that surface (adds pts to scene)
          scene'' <- showHandle oneGeoSrf scene'
          --
          -- experiment with showing every surface curve BBox
          -- this inserts boundingBoxes edges in the scene.
          gpuDataBoxes <- cacheBoxes $ getSceneCurves scene''
          let scene''' = scene'' & SC.cachedBoxes .~ gpuDataBoxes -- ?~ does the Just ...
          ------------------------------
          -- // TEMP end.
          ------------------------------

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

          cvShaderProgram <- loadCVShader
            "shaders/curve.vert"
            "shaders/cv.geom"
            "shaders/cv.frag"

          let appState = AppState
                { _asRotationView  = rotViewRef
                , _asZoom          = zoomRef
                , _asScene         = scene'''
                }

              sceneDiagonal = double2Float 
                $ boxDiagonal $ scene''' ^. SC.sceneBox

              renderCtx = RenderContext
                { _rcCurveShader       = curveShaderProgram
                , _rcDashedCurveShader = dashedCurveShaderProgram
                , _rcCVShader          = cvShaderProgram
                , _rcMVPMatrix         = identity
                , _rcViewportSize      = sizeRef
                }

          appLoop win (sceneDiagonal/2) appState renderCtx

          destroyWindow win
          terminate

appLoop :: Window -> Float -> AppState -> RenderContext -> IO ()
appLoop window sceneDiagonal appState renderCtx = do
  shouldClose <- windowShouldClose window
  unless shouldClose $ do
    -- Process all pending window/input events right now, 
    -- and call any callbacks that were registered
    pollEvents
    
    -- clear buffers
    GL.clearColor $= GL.Color4 0.43 0.43 0.47 1
    GL.clear [GL.ColorBuffer, GL.DepthBuffer]

    -- Read current state
    rotView <- readIORef $ appState ^. asRotationView
    zoom <- readIORef $ appState ^. asZoom
    (width, height) <- readIORef $ renderCtx ^. rcViewportSize

    let mvpMatrix = buildMVPMatrix sceneDiagonal rotView zoom width height
        rCtx'     = renderCtx & rcMVPMatrix .~ mvpMatrix
    renderWorldRefs rCtx' 
                    (appState ^. asScene . SC.cachedAxes) 
                    (appState ^. asScene . SC.cachedGrid)
    renderScene rCtx' (appState ^. asScene)
    -------------
    -- TEMP start
    -- to render the bounding boxes of the opengl polylines..
    -- for visualisation purpouses only
    -------------
    mapM_ 
      (\crv -> renderCurve rCtx' GL.Lines crv 1 yellowBox) 
      (appState ^. asScene . SC.cachedBoxes) 
    -------------
    -- TEMP end.
    -------------

    swapBuffers window
    appLoop window sceneDiagonal appState rCtx'

-- Update viewport when window is resized
adjustViewportAndProjection 
  :: IORef (Int, Int) -> Window -> Int -> Int -> IO ()
adjustViewportAndProjection sizeRef _win winW winH = do
  -- Update the viewport (OpenGL's rendering area)
  let windowSize = GL.Size (fromIntegral winW) (fromIntegral winH)
  GL.viewport $= (GL.Position 0 0, windowSize)
  -- Store new window size (will be used next frame to rebuild MVP)
  writeIORef sizeRef (winW, winH)

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

buildMVPMatrix :: Float -> RotationView -> Float -> Int -> Int -> M44 Float
buildMVPMatrix bboxDiagonal rotation zoom width height = 
  model !*! view !*! projection
  where
    ratioWoH = toEnum width / toEnum height :: Float
    dimension = bboxDiagonal * zoom :: Float
    wOrth = dimension
    hOrth = dimension / ratioWoH
    -- zRange = max wOrth hOrth
    projection = ortho (-wOrth) wOrth (-hOrth) hOrth (-6000) 6000
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

scrollHandler 
  :: IORef Float 
  -> Window 
  -> Double 
  -> Double 
  -> IO ()
scrollHandler zoomRef _ _ yoffset = do
  let zoomFactor = 1.1 ** realToFrac yoffset  -- 10% per scroll notch
  modifyIORef' zoomRef $ \currentZoom -> 
    max 0.01 (min 100.0 (currentZoom * zoomFactor))

----------
-- TEMP --
----------
fileLocation :: FilePath
fileLocation = "../file-translator/sample/1srf_normal_trimmed.igs"
-- fileLocation = "../file-translator/sample/laferrari.igs"
-- fileLocation = "../file-translator/sample/ClaireGoldsmithSunGlasses.igs"
-- fileLocation = "../file-translator/sample/acefaceGlasses.igs"
-- fileLocation = "../file-translator/sample/smokesensor.igs"
-- fileLocation = "../file-translator/iges-examples/hp/firstSubD_trimmed_model.igs"
-- fileLocation = "../file-translator/iges-examples/irrational_revolve.igs"
-- fileLocation = "../file-translator/iges-examples/rational_revolve.igs"
-- fileLocation = "../file-translator/iges-examples/rational_revolve2.igs"
-- fileLocation = "../file-translator/iges-examples/A-pill_Classhopper.igs"
-- fileLocation = "../file-translator/iges-examples/saddle.igs"
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
