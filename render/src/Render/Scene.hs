{-# LANGUAGE RecordWildCards #-}

module Render.Scene where 

import Control.Monad (forM_)
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

import Render.Curve
import Render.Surface
import Render.CV
import Render.Common
import Render.Color
import Scene

renderScene :: RenderContext -> Scene -> IO ()
renderScene ctx Scene{..} = do
  setupOpenGLopaque
  forM_ cachedSRFS $ \srf -> 
    renderSurfaceBezier ctx srf black
  forM_ cachedCRVS $ \crv -> 
    renderCurve ctx crv 1.5 blue
  forM_ cachedCVS $ \cvs -> 
    renderCV ctx cvs 10 4 blue

-- | This sets up **global OpenGL rendering STATE**. 
-- to use before shading transaprent objects (surfaces mesh)
setupOpenGLtransparent :: IO ()
setupOpenGLtransparent = do
  GL.blend $= GL.Enabled
  -- ^ **Enables alpha blending** - allows transparency.
  -- without it everything would be opaque. it is basically saying 
  -- that we should use a blending function to see how to draw the 
  -- pixel
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  -- **Defines HOW to blend transparent objects** with what's 
  -- already on screen.
  -- The formula is:
  -- finalColor = 
  --   sourceColor * SrcAlpha + destColor * (1 - SrcAlpha)
  -- sourceColor: The new pixel you're drawing
  -- destColor: What's already on the screen
  -- SrcAlpha: The alpha (transparency) of the new pixel
  GL.depthMask $= GL.Disabled 
  -- ^ writes depth if Enabled, so that every draw is going to 
  -- occlude something that will have less of a depth than them

-- | This sets up **global OpenGL rendering STATE**. 
-- to use before shading opque object (curves always, surfaces depends)
setupOpenGLopaque :: IO ()
setupOpenGLopaque = do
  GL.blend $= GL.Disabled
  GL.depthMask $= GL.Enabled
