{-# LANGUAGE RecordWildCards #-}

module Render.Surface where 

import Linear.V4
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (forM_)

import Render.Common
import Scene.GPU
import Shader.Common
import Render.Curve (renderCurve, renderDashedCurve)
import Render.Color

renderSurfaceBezier
  :: RenderContext 
  -> CachedSurface
  -> Color
  -> IO ()
renderSurfaceBezier ctx surface color = do 
  forM_ (surface ^. borders) $ \curve -> 
    renderCurve ctx GL.LineStrip curve 3 color
  forM_ (surface ^. isos) $ \curve -> 
    renderDashedCurve ctx curve 1 10 10 color
