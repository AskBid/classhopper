{-# LANGUAGE RecordWildCards #-}

module Render.Surface where 

import Linear.V4
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL (($=))
import Control.Monad (forM_)

import Render.Common
import Scene (CachedSurface(..), CachedCurve(..))
import Shader.Common
import Render.Curve (renderCurve, renderDashedCurve)
import Render.Color

renderSurfaceBezier
  :: RenderContext 
  -> CachedSurface
  -> Color
  -> IO ()
renderSurfaceBezier ctx CachedSurface{..} color = do
  forM_ csBorders $ \curve -> 
    renderCurve ctx curve 3 color
  forM_ csIsoCrvs $ \curve -> 
    renderDashedCurve ctx curve 1 10 10 color
