{-# LANGUAGE RecordWildCards #-}

module Render.WorldRefs where 

import Linear.V4
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL (($=))

import Render.Common
import Scene.WorldRefs
import Shader.Common
import Render.Color
import Render.Curve
import Render.Scene (setupOpenGLtransparent, setupOpenGLopaque)

renderWorldRefs
  :: RenderContext 
  -> Axes 
  -> Grid 
  -> IO ()
renderWorldRefs ctx Axes{..} (Grid lines)  = do
  renderCurve ctx GL.Lines xWorld 3 redGrid 
  renderCurve ctx GL.Lines yWorld 3 greenGrid 
  renderCurve ctx GL.Lines zWorld 3 blueGrid 
  setupOpenGLtransparent
  renderCurve ctx GL.Lines lines 1 greyGrid 
  setupOpenGLopaque
