{-# LANGUAGE RecordWildCards #-}

module Render.CV where 

import Linear.V4
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL (($=))

import Render.Common
import Scene (CachedCVS(..))
import Shader.Common
import Render.Color

renderCV 
  :: RenderContext 
  -> CachedCVS 
  -> Float 
  -> Float
  -> Color 
  -> IO ()
renderCV RenderContext{..} CachedCVS{..} radius thickness (Color rgba) = do
  let ShaderProgram prog shadersVariables = rcCVShader
  
  GL.currentProgram $= Just prog
  
  -- Set MVP matrix -> MVP = Model × View × Projection
  case Map.lookup "mvpMatrix" shadersVariables of
    Just locationGpuMem -> setVariableMatrix4fv locationGpuMem rcMVPMatrix
    Nothing -> return ()
  
  -- Set viewport size
  case Map.lookup "viewportSize" shadersVariables of
    Just locationGpuMem -> do
      let (w, h) = rcViewportSize
      GL.uniform locationGpuMem $= 
        GL.Vector2 
          (fromIntegral w :: GL.GLfloat) 
          (fromIntegral h :: GL.GLfloat)
    Nothing -> return ()

  -- Set radius (in pixels)
  case Map.lookup "radius_px" shadersVariables of
    Just locationGpuMem -> 
      GL.uniform locationGpuMem $= (radius :: GL.GLfloat)
    Nothing -> return ()
  
  -- Set thickness (in pixels)
  case Map.lookup "thickness" shadersVariables of
    Just locationGpuMem -> 
      GL.uniform locationGpuMem $= (thickness :: GL.GLfloat)
    Nothing -> return ()
  
  -- Set color
  case Map.lookup "color" shadersVariables of
    Just loc -> do
      let V4 r g b a = rgba
      GL.uniform loc $= GL.Vector4 
        (realToFrac r :: GL.GLfloat) 
        (realToFrac g :: GL.GLfloat) 
        (realToFrac b :: GL.GLfloat) 
        (realToFrac a :: GL.GLfloat)
    Nothing -> return ()
  
  GL.bindVertexArrayObject $= Just ccvVAO
  GL.drawArrays GL.Points 0 ccvVertexCount  -- GL.Points, not GL.LineStrip!
  GL.bindVertexArrayObject $= Nothing
