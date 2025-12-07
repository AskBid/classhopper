{-# LANGUAGE RecordWildCards #-}

module Render.Curve where 

import Linear.V4
import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Graphics.Rendering.OpenGL (($=))

import Render.Common
import Scene.GPU (CachedCurve(..))
import Shader.Common
import Render.Color

data LineRenderType = Lines | LinesStrip

renderCurve 
  :: RenderContext 
  -> GL.PrimitiveMode
  -> CachedCurve 
  -> Float 
  -> Color 
  -> IO ()
renderCurve RenderContext{..} mode CachedCurve{..} thickness (Color rgba) = do
  let ShaderProgram prog shadersVariables = rcCurveShader
  
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
  
  -- Draw as line strip
  GL.bindVertexArrayObject $= Just ccVAO -- binds / open
  GL.drawArrays mode 0 ccVertexCount
  GL.bindVertexArrayObject $= Nothing    -- unbinds / close


renderDashedCurve 
  :: RenderContext 
  -> CachedCurve 
  -> Float 
  -> Float 
  -> Float
  -> Color 
  -> IO ()
renderDashedCurve RenderContext{..} 
                  CachedCurve{..} 
                  thickness 
                  dashLength 
                  gapLength 
                  (Color rgba) = do
  let ShaderProgram prog shadersVariables = rcDashedCurveShader
  
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
  
  -- Set thickness (in pixels)
  case Map.lookup "thickness" shadersVariables of
    Just locationGpuMem -> 
      GL.uniform locationGpuMem $= (thickness :: GL.GLfloat)
    Nothing -> return ()

  case Map.lookup "dashLength" shadersVariables of
    Just locationGpuMem -> 
      GL.uniform locationGpuMem $= (dashLength :: GL.GLfloat)
    Nothing -> return ()

  case Map.lookup "gapLength" shadersVariables of
    Just locationGpuMem -> 
      GL.uniform locationGpuMem $= (gapLength :: GL.GLfloat)
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
  
  -- Draw as line strip
  GL.bindVertexArrayObject $= Just ccVAO -- binds / open
  GL.drawArrays GL.LineStrip 0 ccVertexCount
  GL.bindVertexArrayObject $= Nothing    -- unbinds / close
