module Render.Common where 

import Graphics.GL.Compatibility33 (glUniformMatrix4fv)
import qualified Graphics.Rendering.OpenGL as GL
import Linear.Matrix (M44)
import Linear.V4
import Foreign.Marshal.Array (withArray)

import Shader.Common (ShaderProgram)

data RenderContext = RenderContext
  { rcShaderProgram :: ShaderProgram
  , rcMVPMatrix     :: M44 Float
  , rcViewportSize  :: (Int, Int)
  }

setVariableMatrix4fv :: GL.UniformLocation -> M44 Float -> IO ()
setVariableMatrix4fv locGpuMem mat = do
  withArray (matrixToList mat) $ \ptr ->
    glUniformMatrix4fv (unwrapUniLocToGLint locGpuMem) 1 0 ptr
  -- ^ creates a C-style array for GPU memory.
  where
    matrixToList 
      ( V4 
        (V4 a b c d) 
        (V4 e f g h) 
        (V4 i j k l) 
        (V4 m n o p)
      ) = [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p]
    unwrapUniLocToGLint (GL.UniformLocation x) = x
