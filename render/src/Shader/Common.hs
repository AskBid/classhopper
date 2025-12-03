module Shader.Common where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Data.Map as Map
import Control.Monad (unless)

data ShaderProgram = ShaderProgram
  { spProgram  :: GL.Program
  , spUniforms :: Map.Map String GL.UniformLocation
  }

-- | Checks if compilation succeeded: GL.compileStatus shader
-- If it failed: Gets the error log from OpenGL.
checkShaderCompile :: GL.Shader -> String -> IO ()
checkShaderCompile shader name = do
  ok <- GL.get $ GL.compileStatus shader
  unless ok $ do
    log <- GL.get $ GL.shaderInfoLog shader
    error $ name ++ " shader compilation failed:\n" ++ log

-- | checks if linking the shaders together into 
-- a program succeeded.
checkProgramLink :: GL.Program -> IO ()
checkProgramLink prog = do
  ok <- GL.get $ GL.linkStatus prog
  unless ok $ do
    log <- GL.get $ GL.programInfoLog prog
    error $ "Program linking failed:\n" ++ log

