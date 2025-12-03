module Shader.Curve where 

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Data.Map as Map
import qualified Data.ByteString as BS

import Scene 
import Shader.Common 
  ( ShaderProgram(..)
  , checkShaderCompile
  , checkProgramLink
  )

-- | Load shader program from files path of the scripts. 
-- it is scoped on Curve only because of the variables/uniforms
-- but otherwise the inner logic can be commonised with 
-- dashedCurve
loadCurveShader 
  :: FilePath 
  -> FilePath 
  -> FilePath 
  -> IO ShaderProgram
loadCurveShader vertPath geomPath fragPath = do
  vertSrc <- BS.readFile vertPath
  geomSrc <- BS.readFile geomPath
  fragSrc <- BS.readFile fragPath

  -- Create shaders
  vertShader <- GL.createShader GL.VertexShader
  geomShader <- GL.createShader GL.GeometryShader
  fragShader <- GL.createShader GL.FragmentShader
  
  -- Compile vertex shader
  GL.shaderSourceBS vertShader $= vertSrc
  GL.compileShader vertShader
  checkShaderCompile vertShader "err: CurveVertex"
  
  -- Compile geometry shader
  GL.shaderSourceBS geomShader $= geomSrc
  GL.compileShader geomShader
  checkShaderCompile geomShader "err: CurveGeometry"
  
  -- Compile fragment shader
  GL.shaderSourceBS fragShader $= fragSrc
  GL.compileShader fragShader
  checkShaderCompile fragShader "err: CurveFragment"
  
  -- Link program
  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program geomShader
  GL.attachShader program fragShader
  GL.linkProgram program
  checkProgramLink program
  
  -- Cleanup shaders (they're linked into program now)
  GL.deleteObjectName vertShader
  GL.deleteObjectName geomShader
  GL.deleteObjectName fragShader
  
  -- Get uniform/shaders-variables locations
  mvpLoc <- GL.uniformLocation program "mvpMatrix"
  viewportLoc <- GL.uniformLocation program "viewportSize"
  thicknessLoc <- GL.uniformLocation program "thickness"
  colorLoc <- GL.uniformLocation program "color"
  
  let shadersVariables = Map.fromList
        [ ("mvpMatrix", mvpLoc)
        , ("viewportSize", viewportLoc)
        , ("thickness", thicknessLoc)
        , ("color", colorLoc)
        ]
  
  return $ ShaderProgram program shadersVariables

loadDashedCurveShader 
  :: FilePath 
  -> FilePath 
  -> FilePath 
  -> IO ShaderProgram
loadDashedCurveShader vertPath geomPath fragPath = do
  vertSrc <- BS.readFile vertPath
  geomSrc <- BS.readFile geomPath
  fragSrc <- BS.readFile fragPath

  -- Create shaders
  vertShader <- GL.createShader GL.VertexShader
  geomShader <- GL.createShader GL.GeometryShader
  fragShader <- GL.createShader GL.FragmentShader
  
  -- Compile vertex shader
  GL.shaderSourceBS vertShader $= vertSrc
  GL.compileShader vertShader
  checkShaderCompile vertShader "err: DashedCurveVertex"
  
  -- Compile geometry shader
  GL.shaderSourceBS geomShader $= geomSrc
  GL.compileShader geomShader
  checkShaderCompile geomShader "err: DashedCurveGeometry"
  
  -- Compile fragment shader
  GL.shaderSourceBS fragShader $= fragSrc
  GL.compileShader fragShader
  checkShaderCompile fragShader "err: DashedCurveFragment"
  
  -- Link program
  program <- GL.createProgram
  GL.attachShader program vertShader
  GL.attachShader program geomShader
  GL.attachShader program fragShader
  GL.linkProgram program
  checkProgramLink program
  
  -- Cleanup shaders (they're linked into program now)
  GL.deleteObjectName vertShader
  GL.deleteObjectName geomShader
  GL.deleteObjectName fragShader
  
  -- Get uniform/shaders-variables locations
  mvpLoc <- GL.uniformLocation program "mvpMatrix"
  viewportLoc <- GL.uniformLocation program "viewportSize"
  thicknessLoc <- GL.uniformLocation program "thickness"
  colorLoc <- GL.uniformLocation program "color"
  dashLengthLoc <- GL.uniformLocation program "dashLength"
  gapLengthLoc <- GL.uniformLocation program "gapLength"
  
  let shadersVariables = Map.fromList
        [ ("mvpMatrix", mvpLoc)
        , ("viewportSize", viewportLoc)
        , ("thickness", thicknessLoc)
        , ("color", colorLoc)
        , ("dashLength", dashLengthLoc)
        , ("gapLength", gapLengthLoc)
        ]
  
  return $ ShaderProgram program shadersVariables
