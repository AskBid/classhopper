module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Curve.BersteinBFs as SPline

drawing :: IO ()
drawing = do 
  -- GL.color $ GL.Color3 (1 :: GL.GLfloat) 1 1
  GL.renderPrimitive GL.LineStrip $
    pathLines $ SPline.evaluateCrv [(-0.5, -0.7), (0.0, 0.7), (0.5, 0.0)] 3
  GL.renderPrimitive GL.LineStrip $
    pathLines $ SPline.evaluateCrv [(-0.5, -0.7), (0.0, 0.7), (0.5, 0.0)] 104

  GL.renderPrimitive GL.Lines $ do 
    GL.lineWidth GL.$= 3
    GL.color $ GL.Color3 (1 :: GL.GLfloat) 0 0
    GL.vertex $ GL.Vertex3 0 0 (0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex3 0.1 0 (0  :: GL.GLfloat)
    
    GL.color $ GL.Color3 0 (1 :: GL.GLfloat) 0
    GL.vertex $ GL.Vertex3 0 0 (0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex3 0 0.1 (0 :: GL.GLfloat)
    
    GL.color $ GL.Color3 0 0 (1 :: GL.GLfloat)
    GL.vertex $ GL.Vertex3 0 0 (0 :: GL.GLfloat)
    GL.vertex $ GL.Vertex3 0 0 (0.1 :: GL.GLfloat)

    GL.color $ GL.Color3 1 (1 :: GL.GLfloat) 1
    GL.lineWidth GL.$= 1
    

pathLines :: [SPline.Point] -> IO ()
pathLines []          = return ()
pathLines ((x,y):pts) = do 
  GL.vertex $ GL.Vertex3 (x :: GL.GLfloat) (y :: GL.GLfloat) 0
  pathLines pts

