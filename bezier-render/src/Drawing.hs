module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Curve.BersteinBFs as SPline

drawing :: IO ()
drawing = do 
    GL.renderPrimitive GL.LineStrip $
      pathLines $ SPline.evaluateCrv [(-0.5, -0.5), (0.0, 0.5), (0.5, 0.0)] 3
    GL.renderPrimitive GL.LineStrip $
      pathLines $ SPline.evaluateCrv [(-0.5, -0.5), (0.0, 0.5), (0.5, 0.0)] 14

pathLines :: [SPline.Point] -> IO ()
pathLines []          = return ()
pathLines ((x,y):pts) = do 
  GL.vertex $ GL.Vertex2 (x :: GL.GLfloat) (y :: GL.GLfloat)
  pathLines pts
