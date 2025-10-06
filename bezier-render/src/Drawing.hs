module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Geometry.Curve as SPline
import qualified Geometry.Bernstein as B

drawing :: IO ()
drawing = do 
  drawGlobalAxis
  drawCurve [(-1, -1, -1), (0.0, -1, 0), (1, -1, -1)] 104
  drawCurve [(-1, 0, -0.5), (0.0, 0, 0.5), (1, 0, -0.5)] 104
  drawCurve [(-1, 1, -1), (0.0, 1, 0), (1, 1, -1)] 104

pathLines :: [SPline.Point] -> IO ()
pathLines []          = return ()
pathLines ((x,y,z):pts) = do 
  GL.vertex $ GL.Vertex3 (x :: GL.GLfloat) (y :: GL.GLfloat) (z :: GL.GLfloat)
  pathLines pts

drawGlobalAxis :: IO ()
drawGlobalAxis = GL.renderPrimitive GL.Lines $ do 
  -- X 
  GL.color $ GL.Color3 (1 :: GL.GLfloat) 0 0
  GL.vertex $ GL.Vertex3 0.01  0    (0.00 :: GL.GLfloat)
  GL.vertex $ GL.Vertex3 0.21  0    (0.00 :: GL.GLfloat)
  -- Y 
  GL.color $ GL.Color3 0 (1 :: GL.GLfloat) 0
  GL.vertex $ GL.Vertex3 0     0.01    (0.00 :: GL.GLfloat)
  GL.vertex $ GL.Vertex3 0     0.21    (0.00 :: GL.GLfloat)
  -- Z
  GL.color $ GL.Color3 0 0 (1 :: GL.GLfloat)
  GL.vertex $ GL.Vertex3 0     0     ((-0.01) :: GL.GLfloat)
  GL.vertex $ GL.Vertex3 0     0     ((-0.21) :: GL.GLfloat)
  
drawCurve :: [SPline.Point] -> Int -> IO ()
drawCurve pts precision = do 
  -- hull
  GL.color $ GL.Color3 (0.5 :: GL.GLfloat) 0.5 0.5
  GL.renderPrimitive GL.LineStrip $
    pathLines pts
  -- curve
  GL.color $ GL.Color3 (1 :: GL.GLfloat) 1 1
  GL.renderPrimitive GL.LineStrip $
    pathLines $ SPline.subdivideCrv (SPline.Curve B.deg2_bfs pts) precision
