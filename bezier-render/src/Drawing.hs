module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Geometry.Curve as SPline
import qualified Geometry.Bernstein as B
import qualified Geometry.Surface as S
import qualified Geometry.Point as P
import Data.Maybe (fromMaybe)

drawing :: IO ()
drawing = do 
  drawGlobalAxis
  drawCurve [(-1, -1, -1), (0.0, -1, 0), (1, -1, -1)] 104
  drawCurve [(-1, 0, -0.5), (0.0, 0, 0.5), (1, 0, -0.5)] 104
  drawCurve [(-1, 1, -1), (0.0, 1, 0), (1, 1, -1)] 104
  drawIsocrvs

pathLines :: [P.Point] -> IO ()
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
  
drawCurve :: [P.Point] -> Int -> IO ()
drawCurve pts precision = do 
  -- hull
  GL.color $ GL.Color3 (0.5 :: GL.GLfloat) 0.5 0.5
  GL.renderPrimitive GL.LineStrip $
    pathLines pts
  -- curve
  GL.color $ GL.Color3 (1 :: GL.GLfloat) 1 1
  GL.renderPrimitive GL.LineStrip $
    pathLines $ SPline.subdivideCrv (SPline.Curve B.deg2_bfs pts) precision

drawIsocrvs :: IO ()
drawIsocrvs = do 
  let srf = S.mkSurface 1 2 
        [ -1,0,0, 1,0,0
        , -1,1,1, 1,1,1
        , -1,2,0, 1,2,0
        ]
  let pts = S.subdivideIsocrv S.U 0.0 8 <$> srf 
  GL.renderPrimitive GL.LineStrip $
    pathLines $ fromMaybePts pts
  let pts = S.subdivideIsocrv S.U 0.5 8 <$> srf 
  GL.renderPrimitive GL.LineStrip $
    pathLines $ fromMaybePts pts
  let pts = S.subdivideIsocrv S.U 1.0 8 <$> srf 
  GL.renderPrimitive GL.LineStrip $
    pathLines $ fromMaybePts pts

  let pts = S.subdivideIsocrv S.V 0.0 8 <$> srf 
  GL.renderPrimitive GL.LineStrip $
    pathLines $ fromMaybePts pts
  let pts = S.subdivideIsocrv S.V 0.5 8 <$> srf 
  GL.renderPrimitive GL.LineStrip $
    pathLines $ fromMaybePts pts
  let pts = S.subdivideIsocrv S.V 1.0 8 <$> srf 
  GL.renderPrimitive GL.LineStrip $
    pathLines $ fromMaybePts pts


fromMaybePts :: Maybe [P.Point] -> [P.Point]
fromMaybePts Nothing = [(0.0,0,0), (1.0,1,1)]
fromMaybePts (Just pts) = pts
