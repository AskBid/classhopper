{-# LANGUAGE LambdaCase #-}

module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Geometry.Curve as C
import qualified Geometry.Bezier as B
import qualified Geometry.Surface as S
import qualified Geometry.Point as P
import Geometry.Type

import Linear.V3
import Data.Maybe (fromMaybe)
import Debug.Trace

import Scene

drawing :: Scene -> IO ()
drawing scene = do 
  drawGlobalAxis 
  mapM_ (drawCurve 500) $ crvs scene 
  mapM_ drawSurface $ srfs scene 

isBezier :: S.Surface -> Bool 
isBezier (S.Surface _ _ Bezier Bezier _ _) = True
isBezier _ = False

pathLines :: [P.Point3d] -> IO ()
pathLines [] = return ()
pathLines (V3 x y z :pts) = do 
  GL.vertex $ GL.Vertex3 (x :: GL.GLdouble) (y :: GL.GLdouble) (z :: GL.GLdouble)
  pathLines pts

drawGlobalAxis :: IO ()
drawGlobalAxis = do 
  GL.lineWidth GL.$= 3.0
  GL.renderPrimitive GL.Lines $ do 
      -- X 
    GL.color $ GL.Color3 (1 :: GL.GLdouble) 0 0
    GL.vertex $ GL.Vertex3 0.01  0    (0.00 :: GL.GLdouble)
    GL.vertex $ GL.Vertex3 1.21  0    (0.00 :: GL.GLdouble)
    -- Y 
    GL.color $ GL.Color3 0 (1 :: GL.GLdouble) 0
    GL.vertex $ GL.Vertex3 0     0.01    (0.00 :: GL.GLdouble)
    GL.vertex $ GL.Vertex3 0     1.21    (0.00 :: GL.GLdouble)
    -- Z
    GL.color $ GL.Color3 0 0 (1 :: GL.GLdouble)
    GL.vertex $ GL.Vertex3 0     0     ((-0.01) :: GL.GLdouble)
    GL.vertex $ GL.Vertex3 0     0     ((-1.21) :: GL.GLdouble)
  GL.lineWidth GL.$= 1.0
  
drawCurve :: Int -> C.Curve -> IO ()
drawCurve precision crv = do 
  -- hull
  GL.color $ GL.Color3 (0.4 :: GL.GLdouble) 0.7 0.7
  GL.renderPrimitive GL.LineStrip $
    pathLines $ C.cvs crv
  -- curve
  GL.lineWidth GL.$= 4.0
  GL.color $ GL.Color3 (0.1 :: GL.GLdouble) 0.1 0.4
  GL.renderPrimitive GL.LineStrip $
    pathLines $ C.subdivideCrv crv precision
  GL.lineWidth GL.$= 1.0

drawSurface :: S.Surface -> IO ()
drawSurface srf = do 
    if isBezier srf 
    then GL.color $ GL.Color3 (0.0 :: GL.GLdouble) 0.0 0.0
    else GL.color $ GL.Color3 (0.0 :: GL.GLdouble) 0.7 0.2
    let pts1 = S.subdivideIsocrv S.U 0.0 8 srf 
    GL.lineWidth GL.$= 2.0
    GL.renderPrimitive GL.LineStrip $
      pathLines pts1
    let pts2 = S.subdivideIsocrv S.U 0.5 8 srf 
    drawDashedLine pts2
    let pts3 = S.subdivideIsocrv S.U 1.0 8 srf 
    GL.lineWidth GL.$= 2.0
    GL.renderPrimitive GL.LineStrip $
      pathLines pts3

    let pts4 = S.subdivideIsocrv S.V 0.0 8 srf 
    GL.lineWidth GL.$= 2.0
    GL.renderPrimitive GL.LineStrip $
      pathLines pts4
    let pts5 = S.subdivideIsocrv S.V 0.5 8 srf 
    drawDashedLine pts5
    let pts6 = S.subdivideIsocrv S.V 1.0 8 srf
    GL.lineWidth GL.$= 2.0
    GL.renderPrimitive GL.LineStrip $
      pathLines pts6

    -- let pts7 = S.evaluateSrfCOSs srf
    -- mapM_ drawCOS pts7
  where 
    drawDashedLine :: [P.Point3d] -> IO ()
    drawDashedLine pts = do
      GL.lineStipple GL.$= Just (1, 0x00FF)  -- repeat pattern of 16 bits
      GL.lineWidth GL.$= 1.0
      GL.renderPrimitive GL.LineStrip $ pathLines pts
      GL.lineStipple GL.$= Nothing

drawCOS :: [P.Point3d] -> IO ()
drawCOS pts = do 
  GL.color $ GL.Color3 (1 :: GL.GLdouble) 0.1 0.5
  GL.lineWidth GL.$= 1.0
  GL.renderPrimitive GL.LineStrip $ pathLines pts


