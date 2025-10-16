{-# LANGUAGE LambdaCase #-}

module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import qualified Geometry.Curve as C
import qualified Geometry.Bernstein as B
import qualified Geometry.Surface as S
import qualified Geometry.Point as P

import Linear.V3
import Data.Maybe (fromMaybe)

data Scene = Scene 
  { srfs :: [Maybe S.Surface]
  , crvs :: [C.Curve]
  }

srf1 = S.mkSurface 2 2 
      [ -1,-1,0, 0,-1,-1, 1,-1,0 
      , -1,0,1, 0,0,-1, 1,0,1
      , -1,1,0, 0,1,-1, 1,1,0
      ]

scene = Scene 
  { srfs = [srf1]
  , crvs =
      [ C.Curve B.deg2_bfs [ V3 (-1) (-1) 0,   V3 0.0 (-1) (-1), V3 1 (-1) 0   ]
      , C.Curve B.deg2_bfs [ V3 (-1) 0 (-0.5), V3 0.0 0 0.5,     V3 1 0 (-0.5) ]
      , C.Curve B.deg2_bfs [ V3 (-1) 1 (-1),   V3 0.0 1 0,       V3 1 1 (-1)   ]
      ]
  }

drawing :: IO ()
drawing = do 
  drawGlobalAxis
  mapM_ (drawCurve 50) $ crvs scene 
  mapM_ drawSurface $ srfs scene 

pathLines :: [P.Point3d] -> IO ()
pathLines [] = return ()
pathLines (V3 x y z :pts) = do 
  GL.vertex $ GL.Vertex3 (x :: GL.GLfloat) (y :: GL.GLfloat) (z :: GL.GLfloat)
  pathLines pts

drawGlobalAxis :: IO ()
drawGlobalAxis = do 
  GL.lineWidth GL.$= 3.0
  GL.renderPrimitive GL.Lines $ do 
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
  GL.lineWidth GL.$= 1.0
  
drawCurve :: Int -> C.Curve -> IO ()
drawCurve precision crv = do 
  -- hull
  GL.color $ GL.Color3 (0.4 :: GL.GLfloat) 0.7 0.7
  GL.renderPrimitive GL.LineStrip $
    pathLines $ C._CVs crv
  -- curve
  GL.lineWidth GL.$= 4.0
  GL.color $ GL.Color3 (0.1 :: GL.GLfloat) 0.1 0.4
  GL.renderPrimitive GL.LineStrip $
    pathLines $ C.subdivideCrv crv precision
  GL.lineWidth GL.$= 1.0

drawSurface :: Maybe S.Surface -> IO ()
drawSurface = \case 
  Nothing -> return ()
  Just srf -> do 
    GL.color $ GL.Color3 (0.0 :: GL.GLfloat) 0.0 0.0
    let pts1 = S.subdivideIsocrv S.U 0.0 8 srf 
    GL.renderPrimitive GL.LineStrip $
      pathLines pts1
    let pts2 = S.subdivideIsocrv S.U 0.5 8 srf 
    drawDashedLine pts2
    let pts3 = S.subdivideIsocrv S.U 1.0 8 srf 
    GL.renderPrimitive GL.LineStrip $
      pathLines pts3

    let pts4 = S.subdivideIsocrv S.V 0.0 8 srf 
    GL.renderPrimitive GL.LineStrip $
      pathLines pts4
    let pts5 = S.subdivideIsocrv S.V 0.5 8 srf 
    drawDashedLine pts5
    let pts6 = S.subdivideIsocrv S.V 1.0 8 srf 
    GL.renderPrimitive GL.LineStrip $
      pathLines pts6

    let pts7 = S.evaluateSrfCOSs srf
    mapM_ drawCOS pts7
  where 
    drawDashedLine :: [P.Point3d] -> IO ()
    drawDashedLine pts = do
      GL.lineStipple GL.$= Just (1, 0x00FF)  -- repeat pattern of 16 bits
      GL.lineWidth GL.$= 2.0
      GL.renderPrimitive GL.LineStrip $ pathLines pts
      GL.lineStipple GL.$= Nothing

drawCOS :: [P.Point3d] -> IO ()
drawCOS pts = do 
  GL.color $ GL.Color3 (1 :: GL.GLfloat) 0.1 0.5
  GL.lineWidth GL.$= 1.0
  GL.renderPrimitive GL.LineStrip $ pathLines pts


