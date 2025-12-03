-- {-# DEPRECATED messao #-}
module Drawing where 

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import qualified Geometry.Curve as C
import qualified Geometry.Bezier as B
import qualified Geometry.Surface as S
import qualified Geometry.Point as P
import Geometry.Type

-- import Linear.V3
-- import Data.Maybe (fromMaybe)
-- import Data.Map (toList)
-- import Control.Monad (forM_, mapM_)
--
-- import Scene
--
-- render :: Scene -> IO ()
-- render scene = do 
--   GL.clear [GL.ColorBuffer, GL.DepthBuffer]
--   renderSurfaces scene
--   -- renderCurves scene
--   return ()
--
-- renderSurfaces :: Scene -> IO ()
-- renderSurfaces scene = do
--   let cachedSrfs = toList $ cachedSRFS scene
--   mapM_ (renderSurface . snd) cachedSrfs
--
-- renderSurface :: CachedSurface -> IO ()
-- renderSurface CachedSurface{..} = do
--   forM_ csBorders $ \CachedCurve{..} -> do
--     GL.bindVertexArrayObject $= Just ccVAO
--     GL.drawArrays GL.LineStrip 0 ccVertexCount
--   GL.bindVertexArrayObject $= Nothing

isBezier :: S.Surface -> Bool 
isBezier (S.Surface _ _ Bezier Bezier _ _) = True
isBezier _ = False

-- drawGlobalAxis :: IO ()
--   GL.lineWidth GL.$= 3.0
--       -- X 
--     GL.color $ GL.Color3 (1 :: GL.GLdouble) 0 0
--     GL.vertex $ GL.Vertex3 0.01  0    (0.00 :: GL.GLdouble)
--     GL.vertex $ GL.Vertex3 1.21  0    (0.00 :: GL.GLdouble)
--     -- Y 
--     GL.color $ GL.Color3 0 (1 :: GL.GLdouble) 0
--     GL.vertex $ GL.Vertex3 0     0.01    (0.00 :: GL.GLdouble)
--     GL.vertex $ GL.Vertex3 0     1.21    (0.00 :: GL.GLdouble)
--     -- Z
--     GL.color $ GL.Color3 0 0 (1 :: GL.GLdouble)
--     GL.vertex $ GL.Vertex3 0     0     ((-0.01) :: GL.GLdouble)
--     GL.vertex $ GL.Vertex3 0     0     ((-1.21) :: GL.GLdouble)
  
-- drawCurve :: Int -> C.Curve -> IO ()
--   -- hull
--   GL.lineWidth GL.$= 1.0
--   GL.color $ GL.Color3 (0.4 :: GL.GLdouble) 0.7 0.7
--   -- curve
--   GL.lineWidth GL.$= 4.0
--   GL.color $ GL.Color3 (0.1 :: GL.GLdouble) 0.1 0.4

-- drawSurface :: S.Surface -> IO ()
--     if isBezier srf 
--     then GL.color $ GL.Color3 (0.0 :: GL.GLdouble) 0.0 0.0
--     else GL.color $ GL.Color3 (0.0 :: GL.GLdouble) 0.7 0.2
--     let pts1 = S.subdivideIsocrv S.U 0.0 8 srf 
--     GL.lineWidth GL.$= 2.0

-- drawCOS :: [P.Point3d] -> IO ()
--   GL.color $ GL.Color3 (1 :: GL.GLdouble) 0.1 0.5
--   GL.lineWidth GL.$= 1.0



