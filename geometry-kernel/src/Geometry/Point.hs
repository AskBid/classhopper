module Geometry.Point where 

import Linear.V3
import Linear.V2 
import Linear.Vector (zero, (^+^), Additive)
import Data.List (foldl')

-- | thought to be the Control Point coordinates
type Point3d = V3 Double

data Point3dW = Point3dW 
  { pt :: Point3d
  , w  :: Double 
  } deriving Show

type PointUV = V2 Double

ptsSummationE :: (Additive f, Num a) => [f a] -> f a
ptsSummationE = foldr (^+^) zero

-- | thought tot be the t paramenter to indicate the location
-- along the curve where we are evaluation the curve. 
-- usually 0 < t < 1
type Parameter = Double

data BBox = BBox 
  { minBBox :: Point3d
  , maxBBox :: Point3d 
  } deriving Show

bboxDiagonal :: BBox -> Double 
bboxDiagonal (BBox (V3 minX minY minZ) (V3 maxX maxY maxZ)) = 
  sqrt (((maxX - minX)^2 + (maxY - minY)^2) + (maxZ - minZ)^2)

chunkPtsBBox :: [Double] -> ([Point3d], BBox)
chunkPtsBBox [] = ([], BBox (V3 0 0 0) (V3 0 0 0))
chunkPtsBBox (x0:y0:z0:rest) = 
  let initialPt = V3 x0 y0 z0
      (pts, bbox) = go rest ([], BBox initialPt initialPt)
  in (initialPt : pts, bbox)
  where
    go :: [Double] -> ([Point3d], BBox) -> ([Point3d], BBox)
    go (x:y:z:rest) (pts, BBox (V3 minX minY minZ) (V3 maxX maxY maxZ)) =
      let pt = V3 x y z
          newMin = V3 (min minX x) (min minY y) (min minZ z)
          newMax = V3 (max maxX x) (max maxY y) (max maxZ z)
          (restPts, finalBBox) = go rest (pts, BBox newMin newMax)
      in (pt : restPts, finalBBox)
    go _ acc = acc
chunkPtsBBox _ = ([], BBox (V3 0 0 0) (V3 0 0 0))
