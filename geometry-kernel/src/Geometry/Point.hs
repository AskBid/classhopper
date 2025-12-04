module Geometry.Point where 

import Linear.V3
import Linear.V2 
import Linear.Vector (zero, (^+^), Additive)

-- | thought to be the Control Point coordinates
type Point3d = V3 Double

data Point3dW = Point3dW 
  { pt :: Point3d
  , w  :: Double 
  }

type PointUV = V2 Double

ptsSummationE :: (Additive f, Num a) => [f a] -> f a
ptsSummationE = foldr (^+^) zero

-- | thought tot be the t paramenter to indicate the location
-- along the curve where we are evaluation the curve. 
-- usually 0 < t < 1
type Parameter = Double
