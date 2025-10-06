module Geometry.Curve where

import Geometry.Bernstein

-- | thought to be the Control Point coordinates
type Point = (Float, Float, Float)

-- | thought tot be the t paramenter to indicate the location
-- along the curve where we are evaluation the curve. 
-- usually 0 < t < 1
type Parameter = Float

data Curve = Curve
  { uCrvBer :: Bernstein
  , crvCVs  :: [Point] 
  }

-- | given basis function and a set of points finds the point 
-- on the curve at a certain parameter u/t.
evaluatePt :: Curve -> Parameter -> Point
evaluatePt (Curve uBerstein cvs) t = tplSum weightedPts 
  where 
    tuBerstein = map ($ t) uBerstein
    tplProduct t (x, y, z) = (x*t, y*t, z*t)
    weightedPts = zipWith tplProduct tuBerstein cvs
    tplSum = foldr (\(x,y,z) (sx,sy,sz) -> (x+sx, y+sy, z+sz)) (0,0,0)

subdivideCrv :: Curve -> Int -> [Point]
subdivideCrv crv divisions =
  let 
    params = (/toEnum divisions) <$> [0..toEnum divisions]
  in 
    evaluatePt crv <$> params
