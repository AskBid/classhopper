{-# LANGUAGE RecordWildCards #-}

module Geometry.Curve where

import Geometry.Bernstein
import Geometry.Point (Point, ptProduct, ptsSummationE)

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
evaluatePt (Curve{..}) t = ptsSummationE weightedPts 
  where 
    uBerTs = map ($ t) uCrvBer
    weightedPts = zipWith ptProduct uBerTs crvCVs

subdivideCrv :: Curve -> Int -> [Point]
subdivideCrv crv divisions =
  let 
    divs = toEnum divisions
    params = (/divs) <$> [0..divs]
  in 
    evaluatePt crv <$> params
