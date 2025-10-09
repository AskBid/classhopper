{-# LANGUAGE RecordWildCards #-}

module Geometry.Curve where

import Geometry.Bernstein
import Geometry.Point (Point3d, ptsSummationE, Parameter)

import Linear.Vector ((*^))

data Curve = Curve
  { uBasisFuncs :: Bernstein
  , _CVs :: [Point3d] 
  }

-- | given basis function and a set of points finds the point 
-- on the curve at a certain parameter u/t.
evaluatePtCrv :: Curve -> Parameter -> Point3d
evaluatePtCrv (Curve{..}) t = ptsSummationE weightedPts 
  where 
    uBF_ts = map ($ t) uBasisFuncs
    weightedPts = zipWith (*^) uBF_ts _CVs

subdivideCrv :: Curve -> Int -> [Point3d]
subdivideCrv crv divisions =
  let 
    divs = toEnum divisions
    params = (/divs) <$> [0..divs]
  in 
    evaluatePtCrv crv <$> params
