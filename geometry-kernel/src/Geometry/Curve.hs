{-# LANGUAGE RecordWildCards #-}

module Geometry.Curve where

import Geometry.Type (BasisFunc, Degree, Knots)
import Geometry.BSpline (getBasisFuncs)
import Geometry.Point (Point3d, ptsSummationE, Parameter)
import Geometry.Knot (multispan)

import Linear.Vector ((*^))
import GHC.IO.Exception (cannotCompactMutable)

data Curve 
  = Bezier  BaseData
  | BSpline BaseData (Maybe Knots)

data BaseData = BaseData
  { uBasisFuncs :: [BasisFunc]
  , _CVs :: [Point3d] 
  }

-- | given basis function and a set of points finds the point 
-- on the curve at a certain parameter u/t.
evaluatePtCrv :: Curve -> Parameter -> Point3d
evaluatePtCrv (_ {..}) t = ptsSummationE weightedPts 
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

mkBSpline :: Degree -> Knots -> [Double] -> Maybe Curve
mkBSpline p kts coords = do 
  baseData <- BaseData <$> getBasisFuncs p kts' <*> pts
  case kts' of 
    Nothing -> pure $ BSpline
    Just kts -> coxDeBoorUnsafe p kts 
  where 
    pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 coords
    m = length kts - 1 
    n1 = p - m 
    kts' = multispan kts
