{-# LANGUAGE RecordWildCards #-}

module Geometry.Curve where

import Geometry.Type (BasisFunc, Degree, Knots, ParamRep(..))
import Geometry.BSpline (getBasisFuncs)
import Geometry.Point (Point3d, ptsSummationE, Parameter)
import Geometry.Knot (multispan)
import Geometry.Helper (chunk)

import Linear.V3
import Linear.Vector ((*^))
import GHC.IO.Exception (cannotCompactMutable)

data Curve = Curve
  { uBasisFuncs :: [BasisFunc]
  , uRep        :: ParamRep
  , cvs         :: [Point3d]
  }

instance Show Curve where
  show Curve{..} =
    "Cruve:\n" ++ unlines (map (("  " ++) . show) cvs)

-- | given basis function and a set of points finds the point 
-- on the curve at a certain parameter t (or called u sometimes)
evaluatePtCrv :: Curve -> Parameter -> Point3d
evaluatePtCrv (Curve{..}) t = ptsSummationE weightedPts 
  where 
    uBF_ts = map ($ t) uBasisFuncs
    weightedPts = zipWith (*^) uBF_ts cvs

sampleCrv :: Curve -> Int -> [Point3d]
sampleCrv crv divisions =
  let 
    divs = toEnum divisions
    params = (/divs) <$> [0..divs]
  in 
    evaluatePtCrv crv <$> params

-- | takes in either a bezier or bspline and sorts them while 
-- creating them.
mkBSpline :: Degree -> Knots -> [Double] -> Maybe Curve
mkBSpline p kts coords = 
  let
    pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 coords
    m = length kts - 1 
    n1 = m - p 
    kts' = multispan kts
  in Curve <$> getBasisFuncs p kts'
           <*> Just kts'
           <*> if length pts == n1 
               then Just pts 
               else Nothing
