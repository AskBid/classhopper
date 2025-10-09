{-# LANGUAGE RecordWildCards #-}

module Geometry.COS where

import Geometry.Bernstein
import Geometry.Point

import Linear.Vector ((*^))

data COS = COS
  { uBasisFuncs :: Bernstein
  , _CVs :: [PointUV]
  }

evaluatePtCos :: COS -> Parameter -> PointUV
evaluatePtCos (COS{..}) t = ptsSummationE weightedPts 
  where 
    uBF_ts = map ($ t) uBasisFuncs
    weightedPts = zipWith (*^) uBF_ts _CVs

subdivideCos :: COS -> Int -> [PointUV]
subdivideCos crv divisions =
  let 
    divs = toEnum divisions
    params = (/divs) <$> [0..divs]
  in 
    evaluatePtCos crv <$> params
