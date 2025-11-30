{-# LANGUAGE RecordWildCards #-}

module Geometry.Surface where

import Geometry.Type (Knots, BasisFunc, Degree, ParamRep)
import Geometry.Bezier (bernsteinSelector, deg2_bfs)
import Geometry.Curve
import Geometry.COS
import Geometry.Point
import Geometry.Helper
import Geometry.BSpline (coxDeBoorUnsafe, getBasisFuncs)
import Geometry.Knot (multispan)

import Linear.Vector ((*^))
import Linear.V3
import Linear.V2
import Data.Maybe (isJust)
import Prelude hiding (cos)
import Debug.Trace

data DirectionUV = U | V

data Surface = Surface
  { uBasisFuncs :: [BasisFunc]
  , vBasisFuncs :: [BasisFunc]
  , uRep        :: ParamRep
  , vRep        :: ParamRep
  , cvs         :: [[Point3d]]
  , cos         :: [COS]
  }

instance Show Surface where 
  show Surface{..} = 
    "Surface:\n" ++ unlines (map (("  " ++) . show) cvs)

-- From a list of Double, construct a point for every 3 Double, and builds a 
-- surface if there are enough points to comply with the given U and V degrees 
-- and UV knot vectors.
-- For each direction, @multispan@ understands if a direction needs the BasisFunc
-- calculation with @CoxDeBoor@. @getBasisFuncs@ dispatches the calculation based
-- on that @ParamRep@ value.
-- TODO : COS is just a dummy pre-defined COS. Not building it from inputs yet.
mkBSpline
  :: Degree 
  -> Knots 
  -> Degree 
  -> Knots 
  -> [Double] 
  -> Maybe Surface
mkBSpline pU ktsU pV ktsV coords =

  let
    pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 coords

    mU = length ktsU - 1
    mV = length ktsV - 1
    nU1 = mU - pU
    nV1 = mV - pV

    ktsUrep = multispan ktsU 
    ktsVrep = multispan ktsV
    
    uRowsOfPts                     -- :: Maybe [[Point3d]]
      | trace (show $ length pts >= 4) length pts >= 4 =          -- 4 points (plane).
          if length matrix == nV1 
          then Just matrix 
          else Nothing
      | otherwise = Nothing
      where 
        matrix = take nV1 $ chunk nU1 pts
    
    cos = Just [COS deg2_bfs [V2 0 0.5, V2 0.5 0.8, V2 1 0.5]]

  in 
    Surface <$> getBasisFuncs pU ktsUrep
            <*> getBasisFuncs pV ktsVrep
            <*> Just ktsUrep
            <*> Just ktsVrep
            <*> uRowsOfPts
            <*> cos

evaluateSrfPt :: Surface -> PointUV -> Point3d
evaluateSrfPt (Surface{..}) (V2 u v) = evaluateSrfPt' cvs uBF_ts vBF_ts 
  where
    uBF_ts = map ($ u) uBasisFuncs
    vBF_ts = map ($ v) vBasisFuncs 

-- ------------------------
-- |     |  i=0  i=1  i=2 |
-- |-----|-----------------
-- | j=0 | P_00 P_10 P_20 |
-- | j=1 | P_01 P_11 P_21 |
-- ------------------------
-- sumE_ij (B_i * B_j * P_ij)
evaluateSrfPt' :: [[Point3d]] -> [Double] -> [Double] -> Point3d
evaluateSrfPt' srfCVs uBerTs_i vBerTs_j  = ptsSummationE $ concat weightedPts
  where 
    parseUrow vBerT_j = zipWith (*^) ((* vBerT_j) <$> uBerTs_i) 
    -- ^ feed Row of Pts i
    weightedPts = zipWith parseUrow vBerTs_j srfCVs

sampleIsocrv :: DirectionUV -> Parameter -> Int -> Surface -> [Point3d]
sampleIsocrv uvDir t divisions (Surface{..}) = case uvDir of
  U -> evaluateSrfPt' cvs (map ($ t) uBasisFuncs) <$> paramsBerTs vBasisFuncs
  V -> flip (evaluateSrfPt' cvs) (map ($ t) vBasisFuncs) <$> paramsBerTs uBasisFuncs
  where 
    div = toEnum divisions
    params :: [Double]
    params = (/div) <$> [0..div]
    paramsBerTs :: [BasisFunc] -> [[Double]]
    paramsBerTs uORvBasisFuncs = map (\t -> map ($ t) uORvBasisFuncs) params

-- | evaluates one COS.
evaluateCOS :: Surface -> Int -> COS -> [Point3d]
evaluateCOS srf divisions cos = evaluateSrfPt srf <$> uvPts
  where
    uvPts = subdivideCos cos divisions

-- | evaluates all COSs on a Surface.
evaluateSrfCOSs :: Surface -> [[Point3d]]
evaluateSrfCOSs srf = evaluateCOS srf 16 <$> cos srf
