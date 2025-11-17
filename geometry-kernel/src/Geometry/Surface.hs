{-# LANGUAGE RecordWildCards #-}

module Geometry.Surface where

import Geometry.Type (Knots, BasisFunc, Degree)
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

data DirectionUV = U | V

data Surface 
  = Bezier  BaseData 
  | BSpline BaseData (Maybe Knots) (Maybe Knots)

-- | notice we use Bernstein both for Bezier and BSpline 
-- although to be precise for BSpilne should be called Basis Funcionts
data BaseData = BaseData
  { uBasisFuncs :: [BasisFunc]
  , vBasisFuncs :: [BasisFunc]
  , _CVs :: [[Point3d]]
  , _COS :: [COS]
  }

instance Show BaseData where 
  show (BaseData _ _ pts _) = 
    "Surface:\n" ++ unlines (map (("  " ++) . show) pts)

-- From a list of Double, construct a point for every 3 Double, and builds a 
-- surface if there are enough points to comply with the given U and V degrees.
-- for each direction, if knots are given calculates the BasisFunc with CoxDeBoor.
-- TODO : COS is just a dummy pre-defined COS. Not building it from inputs yet.
mkBezier
  :: Degree 
  -> Degree 
  -> [Double] 
  -> Maybe Surface
mkBezier degU degV cvs = do 
  baseData <- BaseData 
                <$> bernsteinSelector degU
                <*> bernsteinSelector degV
                <*> uRows
                <*> cos
  pure $ Bezier baseData
  where
    pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 cvs
    uRows
      | length pts >= 4 = Just $ take (degV + 1) $ chunk (degU + 1) pts
      | otherwise       = Nothing
    -- ^ Notice it can NOT exist a surface with less than 4 points (plane)
    cos = Just [COS deg2_bfs [V2 0 0.5, V2 0.5 0.8, V2 1 0.5]]

filterBezier :: Surface -> Maybe Surface 
filterBezier (Bezier basedata)                  = Just (Bezier basedata)
filterBezier (BSpline basedata Nothing Nothing) = Just (Bezier basedata)
filterBezier _ = Nothing

mkBSpline
  :: Degree 
  -> Knots 
  -> Degree 
  -> Knots 
  -> [Double] 
  -> Maybe Surface
mkBSpline pU ktsU pV ktsV coords = do
  baseData <- BaseData 
                <$> getBasisFuncs pU ktsU'
                <*> getBasisFuncs pV ktsV'
                <*> uRowsOfPts
                <*> cos
  pure $ BSpline baseData ktsU' ktsV'
  where
    pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 coords

    mU = length ktsU - 1
    mV = length ktsV - 1
    nU1 = mU - pU
    nV1 = mV - pV

    ktsU' = multispan ktsU 
    ktsV' = multispan ktsV
    
    uRowsOfPts -- :: Maybe [[Point3d]]
      | length pts >= 4 = 
          if length matrix == nV1 
          then Just matrix 
          else Nothing
      | otherwise = Nothing
      where 
        matrix = take nV1 $ chunk nU1 pts
    -- ^ Notice it can NOT exist a surface with less than 
    -- 4 points (plane).
    cos = Just [COS deg2_bfs [V2 0 0.5, V2 0.5 0.8, V2 1 0.5]]



-- mkBSpline :: Degree -> Degree -> [Double] -> (Knots, Knots) -> Maybe Surface
-- mkBSpline degU degV cvs (ktsU, ktsV) = undefined 
--   where 
--     basisFuncsU = coxDeBoor uDeg kts 

evaluateSrfPt :: BaseData -> PointUV -> Point3d
evaluateSrfPt (BaseData{..}) (V2 u v) = evaluateSrfPt' _CVs uBF_ts vBF_ts 
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
--
evaluateSrfPt' :: [[Point3d]] -> [Double] -> [Double] -> Point3d
evaluateSrfPt' srfCVs uBerTs_i vBerTs_j  = ptsSummationE $ concat weightedPts
  where 
    parseUrow vBerT_j = zipWith (*^) ((* vBerT_j) <$> uBerTs_i) 
    -- ^ feed Row of Pts i
    weightedPts = zipWith parseUrow vBerTs_j srfCVs

subdivideIsocrv :: DirectionUV -> Parameter -> Int -> BaseData -> [Point3d]
subdivideIsocrv uvDir t divisions (BaseData{..}) = case uvDir of
  U -> evaluateSrfPt' _CVs (map ($ t) uBasisFuncs) <$> paramsBerTs vBasisFuncs
  V -> flip (evaluateSrfPt' _CVs) (map ($ t) vBasisFuncs) <$> paramsBerTs uBasisFuncs
  where 
    div = toEnum divisions
    params :: [Double]
    params = (/div) <$> [0..div]
    paramsBerTs :: [BasisFunc] -> [[Double]]
    paramsBerTs uORvBasisFuncs = map (\t -> map ($ t) uORvBasisFuncs) params

-- | evaluates one COS.
evaluateCOS :: BaseData -> Int -> COS -> [Point3d]
evaluateCOS srf divisions cos = evaluateSrfPt srf <$> uvPts
  where
    uvPts = subdivideCos cos divisions

-- | evaluates all COSs on a Surface.
evaluateSrfCOSs :: BaseData -> [[Point3d]]
evaluateSrfCOSs srf = evaluateCOS srf 16 <$> _COS srf
