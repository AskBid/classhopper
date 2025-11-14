{-# LANGUAGE RecordWildCards #-}

module Geometry.Surface where

import Geometry.Bernstein
import Geometry.Curve
import Geometry.COS
import Geometry.Point
import Geometry.Helper

import Linear.Vector ((*^))
import Linear.V3
import Linear.V2

data DirectionUV = U | V

data Surface = Surface
  { uBasisFuncs :: Bernstein
  , vBasisFuncs :: Bernstein
  , _CVs :: [[Point3d]]
  , _COS :: [COS]
  }  

instance Show Surface where 
  show (Surface _ _ pts _) = 
    "Surface:\n" ++ unlines (map (("  " ++) . show) pts)

-- From a list of Float construct a point for every 3 Float, and builds a surface
-- if there are enough points to comply with the given U and V degrees.
mkSurface :: Int -> Int -> [Double] -> Maybe Surface
mkSurface uDeg vDeg cvs = 
  Surface <$> bernsteinSelector uDeg <*> bernsteinSelector vDeg <*> validSrf uRows <*> cos
  where
    pts :: [Point3d]
    pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 cvs
    uRows :: Maybe [[Point3d]]
    uRows
      | length pts >= 4 = Just $ chunk (uDeg + 1) pts
      | otherwise       = Nothing
    -- ^ Notice that there can't exist a surface with less than 4 points (the plane)
    validSrf Nothing = Nothing
    validSrf (Just uRows') 
      | length uRows' > vDeg = Just $ take (vDeg + 1) uRows'
      | otherwise            = Nothing
    -- ^ Once we found all possible U hulls, we take only the one for vDeg if possible.
    cos = Just [COS deg2_bfs [V2 0 0.5, V2 0.5 0.8, V2 1 0.5]]

evaluateSrfPt :: Surface -> PointUV -> Point3d
evaluateSrfPt (Surface{..}) (V2 u v) = evaluateSrfPt' _CVs uBF_ts vBF_ts 
  where
    uBF_ts = map ($ u) uBasisFuncs
    vBF_ts = map ($ v) vBasisFuncs 

-- | i=0  i=1  i=2 
-- | P_00 P_10 P_20 j=0
-- | P_01 P_11 P_21 j=1
-- summationE_ij : B_i * B_j * P_ij
evaluateSrfPt' :: [[Point3d]] -> [Double] -> [Double] -> Point3d
evaluateSrfPt' srfCVs uBerTs_i vBerTs_j  = ptsSummationE $ concat weightedPts
  where 
    parseUrow vBerT_j = zipWith (*^) ((* vBerT_j) <$> uBerTs_i) 
    -- ^ feed Row of Pts i
    weightedPts = zipWith parseUrow vBerTs_j srfCVs

subdivideIsocrv :: DirectionUV -> Parameter -> Int -> Surface -> [Point3d]
subdivideIsocrv uvDir t divisions (Surface{..}) = case uvDir of
  U -> evaluateSrfPt' _CVs (map ($ t) uBasisFuncs) <$> paramsBerTs vBasisFuncs
  V -> flip (evaluateSrfPt' _CVs) (map ($ t) vBasisFuncs) <$> paramsBerTs uBasisFuncs
  where 
    div = toEnum divisions
    params :: [Double]
    params = (/div) <$> [0..div]
    paramsBerTs :: Bernstein -> [[Double]]
    paramsBerTs uORvBasisFuncs = map (\t -> map ($ t) uORvBasisFuncs) params

evaluateCOS :: Surface -> Int -> COS -> [Point3d]
evaluateCOS srf divisions cos = evaluateSrfPt srf <$> uvPts
  where
    uvPts = subdivideCos cos divisions

evaluateSrfCOSs :: Surface -> [[Point3d]]
evaluateSrfCOSs srf = evaluateCOS srf 16 <$> _COS srf
