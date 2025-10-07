{-# LANGUAGE RecordWildCards #-}

module Geometry.Surface where

import Geometry.Bernstein
import Geometry.Curve
import Geometry.Point
import Geometry.Helper

import Debug.Trace (trace)

type ParameterUV = (Float, Float)

data DirectionUV = U | V

data Surface = Surface
  { uSrfBer :: Bernstein
  , vSrfBer :: Bernstein
  , srfCVs :: [[Point]]
  }  

instance Show Surface where 
  show (Surface _ _ pts) = 
    "Surface:\n" ++ unlines (map (("  " ++) . show) pts)

-- From a list of Float construct a point for every 3 Float, and builds a surface
-- if there are enough points to comply with the given U and V degrees.
mkSurface :: Int -> Int -> [Float] -> Maybe Surface
mkSurface uDeg vDeg cvs = 
  Surface <$> bernsteinSelector uDeg <*> bernsteinSelector vDeg <*> validSrf uRows
  where
    pts :: [Point]
    pts = (\[x,y,z] -> (x,y,z)) <$> chunk 3 cvs
    -- Notice that there can't exist a surface with less than 4 points (the plane)
    uRows :: Maybe [[Point]]
    uRows
      | length pts >= 4 = Just $ chunk (uDeg + 1) pts
      | otherwise       = Nothing
    -- Once we found all possible U hulls, we take only the one for vDeg if possible.
    validSrf Nothing = Nothing
    validSrf (Just uRows') 
      | length uRows' > vDeg = Just $ take (vDeg + 1) uRows'
      | otherwise            = Nothing

evaluateSrfPt :: Surface -> ParameterUV -> Point
evaluateSrfPt (Surface{..}) (u,v) = evaluateSrfPt' srfCVs uBerTs vBerTs 
  where
    uBerTs = map ($ u) uSrfBer
    vBerTs = map ($ v) vSrfBer 

-- | i=0  i=1  i=2 
-- | P_00 P_10 P_20 j=0
-- | P_01 P_11 P_21 j=1
-- summationE_ij : B_i * B_j * P_ij
evaluateSrfPt' :: [[Point]] -> [Float] -> [Float] -> Point
evaluateSrfPt' srfCVs uBerTs_i vBerTs_j  = ptsSummationE $ concat weightedPts
  where 
    parseUrow vBerT_j = zipWith ptProduct ((* vBerT_j) <$> uBerTs_i) 
    -- ^ feed Row of Pts i
    weightedPts = zipWith parseUrow vBerTs_j srfCVs

subdivideIsocrv :: DirectionUV -> Parameter -> Int -> Surface -> [Point]
subdivideIsocrv uvDir t divisions (Surface{..}) = case uvDir of
  U -> evaluateSrfPt' srfCVs (map ($ t) uSrfBer) <$> paramsBerTs vSrfBer
  V -> flip (evaluateSrfPt' srfCVs) (map ($ t) vSrfBer) <$> paramsBerTs uSrfBer
  where 
    div = toEnum divisions
    params :: [Float]
    params = (/div) <$> [0..div]
    paramsBerTs :: Bernstein -> [[Float]]
    paramsBerTs uvSrfBer = map (\t -> map ($ t) uvSrfBer) params
