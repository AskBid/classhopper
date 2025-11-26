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
-- surface if there are enough points to comply with the given U and V degrees.
-- for each direction, if knots are given calculates the BasisFunc with CoxDeBoor.
-- TODO : COS is just a dummy pre-defined COS. Not building it from inputs yet.
-- mkBezier
--   :: Degree 
--   -> Degree 
--   -> [Double] 
--   -> Maybe Surface
-- mkBezier degU degV cvs = do 
--   srf <- Surface
--                 <$> bernsteinSelector degU
--                 <*> bernsteinSelector degV
--                 <*> 
--                 <*>
--                 <*> uRows
--                 <*> cos
--   pure $ srf
--   where
--     pts = (\[x,y,z] -> V3 x y z) <$> chunk 3 cvs
--     uRows
--       | length pts >= 4 = Just $ take (degV + 1) $ chunk (degU + 1) pts
--       | otherwise       = Nothing
--     -- ^ Notice it can NOT exist a surface with less than 4 points (plane)
--     cos = Just [COS deg2_bfs [V2 0 0.5, V2 0.5 0.8, V2 1 0.5]]

-- filterBezier :: Surface -> Maybe Surface 
-- filterBezier ()                  = Just (Bezier basedata)
-- filterBezier (BSpline basedata Nothing Nothing) = Just (Bezier basedata)
-- filterBezier _ = Nothing

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

    ktsU' = multispan ktsU 
    ktsV' = multispan ktsV
    
    uRowsOfPts                     -- :: Maybe [[Point3d]]
      | trace (show $ length pts >= 4) length pts >= 4 =          -- 4 points (plane).
          if length matrix == nV1 
          then Just matrix 
          else Nothing
      | otherwise = Nothing
      where 
        matrix = take nV1 $ chunk nU1 pts
    
    cos = Just [COS deg2_bfs [V2 0 0.5, V2 0.5 0.8, V2 1 0.5]]

  in do 
    trace (unlines [ "number of points: " <> (show $ length pts)
                   , "U degree: " <> show pU 
                   , "V degree: " <> show pV 
                   , "U number of knots: " <> show mU 
                   , "V number of knots: " <> show mV
                   , "U number of points: " <> show nU1
                   , "V number of points: " <> show nV1
                   , "ptsU * ptsV = " <> show (nU1 * nV1) 
                   , "U (m - n - 1) = " <> show (mU - nU1) 
                   , "V (m - n - 1) = " <> show (mV - nV1) 
                   , "ktsU': " <> show ktsU'  
                   , "ktsV': " <> show ktsV'  
                   , "getBasisFuncs pU ktsU': " <> 
                        (case getBasisFuncs pU ktsU' of
                            Nothing -> "NOTHING" 
                            _ -> "OK")
                   , "getBasisFuncs pV ktsV': " <>
                       (case getBasisFuncs pV ktsV' of 
                            Nothing -> "NOTHING" 
                            _ -> "OK")
                   ]) $  
      Surface <$> getBasisFuncs pU ktsU'
              <*> getBasisFuncs pV ktsV'
              <*> Just ktsU' 
              <*> Just ktsV'
              <*> uRowsOfPts
              <*> cos

-- mkBSpline :: Degree -> Degree -> [Double] -> (Knots, Knots) -> Maybe Surface
-- mkBSpline degU degV cvs (ktsU, ktsV) = undefined 
--   where 
--     basisFuncsU = coxDeBoor uDeg kts 

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
--
evaluateSrfPt' :: [[Point3d]] -> [Double] -> [Double] -> Point3d
evaluateSrfPt' srfCVs uBerTs_i vBerTs_j  = ptsSummationE $ concat weightedPts
  where 
    parseUrow vBerT_j = zipWith (*^) ((* vBerT_j) <$> uBerTs_i) 
    -- ^ feed Row of Pts i
    weightedPts = zipWith parseUrow vBerTs_j srfCVs

subdivideIsocrv :: DirectionUV -> Parameter -> Int -> Surface -> [Point3d]
subdivideIsocrv uvDir t divisions (Surface{..}) = case uvDir of
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
