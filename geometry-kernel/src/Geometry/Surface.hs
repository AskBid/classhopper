{-# LANGUAGE RecordWildCards #-}

module Geometry.Surface where

import Geometry.Type 
  ( Knots
  , BasisFunc
  , Degree
  , ParamRep (BSpline)
  )
import Geometry.Bezier (bernsteinSelector, deg2_bfs)
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

data CVS 
  = Irrational [[Point3d]]
  | Rational [[Point3dW]]

getPoints :: CVS -> [[Point3d]]
getPoints (Irrational pts) = pts
getPoints (Rational ptsW)  = (pt <$>) <$> ptsW 

data Surface = Surface
  { uBasisFuncs :: [BasisFunc]
  , vBasisFuncs :: [BasisFunc]
  , uRep        :: ParamRep
  , vRep        :: ParamRep
  , cvs         :: CVS
  , cos         :: [COS]
  , bbox        :: BBox
  }

instance Show Surface where
  show Surface{cvs = Irrational pts, ..} =
    "BSpline Surface:\n" ++ unlines (map (("  " ++) . show) pts)
  show Surface{cvs = Rational pts, ..} =
    "NURBS   Surface:\n" ++ unlines (map (("  " ++) . show) pts)


-- From a list of Double, construct a point for every 
-- 3 Double, and builds a surface if there are enough 
-- points to comply with the given U and V degrees 
-- and UV knot vectors.
-- For each direction, @multispan@ understands if a 
-- direction needs the BasisFunc calculation with 
-- @CoxDeBoor@. 
-- @getBasisFuncs@ dispatches the calculation based
-- on that @ParamRep@ value.
-- TODO : COS is just a dummy pre-defined COS. 
-- Not building it from inputs yet.
-- TODO make Either String Surface w/ errors
mkBSpline
  :: Degree
  -> Knots
  -> Degree
  -> Knots
  -> [Double]
  -> Maybe Surface
mkBSpline pU ktsU pV ktsV coords =

  let
    (pts, bbox) = chunkPtsAndBox coords

    mU = length ktsU - 1
    mV = length ktsV - 1
    nU1 = mU - pU
    nV1 = mV - pV

    ktsUrep = multispan ktsU 
    ktsVrep = multispan ktsV
    
    uRowsOfPts                     -- :: Maybe [[Point3d]]
      | length pts >= 4 =          -- 4 points (plane).
          if length matrix == nV1 
          then Just $ Irrational matrix 
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
            <*> Just bbox

-- | Create a @mkBSpline@ first, then you can add weights 
-- transforming the BSpline into a NURBS.
-- TODO make Either String Surface w/ errors
mkNURBS :: Surface -> [Double] -> Maybe Surface
mkNURBS srf@Surface{cvs = Irrational pts, ..} weights = do 
  let uLength = length uBasisFuncs
      vLength = length vBasisFuncs
      wMatrix = chunk uLength weights

  if uLength * vLength /= length weights 
     || 
     length wMatrix /= vLength
  then Nothing
  else do 
    let newCVS = zipMatrix pts wMatrix
        newSrf = srf { cvs = Rational newCVS }
    return newSrf
  where 
    zipMatrix :: [[Point3d]] -> [[Double]] -> [[Point3dW]]
    zipMatrix = zipWith $ zipWith Point3dW
mkNURBS _ _ = Nothing



-- | scale/modulate the parameter with each BasisFuncs 
-- in both directions for each UV componenet.
-- Giving a list of i.e. tsEvalByBFu -> which are then 
-- going to scale/modulate each ControlPoint and be 
-- summated(E) by @evaluateSrfPt'@
evaluateSrfPt :: Surface -> PointUV -> Point3d
evaluateSrfPt Surface{..} (V2 u v) = 
  evaluateSrfPt' cvs tsEvalByBFu tsEvalByBFv 
  where
    -- [N_i,p(t)]
    tsEvalByBFu = map ($ u) uBasisFuncs
    tsEvalByBFv = map ($ v) vBasisFuncs 

-- ------------------------
-- |     |  i=0  i=1  i=2 |  i is U index
-- |-----|-----------------
-- | j=0 | P_00 P_10 P_20 |  j is V index 
-- | j=1 | P_01 P_11 P_21 |
-- ------------------------
-- sumE_ij (B_i * B_j * P_ij)
-- it modulates/scale each ControlPoint via a Matrix of 
-- corresponding evaluated UV components for each 
-- corresponding BasisFunc. 
-- then summate(E) each of them to find the evaluated Point.
-- it is basically *nested* summations: 
-- S(u,v) = Σᵢ Σⱼ [Nᵢ,ₚ(u) * Mⱼ,q(v) * Pᵢ,ⱼ]
evaluateSrfPt' :: CVS -> [Double] -> [Double] -> Point3d

evaluateSrfPt' (Irrational pts) tsEvalByBFu tsEvalByBFv = 
  ptsSummationE $ concat modulatePts
  where
    modulateUrow :: Double -> ([Point3d] -> [Point3d])
    modulateUrow tEvalByBFv_j = 
      zipWith (*^) ((* tEvalByBFv_j) <$> tsEvalByBFu) 
    -- ^ (*^) :: 0.5 -> V3 1.0 3.3 4.0 -> V3 0.5 1.7 2.0 
    modulatePts = zipWith modulateUrow tsEvalByBFv pts

evaluateSrfPt' (Rational ptsW) tsEvalByBFu tsEvalByBFv = 
  -- using Left Scalar Product to divide the Sigma of all 
  -- weighted points (and basis func) by the sigma of all 
  -- basis func weighted
  (1.0 / weightSumDenominator) *^ numerator
  where 
    -- Compute basis * weight for each control point
    -- returns both basis * weight and basis * weight * pt
    weightedBasis :: Double -> Point3dW -> (Double, Point3d)
    weightedBasis bfNi (Point3dW{..}) = 
      let bw = bfNi * w
      in (bw, bw *^ pt)
    -- Process each row weighting each point.
    weightUrow :: Double -> [Point3dW] -> [(Double, Point3d)]
    weightUrow tEvalByBFv_j = 
      zipWith weightedBasis ((* tEvalByBFv_j) <$> tsEvalByBFu)
    -- goes through the matrix with the above row processing.
    allWeightedPts = concat $ zipWith weightUrow tsEvalByBFv ptsW
    -- Sum the basis * weights (denominator)
    weightSumDenominator = sum $ fst <$> allWeightedPts
    -- Sum the basis * weighted points (numerator)
    numerator = ptsSummationE $ snd <$> allWeightedPts

sampleIsocrv 
  :: DirectionUV 
  -> Parameter 
  -> Int 
  -> Surface 
  -> [Point3d]
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
