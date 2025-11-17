-- | Produces basis functions for a BSpline Curve.
module Geometry.BSpline where 

import Geometry.Type
import Geometry.Helper
import Geometry.Point
import Geometry.Bezier (bernsteinSelector)

getBasisFuncs :: Degree -> Maybe Knots -> Maybe [BasisFunc]
getBasisFuncs deg Nothing    = bernsteinSelector deg
getBasisFuncs deg (Just kts) = Just $ coxDeBoorUnsafe deg kts

-- | Produces basis functions for a BSpline Curve.
-- can have check on point count or not(hing).
coxDeBoorUnsafe :: Degree -> Knots -> [BasisFunc]
coxDeBoorUnsafe p kts = basisFunctions kts p

coxDeBoorSafe :: Degree -> Knots -> [Point3d] -> Maybe [BasisFunc]
coxDeBoorSafe p kts pts
  | p < m - n - 1 = Nothing
  | otherwise      = Just (basisFunctions kts p)
  where 
    m = length kts - 1
    n = length pts - 1

-- | Zero-degree basis function
n0 :: Knots -> Int -> BasisFunc
n0 knots i u =
  case (knots !? i, knots !? (i + 1)) of
    (Just ui, Just ui1)
      | ui <= u && u < ui1 -> 1
      | otherwise          -> 0
    _ -> 0

-- | Cox–de Boor recursion
cox :: Knots -> Int -> Int -> BasisFunc
cox knots i p
  | p == 0 = n0 knots i
  | otherwise = \u ->
      let n_i_p1   = cox knots i (p-1) u
          n_i1_p1 = cox knots (i+1) (p-1) u

          ui   = knots !? i
          uip  = knots !? (i+p)
          ui1  = knots !? (i+1)
          uip1 = knots !? (i+p+1)

          alpha =
            case (ui, uip) of
              (Just ui', Just uip') | uip' /= ui' -> (u - ui') / (uip' - ui')
              _ -> 0

          beta =
            case (ui1, uip1) of
              (Just ui1', Just uip1') | uip1' /= ui1' -> (uip1' - u) / (uip1' - ui1')
              _ -> 0
      in alpha * n_i_p1 + beta * n_i1_p1

-- | 
basisFunctions :: Knots -> Int -> [BasisFunc]
basisFunctions knots p =
  [ cox knots i p | i <- [0 .. length knots - p - 2] ]
