module Geometry.Knot where 

import Geometry.Type

-- | if the knot vector is clamped, you can safely rescale it
-- so that the last knot becomes 1 (and the first becomes 0).
-- which means once you know it is no a multispan knot you can-- replace the two values by 0s and 1s (Bezier).
-- NOTE: If a knot vector has no internal knots, then the 
-- curve/surface must be Bézier.
multispan :: Knots -> Maybe Knots 
multispan knots
  | length knots < 3 = Nothing
  | otherwise = 
      if any isInternal (middle knots)
      then Just knots 
      else Nothing 
  where
    firstK = head knots
    lastK  = last knots
    middle = init . tail  -- all except first and last
    isInternal k = k > firstK && k < lastK
