module Geometry.BSpline.Curve where 

import Geometry.Point
import Geometry.Curve
import Geometry.Bernstein (Bernstein)

-- | BSpline Curve before having computed Bernstein Basis Funcs 
data BSplineCurve = BSplineCurve
  { uDegree :: Int 
  , uKnots  :: [Double]
  , uPoints :: [Point3d]
  }

coxDeBoor :: BSplineCurve -> Either [String] Bernstein
coxDeBoor BSplineCurve{..} = undefined



  -- where 
  --   p = uDegree
  --   m = length uKnots-1 
  --   n = length uPoints-1
