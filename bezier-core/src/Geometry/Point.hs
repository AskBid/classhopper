module Geometry.Point where 

-- | thought to be the Control Point coordinates
type Point = (Float, Float, Float)

ptProduct :: Float -> Point -> Point 
ptProduct t (x, y, z) = (x*t, y*t, z*t)

ptsSummationE :: [Point] -> Point
ptsSummationE = foldr addPt (0,0,0)
  where 
    addPt (x,y,z) (sx,sy,sz) = (x+sx, y+sy, z+sz)
