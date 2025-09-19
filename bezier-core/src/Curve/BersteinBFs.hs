-- | Bernstein Basis Functions
module Curve.BersteinBFs where

type BernsteinBFs = [Float -> Float]
-- ^ Using Float bc I have speed and efficiency in mind rather
-- then precision, but may need to switch to Double in the future
-- Float 32b, Double 64b

-- | Basis functions for a degree 1 curve (Linear)
deg1_BBFs :: BernsteinBFs
deg1_BBFs = 
  [ (1-)
  , id
  ]

-- | Basis functions for a degree 2 curve (Arch like)
-- `t` is the curve parameter, someitme called `u`
deg2_BBFs :: BernsteinBFs
deg2_BBFs = 
  [ \t -> (t-1)^2
  , \t -> 2*t*(1-t)
  , (^2)
  ]

-- | thought to be the Control Point coordinates
type Point = (Float, Float)

-- | thought tot be the t paramenter to indicate the location
-- along the curve where we are evaluation the curve. 
-- usually 0 < t < 1
type Parameter = Float

-- | given basis function and a set of points finds the point 
-- on the curve at a certain parameter t.
evaluatePt :: BernsteinBFs -> [Point] -> Parameter -> Point
evaluatePt bsfs pts t = tplSum weightedPts 
  where 
    bsfst = map ($ t) bsfs
    tplProduct t (x, y) = (x*t, y*t)
    weightedPts = zipWith tplProduct bsfst pts
    tplSum = foldr (\(x,y) (sx,sy) -> (x+sx, y+sy)) (0,0)

evaluateCrv :: [Point] -> Int -> [Point]
evaluateCrv cvs divisions = evaluatePt deg2_BBFs cvs <$> params
  where 
    params = (/toEnum divisions) <$> [0..toEnum divisions]
