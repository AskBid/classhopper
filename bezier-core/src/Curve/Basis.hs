-- | Bernstein Basis Functions
module Curve.BersteinBFs where

data BernsteinBFs = BernsteinBFs [Float -> Float]
-- ^ Using Float bc I have speed and efficiency in mind rather
-- then precision, but may need to switch to Double in the future
-- Float 32b, Double 64b

-- | Basis functions for a degree 1 curve (Linear)
deg1_BBFs :: BernsteinBFs
deg1_BBFs = 
  [ \t -> 1-t
  , id
  ]

-- | Basis functions for a degree 2 curve (Arch like)
deg2_BBFs :: BernsteinBFs
deg2_BBFs = 
  [ \t -> (t-1)^2
  , \t -> 2*t*(1−t)
  , \t -> t^2
  ]

-- | thought to be the Control Point coordinates
type Point = (Float, Float)

-- | thought tot be the t paramenter to indicate the location
-- along the curve where we are evaluation the curve. 
-- usually 0 < t < 1
type Parameter = Float

-- | given basis function and a set of points finds the point 
-- on the curve at a certain parameter t.
evaluate :: BernsteinBFs -> [Point] -> Parameter -> Point
evaluate bsf pts t = undefined
