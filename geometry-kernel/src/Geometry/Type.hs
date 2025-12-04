module Geometry.Type where 

type Degree = Int

type Knots = [Double]

newtype Weights = Weights [Double]
  deriving Show

type BasisFunc = Double -> Double
-- ^ Using Float bc I have speed and efficiency in mind rather
-- then precision, but may need to switch to Double in the 
-- future - (Float 32b, Double 64b)
-- Eventually I transfred to Double.

-- | distinguishes between a parametrisation defined without 
-- internal knots and one with. Useful in case is required to 
-- split a BSpline into Bezier eventually.
-- RationalBezier is a NURBS, so an entity with Non-Uniform- 
-- -Rationl Weights, but that is not MultiSpan. 
data ParamRep 
  = Bezier 
  | BSpline Knots 
  | RationalBezier Weights 
  | NURBS Knots Weights 
  deriving Show

