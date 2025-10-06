module Geometry.Surface where

import Geometry.Bernstein
import Geometry.Curve

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
    validSrf Nothing = Nothing
    validSrf (Just uRows') 
      | length uRows' > vDeg = Just $ take (vDeg + 1) uRows'
      | otherwise            = Nothing

chunk :: Int -> [a] -> [[a]]
chunk n xs 
  | n <= length xs = take n xs : chunk n (drop n xs)
  | otherwise      = []

subdivideIsocrv :: DirectionUV -> Parameter -> Surface -> Int -> [Point]
subdivideIsocrv _ _ _ _ = undefined
