{-# LANGUAGE LambdaCase #-} 

-- | Bernstein Basis Functions
module Geometry.Bernstein where

type Bernstein = [Float -> Float]
-- ^ Using Float bc I have speed and efficiency in mind rather
-- then precision, but may need to switch to Double in the future
-- Float 32b, Double 64b

-- | Basis functions for a degree 1 curve (Linear)
deg1_bfs :: Bernstein
deg1_bfs =
  [ (1-)
  , id
  ]

-- | Basis functions for a degree 2 curve (Arch like)
-- `t` is the curve parameter, someitme called `u`
deg2_bfs :: Bernstein
deg2_bfs =
  [ \t -> (t-1)^2
  , \t -> 2*t*(1-t)
  , (^2)
  ] 

bernsteinSelector :: Int -> Maybe Bernstein
bernsteinSelector = \case 
  1 -> Just deg1_bfs
  2 -> Just deg2_bfs
  _ -> Nothing
