{-# LANGUAGE LambdaCase #-} 

-- | Bernstein Basis Functions
module Geometry.Bezier where

import Geometry.Type (BasisFunc)
import Text.ParserCombinators.ReadP (between)

-- | Bernstein is a bezier specific BasisFunc as they are just
-- like basis functions, but unlike BSpline they will always be 
-- valid for the whole parameter domain (knots domain). 
type Bernstein = BasisFunc


-- | Basis functions for a degree 1 curve (Linear)
deg1_bfs :: [Bernstein]
deg1_bfs =
  [ (1-)
  , id
  ]

-- | Basis functions for a degree 2 curve (Arch like)
-- `t` is the curve parameter, someitme called `u`
deg2_bfs :: [Bernstein]
deg2_bfs =
  [ \t -> (t-1)^2
  , \t -> 2*t*(1-t)
  , (^2)
  ] 

deg3_bfs :: [Bernstein]
deg3_bfs =
  [ \t -> (1-t)^3
  , \t -> 3*t*((1-t)^2)
  , \t -> (3*(t^2))*(1-t)
  , (^3)
  ]

deg4_bfs :: [Bernstein]
deg4_bfs =
  [ \t -> (1-t)^4
  , \t -> 4*t*((1-t)^3)
  , \t -> (6*(t^2))*((1-t)^2)
  , \t -> (4*(t^3))*(1-t)
  , (^4)
  ]

deg5_bfs :: [Bernstein]
deg5_bfs =
  [ \t -> (1-t)^5
  , \t -> 5*t*((1-t)^4)
  , \t -> (10*(t^2))*((1-t)^3)
  , \t -> (10*(t^3))*((1-t)^2)
  , \t -> (5*(t^4))*(1-t)
  , (^5)
  ]

bernsteinSelector :: Int -> Maybe [Bernstein]
bernsteinSelector = \case 
  1 -> Just deg1_bfs
  2 -> Just deg2_bfs
  3 -> Just deg3_bfs
  4 -> Just deg4_bfs
  5 -> Just deg5_bfs
  _ -> Nothing
