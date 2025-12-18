module Geometry.File.IGES.ParameterParser.PointersEntity142
  ( curveOnSurface142parser 
  ) where 

import Text.Parsec
import Data.Default

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity

-- | using user state from the parser to build up the 
-- return Surface type.
type Parser142 a = Parsec Parameter CurveOnSurface142 a

curveOnSurface142parser :: Parser142 CurveOnSurface142
curveOnSurface142parser = do 
  pure def 
