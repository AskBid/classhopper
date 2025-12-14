module Geometry.File.IGES.ParameterParser.CompositeCurve102
  ( compositeCurve102parser
  ) where 

import Text.Parsec
import Data.Default

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity

-- | using user state from the parser to build up the 
-- return Surface type.
type Parser102 a = Parsec Parameter CompositeCurve102 a

compositeCurve102parser :: Parser102 CompositeCurve102
compositeCurve102parser = do 
  pure def 
