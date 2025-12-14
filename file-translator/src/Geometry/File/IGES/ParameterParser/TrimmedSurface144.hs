module Geometry.File.IGES.ParameterParser.TrimmedSurface144  
  ( trimmedSurface144parser 
  ) where 

import Text.Parsec
import Data.Default

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.ParameterParser.Common

-- | using user state from the parser to build up the 
-- return Surface type.
type Parser144 a = Parsec Parameter TrimmedSurface144 a

trimmedSurface144parser :: Parser144 TrimmedSurface144
trimmedSurface144parser = do 
  pure def
