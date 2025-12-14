{-# LANGUAGE OverloadedStrings #-}
-- | To better understand this parser is hepful to read about 
-- entity 126 in any IGES ISO documentation.
module Geometry.File.IGES.ParameterParser.Curve126 where

import Text.Parsec
import qualified Data.Text as T
import Data.Default

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Helper
import Geometry.File.IGES.ParameterParser.Common

-- | using user state from the parser to build up the 
-- return Curve type.
type Parser126 a = Parsec Parameter Curve126 a

curve126parser :: Parser126 Curve126 
curve126parser = do 
  modifyState (const def)

  getState




