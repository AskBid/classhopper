module Geometry.File.IGES.ParameterParser.PointersEntity102
  ( pointersEntity102parser
  ) where 

import Text.Parsec
import Data.Default
import Control.Lens
import Control.Monad (when)


import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.ParameterParser.Common

-- | using user state from the parser to build up the 
-- return Surface type.
type Parser102 a = Parsec Parameter PointersEntity102 a

pointersEntity102parser :: Parser102 PointersEntity102
pointersEntity102parser = do 
  modifyState (const def)
  et <- entity
  when (et /= CompositeCurve102_label) $
    fail "Expected entity 102 but got something else"
  n <- num 
  modifyState $ nCurvesPointer .~ n 
  cps <- count n num
  modifyState $ curvesPointers .~ cps
  getState
