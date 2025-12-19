module Geometry.File.IGES.ParameterParser.PointersEntity144  
  ( pointersEntity144parser 
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
type Parser144 a = Parsec Parameter PointersEntity144 a

pointersEntity144parser :: Parser144 PointersEntity144
pointersEntity144parser = do 
  modifyState (const def)
  et <- entity 
  when (et /= TrimmedSurface144_label) $
    fail "Expected entity 142 but got something else"
  
  srf      <- num 
  boundary <- parse01
  nInner   <- num 
  outBound <- num 
  inBounds <- count nInner num  

  modifyState
    $ (trimmedEntityPtr        .~ srf)
    . (boundaryIsBoundaryPtr   .~ boundary) 
    . (countInnerBoundariesPtr .~ nInner)
    . (outerBoundaryPtr        .~ outBound)
    . (innerBoundariesPtr      .~ inBounds)
  -- TODO now that we complete it, the State in this 
  -- Pointers only parsers is probably not necessary
  getState 
