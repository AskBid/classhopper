{-# LANGUAGE OverloadedStrings #-}
-- | To better understand this parser is hepful to read about 
-- entity 128 in any IGES ISO documentation.
module Geometry.File.IGES.ParameterParser.Surface128
  ( surface128parser
  ) where 

import Text.Parsec
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (when)
import Control.Lens
import Data.Default

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Helper (textToInt)
import Geometry.File.IGES.ParameterParser.Common

-- | using user state from the parser to build up the 
-- return Surface type.
type Parser128 a = Parsec Parameter Surface128 a


data ColsPaddings = ColsPaddings 
  { nKnotsU :: Int -- n1+2*m1 
  , nKnotsV :: Int -- n2+2*m2 
  , nPoints :: Int -- (1+k1)*(1+k2)
  }

-- | K in iges world is basically n 
--   M in iges worls is basically p
--   from `p = m-n-1` and m = p+n+1 
-- we add 2 because we are not starting from 0.
calculateColsPads :: Int -> Int -> Int -> Int -> ColsPaddings 
calculateColsPads k1 k2 m1 m2 = 
  ColsPaddings  
    { nKnotsU  = k1+m1+2
    , nKnotsV  = k2+m2+2
    , nPoints  = (1+k1)*(1+k2)
    }


surface128parser :: Parser128 Surface128 
surface128parser = do 
  -- modifyState acts on userState of ParsecT
  modifyState (const def)
  et <- entity
  when (et /= Surface128_label) $
    fail "Expected entity 128 but got something else"
  k1 <- parseKM 
  k2 <- parseKM 
  m1 <- parseKM 
  m2 <- parseKM
  modifyState 
    $ (degreeU .~ m1)
    . (degreeV .~ m2)
  let pads = calculateColsPads k1 k2 m1 m2
  parseFlags
  parseKnots pads
  parseWeights pads
  parseCPs pads
  getState


-- SUB PARSERs \/
parseFlags :: Parser128 ()
parseFlags = do
  closedU'    <- parse01
  closedV'    <- parse01
  polynomial' <- parse01
  periodicU'  <- parse01
  periodicV'  <- parse01
  modifyState 
    $ (closedU .~ closedU')
    . (closedV .~ closedV')
    . (polynomial .~ polynomial')
    . (periodicU  .~ periodicU')
    . (periodicV  .~ periodicV')

parseKnots :: ColsPaddings -> Parser128 ()
parseKnots pads = do
  u <- parseDoubles (nKnotsU pads)
  v <- parseDoubles (nKnotsV pads)
  modifyState $
    (knotsU .~ u)
    . (knotsV .~ v)

parseWeights :: ColsPaddings -> Parser128 ()
parseWeights pads = do
  w <- parseDoubles $ nPoints pads
  modifyState $ weights .~ w

parseCPs :: ColsPaddings -> Parser128 ()
parseCPs pads = do
  cps   <- parseDoubles $ nPoints pads * 3
  modifyState $ controlPoints .~ cps
