{-# LANGUAGE OverloadedStrings #-}
-- | To better understand this parser is hepful to read about 
-- entity 126 in any IGES ISO documentation.
module Geometry.File.IGES.ParameterParser.Curve126
  ( curve126parser
  ) where

import Text.Parsec
import qualified Data.Text as T
import Data.Default
import Control.Lens
import Control.Monad (when)

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Helper
import Geometry.File.IGES.ParameterParser.Common

-- | using user state from the parser to build up the 
-- return Curve type.
type Parser126 a = Parsec Parameter Curve126 a


data ColsPaddings = ColsPaddings 
  { nKnots   :: Int
  , nPoints  :: Int
  }

-- | K in iges world is basically n 
--   M in iges worls is basically p
--   from `p = m-n-1` and m = p+n+1 
-- we add 2 because we are not starting from 0.
calculateColsPads :: Int -> Int -> ColsPaddings 
calculateColsPads k m = 
  ColsPaddings  
    { nKnots  = k+m+2
    , nPoints  = 1+k
    }


curve126parser :: Parser126 Curve126 
curve126parser = do 
  modifyState (const def)
  et <- entity
  when (et /= Curve126_label) $
    fail "Expected entity 126 but got something else"
  k <- parseKM 
  m <- parseKM 
  modifyState $ degree .~ m
  let pads = calculateColsPads k m
  parseFlags
  parseKnots pads
  parseWeights pads
  parseCPs pads
  parseV0V1 
  parsePlaneNormal
  getState


-- SUB PARSERs \/
parseFlags :: Parser126 ()
parseFlags = do
  planar'     <- parse01
  closed'     <- parse01
  polynomial' <- parse01
  periodic'   <- parse01
  modifyState 
    $ (planar .~ closed')
    . (closed .~ closed')
    . (polynomial .~ polynomial')
    . (periodic  .~ periodic')

parseKnots :: ColsPaddings -> Parser126 ()
parseKnots pads = do
  u <- parseDoubles (nKnots pads)
  modifyState (knots .~ u)

parseWeights :: ColsPaddings -> Parser126 ()
parseWeights pads = do
  w <- parseDoubles $ nPoints pads
  modifyState $ weights .~ w

parseCPs :: ColsPaddings -> Parser126 ()
parseCPs pads = do
  cps   <- parseDoubles $ nPoints pads * 3
  modifyState $ controlPoints .~ cps

parseV0V1 :: Parser126 ()
parseV0V1 = do
  v0 <- floatToken
  v1 <- floatToken
  modifyState $ (startParamV0 .~ v0) 
              . (endParamV1 .~ v1)

parsePlaneNormal :: Parser126 ()
parsePlaneNormal = do
  currentCurve <- getState
  when (currentCurve ^. planar) $ do
    xn <- floatToken
    yn <- floatToken
    zn <- floatToken
    modifyState $ planeNormal ?~ (xn, yn, zn)
