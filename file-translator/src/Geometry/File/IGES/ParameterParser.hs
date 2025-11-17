{-# LANGUAGE OverloadedStrings #-}
-- | To better understand this parser is hepful to read about 
-- entity 128 in any IGES ISO documentation.
module Geometry.File.IGES.ParameterParser 
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
import Geometry.File.IGES.Helper

-- | using user state from the parser to build up the return Surface type.
type ParserP a = Parsec Parameter Surface128data a

data ColsPaddings = ColsPaddings 
  { n1 :: Int -- 1+k1-m1 
  , n2 :: Int -- 1+k2-m2
  , a  :: Int -- n1+2*m1 
  , b  :: Int -- n2+2*m2 
  , c  :: Int -- (1+k1)*(1+k2)
  }

calculateColsPads :: Int -> Int -> Int -> Int -> ColsPaddings 
calculateColsPads k1 k2 m1 m2 = 
  let n1 = 1+k1-m1 
      n2 = 1+k2-m2
  in ColsPaddings  
    { n1 = n1
    , n2 = n2
    , a  = k1+m1+2  
    , b  = k2+m2+2
    , c  = (1+k1)*(1+k2)
    }

surface128parser :: ParserP Surface128data 
surface128parser = do 
  modifyState (const def)
  et <- entity
  when (et /= Surface128) $
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
parseFlags :: ParserP ()
parseFlags = do
  closedU'    <- parse01
  closedV'    <- parse01
  polynomial' <- parse01
  periodicU'  <- parse01
  periodicV'  <- parse01
  modifyState 
    $ (flags . closedU .~ closedU')
    . (flags . closedV .~ closedV')
    . (flags . polynomial .~ polynomial')
    . (flags . periodicU  .~ periodicU')
    . (flags . periodicV  .~ periodicV')

parseKnots :: ColsPaddings -> ParserP ()
parseKnots pads = do
  u <- parseDoubles (a pads)
  v <- parseDoubles (b pads)
  modifyState $
    (knotsU .~ u)
    . (knotsV .~ v)

parseWeights :: ColsPaddings -> ParserP ()
parseWeights pads = do
  w <- parseDoubles $ c pads
  modifyState $ weights .~ w

parseCPs :: ColsPaddings -> ParserP ()
parseCPs pads = do
  cps   <- parseDoubles $ c pads * 3
  modifyState $ controlPoints .~ cps

entity :: ParserP EntityType
entity = do
  tok <- anyToken
  case textToInt tok >>= ckEntityType of
    Just et -> return et
    Nothing -> fail $ "Not supported entity: " ++ T.unpack tok

-- | also knows as `n+1` in `p = m-n-1`
parseKM :: ParserP Int 
parseKM = do 
  tok <- anyToken
  case textToInt tok of
    Just n -> return n
    Nothing -> fail "Could not read int for n+1 or p"

parse01 :: ParserP Bool
parse01 = do 
  tok <- anyToken
  case tok of
    "0" -> return False
    "1" -> return True
    other -> fail $ "Expected '0' or '1', but got: " ++ T.unpack other

parseDoubles :: Int -> ParserP [Double]
parseDoubles times = count times floatToken

floatToken :: ParserP Double
floatToken = do
  tok <- anyToken
  let tokE = normalizeFortranDouble tok
  case readMaybe (T.unpack $ sortEndingDot tokE) of
    Just x  -> return x
    Nothing -> fail $ "Expected a float, got: " ++ T.unpack tok
  where 
    sortEndingDot t
      | T.last t == '.' = t <> "0"
      | otherwise       = t

-- | Convert Fortran-style exponents (D or d) to E, for Double parsing.
normalizeFortranDouble :: T.Text -> T.Text
normalizeFortranDouble = T.map replaceD
  where
    replaceD c
      | c == 'D' || c == 'd' = 'E'
      | otherwise            = c


