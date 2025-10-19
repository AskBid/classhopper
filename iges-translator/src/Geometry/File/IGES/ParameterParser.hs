{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.IGES.ParameterParser where 

import Text.Parsec
import qualified Data.Text as T
import Text.Read (readMaybe)
import Control.Monad (when)

import Geometry.File.IGES.Type 
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
    , a  = n1+2*m1 
    , b  = n2+2*m2 
    , c  = (1+k1)*(1+k2)
    }

surface128parser :: ParserP Surface128data 
surface128parser = do 
  et <- entity
  when (et /= Surface128) $
    fail "Expected entity 128 but got something else"
  k1 <- parseKM 
  k2 <- parseKM 
  m1 <- parseKM 
  m2 <- parseKM
  let pads = calculateColsPads k1 k2 m1 m2
  closedU <- parse01 
  closedV <- parse01
  polynomial <- parse01
  periodicU <- parse01
  periodicV <- parse01
  knotsU <- parseDoubles $ a pads
  knotsV <- parseDoubles $ b pads
  weights <- parseDoubles $ c pads
  cps   <- parseDoubles $ c pads * 3
  return undefined -- Surface128data m1 m2 knotsU knotsV weights cps 

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
  case readMaybe (T.unpack tok) of
    Just x  -> return x
    Nothing -> fail $ "Expected a float, got: " ++ T.unpack tok
