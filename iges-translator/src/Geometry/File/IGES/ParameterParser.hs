module Geometry.File.IGES.ParameterParser where 

import Text.Parsec
import qualified Data.Text as T
import Control.Monad (when)

import Geometry.File.IGES.Type 
import Geometry.File.IGES.Helper

type ParserP a = Parsec Parameter () a

entity :: ParserP EntityType
entity = do
  tok <- anyToken
  case textToInt tok >>= mkEntityType of
    Just et -> return et
    Nothing -> fail $ "Not supported entity: " ++ T.unpack tok

surface128parser :: ParserP EntityType 
surface128parser = do 
  et <- entity
  when (et /= Surface128) $
    fail "Expected entity 128 but got something else"
  k1 <- parseKM 
  k2 <- parseKM 
  m1 <- parseKM 
  m2 <- parseKM
  -- updateStete u here
  knots <- parseKnots
  cps   <- parseControlPoints
  return Surface128

-- | also knows as `n+1` in `p = m-n-1`
parseKM :: ParserP Int 
parseKM = do 
  tok <- anyToken
  case textToInt tok of
    Just n -> return n
    Nothing -> fail "Could not read int for n+1 or p"


parseKnots :: ParserP a 
parseKnots = undefined

parseControlPoints :: ParserP a 
parseControlPoints = undefined
