{-# LANGUAGE OverloadedStrings #-}
-- | methods for Parameter parsers that are not specific
-- to only one entity.
module Geometry.File.IGES.ParameterParser.Common where 

import Text.Parsec
import qualified Data.Text as T
import Text.Read (readMaybe)

import Geometry.File.IGES.Type 
import Geometry.File.IGES.Helper (textToInt)


-- | s is the UserState (the state set by user in Parser)
-- a is the type returned.
-- this is a Parameter parser that need its UserState
-- customised to the particualr parameter type processed.
-- Using Parameter for s means that the Parser Token is not
-- a char but a cell from the parameter section (Text).
type ParserP s a = Parsec Parameter s a

-- | Positive integer numbers only.
num :: ParserP s Int 
num = do 
  tok <- anyToken
  case textToInt tok of
    Just n -> if n >= 0 
              then return n 
              else fail "The number was negative."
    Nothing -> fail "Could not read number."

-- | checks the first cell of the Parameter and makes
-- sure it is about a supported entity.
entity :: ParserP s EntityType_label
entity = do
  tok <- anyToken
  case textToInt tok >>= ckEntityType of
    Just et -> return et
    Nothing -> fail $ "Not supported entity: " ++ T.unpack tok

-- | to parse flags with only 0 or 1 values possible.
parse01 :: ParserP s Bool
parse01 = do 
  tok <- anyToken
  case tok of
    "0" -> return False
    "1" -> return True
    other -> 
      fail $ "Expected '0' or '1', but got: " ++ T.unpack other


-- | K also knows as `n` from `p = m-n-1`
-- where p is degree, n the points - 1
-- and m the knots - 1.
-- M is the degree, so p.
parseKM :: ParserP s Int 
parseKM = do 
  tok <- anyToken
  case textToInt tok of
    Just n -> return n
    Nothing -> fail "Could not read int for n+1 or p"

-- | we usually have a count on how many coordinates we 
-- need to get for a certain entity.
-- This parser takes that count number and returns a parser
parseDoubles :: Int -> ParserP s [Double]
parseDoubles times = count times floatToken

-- | parses doubles values such as a point coordinate
floatToken :: ParserP s Double
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


-- | Convert Fortran-style exponents (D or d) to E, 
-- for Double parsing.
normalizeFortranDouble :: T.Text -> T.Text
normalizeFortranDouble = T.map replaceD
  where
    replaceD c
      | c == 'D' || c == 'd' = 'E'
      | otherwise            = c

