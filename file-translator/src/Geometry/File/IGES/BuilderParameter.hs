module Geometry.File.IGES.BuilderParameter 
  ( formatParameter 
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (foldl')
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad (when, join)

import Geometry.File.IGES.Type 
import Geometry.File.IGES.BuilderIgesRaw
import Geometry.File.IGES.Helper

-- | the column at which the free formatted parameter data stops.
pColumnEnd :: SeqNumP
pColumnEnd = 65

formatParameter :: SeqNumP -> Int -> IgesRaw -> Maybe Parameter
formatParameter start pCount igs = do
  let paramSect = igs M.! Parameter
      (_, temp) = IM.split (start - 1) paramSect
      (ps, _) = IM.split (start + pCount) temp
  pAsText <- safeInit $ T.concat $ IM.foldrWithKey processLine [] ps
  separator <- snd $ takeFirstNumAndSep pAsText
  return $ splitText separator pAsText
  where
    processLine _ l acc =
      let (p, _) = T.splitAt pColumnEnd l
      in T.strip p : acc
      -- ^ to check if SeqNumDE was right, check on _).

takeFirstNumAndSep :: T.Text -> (T.Text, Maybe Char)
takeFirstNumAndSep t =
  let numPart = T.takeWhile isDigit t
      sepPart = T.take 1 $ T.drop (T.length numPart) t
  in (numPart, textToChar sepPart)
