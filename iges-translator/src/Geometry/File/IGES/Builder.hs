-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE LambdaCase        #-}

module Geometry.File.IGES.Builder where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Read (decimal)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (foldl')
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad (when, join)

import Geometry.File.IGES.Type 
import Geometry.File.IGES.Parser
import Geometry.Surface


-- | at the moment is only a list of DEs, but if we will start support 
-- for composite entities that refer back to DEs, will need perhaps to 
-- make this a @Map SeqNumDE DirEntry@
buildDEs :: IgesRaw -> [DirEntry] 
buildDEs igs = mapMaybe (\(k,r) ->
    if odd k 
    then readDirectoryEntry k section
    else Nothing
  ) $ IM.toList section
  where 
    section = igs M.! Directory

readDirectoryEntry :: SeqNumDE -> SeqNumRawLine -> Maybe DirEntry
readDirectoryEntry n de = do 
  fstLn <- chunkText 8 <$> IM.lookup n de 
  entityType <- mkEntityType =<< textToInt =<< safeIndex fstLn 0
  sndLn <- chunkText 8 <$> IM.lookup (n+1) de
  pointerP <- textToInt =<< safeIndex fstLn 1 
  countPlines <- textToInt =<< safeIndex sndLn 3
  return $ DirEntry n entityType pointerP countPlines

chunkText :: Int -> T.Text -> [T.Text]
chunkText n txt
  | T.null txt = []
  | otherwise  =
    let (chunk, rest) = T.splitAt n txt
    in chunk : chunkText n rest 

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0     = Nothing
  | otherwise = listToMaybe (drop i xs)
