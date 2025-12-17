{-# LANGUAGE LambdaCase        #-}

module Geometry.File.IGES.BuilderSectionedIges where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import Data.Text.Read (decimal)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Text.Read (readMaybe)
import Control.Monad (when, join)

import Geometry.File.IGES.Type 
import Geometry.File.IGES.Helper 

readIGESfile :: FilePath -> IO [FileLine]
readIGESfile location = do 
  file <- TLIO.readFile location
  let lines' = map TL.toStrict (TL.lines file)
  return lines'
  -- TL.lines splits lazily.
  -- TL.toStrict converts each line into strict Text.
  -- You can safely process it with all your T.* functions.

buildSectionedIges :: [FileLine] -> SectionedIges
buildSectionedIges lines =
  foldl' insertLine M.empty $ mapMaybe mkSectionedIgesLine lines 

insertLine :: SectionedIges -> (Section, SeqNumRawLine) -> SectionedIges
insertLine acc (sect, lineMap) =
  M.insertWith IM.union sect lineMap acc

mkSectionedIgesLine :: FileLine -> Maybe (Section, SeqNumRawLine)
mkSectionedIgesLine ln = do
  let (corpText, seqNumText) = T.splitAt 73 ln
  (rawLineChar, sectionChar) <- T.unsnoc corpText
  section <- getSection sectionChar
  seqNum <- textToInt seqNumText
  return (section, IM.singleton seqNum rawLineChar)

getSection :: Char -> Maybe Section
getSection = \case
  'S' -> Just Start
  'G' -> Just Global
  'D' -> Just Directory
  'P' -> Just Parameter
  'T' -> Just Terminate
  _ -> Nothing
