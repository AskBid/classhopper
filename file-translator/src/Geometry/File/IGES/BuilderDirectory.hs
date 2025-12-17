{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoImplicitPrelude #-}

module Geometry.File.IGES.BuilderDirectory where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe (mapMaybe, listToMaybe)
import Data.List (foldl')
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad (when, join, forM_)
import RIO 
  ( logInfo
  , logError
  , logWarn
  , displayShow
  , asks
  , atomicModifyIORef'
  , writeIORef
  , readIORef
  )

import Geometry.File.IGES.Type 
  (ckEntityType
  , SectionedIges(..)
  , DirEntry(..)
  , Section(..)
  , DirEntriesMap(..)
  )
import Geometry.File.IGES.BuilderSectionedIges
import Geometry.File.IGES.Helper 
  ( safeIndex
  , textToInt
  )
import Geometry.File.TranslatorAppType 
  ( TranslatorApp(..)
  , TranslatorEnv(..)
  , modifyDirEntries
  )


buildDEs :: SectionedIges -> TranslatorApp ()
buildDEs igs = do
  let linesDEsection = IM.toList section
  logInfo $
    "Found "
      <> displayShow (length linesDEsection)
      <> " lines in the Directory Entry section of the IGES file."

  forM_ (mapCouples (,) linesDEsection) $ \(l1, l2) -> do
    mde <- readDirectoryEntry l1 l2
    forM_ mde $ \de ->
      modifyDirEntries (IM.insert (seqNum de) de)

  where
    section = igs M.! Directory

    mapCouples :: (a -> a -> b) -> [a] -> [b]
    mapCouples f [] = []
    mapCouples f [l] = []
    mapCouples f (l1:l2:ls) = f l1 l2 : mapCouples f ls


-- | given two lines of Dir Entry, creates the actual type data
readDirectoryEntry 
  :: (IM.Key, T.Text) 
  -> (IM.Key, T.Text) 
  -> TranslatorApp (Maybe DirEntry)
readDirectoryEntry (rowN1, l1) (rowN2, l2) = do
  let cellsL1 = chunkText 8 l1
      deEntityTypeDigit = textToInt =<< safeIndex cellsL1 0

  case deEntityTypeDigit of 

    Nothing -> do 
      logError "EntityType digit was not perceived."
      return Nothing 

    Just n  -> do 
      logInfo $ "EntityType digit reckognised as: " <> displayShow n
      case ckEntityType n of
        Nothing -> do 
          logWarn "The EntityType found is not supported." 
          return Nothing
        Just e -> do 
          logInfo $ "Reckognised entity type " <> displayShow e <> "."
          let cellsL2 = chunkText 8 l2
              pointerP = textToInt =<< safeIndex cellsL1 1 
              -- ^ yes, pointer seems always to be at the same cell.
              -- for all entities. So kind of a default.
              countOfPlines = textToInt =<< safeIndex cellsL2 3
              -- same for parameter lines count.
          case DirEntry rowN1 e <$> pointerP <*> countOfPlines of 
            Nothing -> do 
              logError "Directory Entry not paramenter pointer or lines count missing!"
              return Nothing 
            Just de -> do 
              logInfo "Directory entity successfully parsed:"
              logInfo $ displayShow de
              return $ Just de


chunkText :: Int -> T.Text -> [T.Text]
chunkText n txt
  | T.null txt = []
  | otherwise  =
    let (chunk, rest) = T.splitAt n txt
    in chunk : chunkText n rest 
