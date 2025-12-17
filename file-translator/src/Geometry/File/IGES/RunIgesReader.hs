{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-} 

module Geometry.File.IGES.RunIgesReader 
  ( getIgesEntities
  ) where 

import Text.Parsec 
  ( runParser
  , ParseError
  )
import Text.Parsec.Pos 
  (initialPos)
import Text.Parsec.Error 
  ( newErrorMessage
  , Message(..)
  , errorMessages
  , messageString
  )
import Data.Default
import qualified Data.Text as T
import Control.Monad.IO.Class 
  (liftIO)
import Data.Maybe 
  (catMaybes)
import RIO 
  ( logInfo
  , logError
  , displayShow
  , display
  , asks
  ) 
import Data.IORef
import qualified Data.IntMap.Strict as IM


import Geometry.File.IGES.BuilderSectionedIges 
  (readIGESfile, buildSectionedIges)
import Geometry.File.IGES.BuilderDirectory 
  (buildDEs)
import Geometry.File.IGES.BuilderParameter 
  (formatParameter)
import Geometry.File.IGES.ParameterParser.Surface128 
  (surface128parser)
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Type
import Geometry.File.TranslatorAppType 
  ( TranslatorApp(..)
  , TranslatorEnv(..)
  , removeDirEntry 
  , getDirEntries
  )

-----------
-- SCHEMA
-----------
-- readIGESfile
--   -> buildSectionedIges
--   -> buildDEs
--   -> formatParameter
--   -> surface128parser

-- | only working for Surface128data for now. As it is 
-- the only one supported as of writing, but it may need 
-- polymorphism later. the @Maybe@ is in case the parameter 
-- text wasn't retrievable. 
-- the @Either@ is for parsing errors.
getIgesEntities 
  :: FilePath 
  -> TranslatorApp [a]
getIgesEntities location = do 

  logInfo $ "Reading IGES file: " 
          <> displayShow location
  rawLines <- liftIO $ readIGESfile location
  logInfo $ "Loaded " 
          <> displayShow (length rawLines) 
          <> " raw lines"

  let igs = buildSectionedIges rawLines
  logInfo "Built raw IGES map"

  buildDEs igs

  pe144s <- processDEs TrimmedSurface144_label

  logInfo $ displayShow pe144s
  
  logInfo "Finished IGES parameter parsing."

  -- let params = onlyRights eParams
  logInfo $ "Attempted " 
          <> displayShow (length []) 
          <> " parameters parsing"
  logInfo $ "Successful parsings: " 
          <> displayShow (length [])

  return []


processDEs :: EntityType_label -> TranslatorApp [ParsedEntity]
processDEs lab = go []
  where
    go acc = do
      mde <- popWhere (\de -> entityType de == lab)
      case mde of
        Nothing ->
          pure (reverse acc)

        Just de -> do
          let pe = parseEntity de
          go (pe : acc)

-- | given a predicate, returns nothing if finds
-- an object from that predicate filtered map, 
-- deleting that element from the map. Nothing otherwise
popWhere
  :: (DirEntry -> Bool)
  -> TranslatorApp (Maybe DirEntry)
popWhere p = do
  ref <- asks teDirEntries
  liftIO $ atomicModifyIORef' ref $ \m ->
    case IM.minViewWithKey (IM.filter p m) of
      Nothing ->
        (m, Nothing)
      Just ((k, de), _) ->

        (IM.delete k m, Just de)

-- popWhere 
--   :: (DirEntry -> Bool) 
--   -> TranslatorApp (Maybe DirEntry)
-- popWhere p = do
--   ref <- asks teDirEntries
--   atomicModifyIORef' ref $ \m ->
--     case findAndRemove p m of
--       Nothing       -> (m, Nothing)
--       Just (de, m') -> (m', Just de)


-- | given the sectioned IGES and a Directory Entry, 
-- it returns the parse parameter as full Entity.
fishParameterFromDE 
  :: SectionedIges 
  -> DirEntry 
  -> TranslatorApp (Either ParseError Surface128)
fishParameterFromDE igs de = do

  let mP = formatParameter (pointerP de) (countPlines de) igs

  case mP of

    Nothing  -> do 
      let err = "Missing parameter text block." :: T.Text
      -- importing RIO everything becomes Utf8Builder
      logError $ display err
      return $ Left (toParseError $ T.unpack err)

    Just p   -> do 
      let parseResult = runParser surface128parser def "" p
      case parseResult of 
        Left err -> do 
          mapM_ (logInfo . displayShow . messageString) $ 
            errorMessages err
          return $ Left err
        Right ent -> do 
          -- logInfo $ displayShow ent
          return $ Right ent


parseEntity :: DirEntry -> ParsedEntity
parseEntity (DirEntry _ Surface128_label _ _) = 
  PESurface128 def
parseEntity (DirEntry _ TrimmedSurface144_label _ _) = 
  PETrimmedSurface144 def

data ParsedEntity
  = PESurface128 Surface128
  | PETrimmedSurface144 TrimmedSurface144
  deriving Show
-- runEntityParser 
--   :: EntityType_label 
--   -> (Parameter -> Either ParseError a)
-- runEntityParser Surface128_label = 
--   runParser surface128parser def "" 
-- runEntityParser Curve126_label =
--   runParser curve126parser def "" 
-- runEntityParser CompositeCurve102_label =
--   runParser compositeCurve102parser def ""
-- runEntityParser CurveOnSurface142_label = 
--   runParser curveOnSurface142parser def ""
-- runEntityParser TrimmedSurface144_label =
--   runParser trimmedSurface144parser def ""


toParseError :: String -> ParseError
toParseError msg = 
  newErrorMessage (Message msg) (initialPos "<input>")


onlyRights :: [Either b a] -> [a]
onlyRights [] = []
onlyRights (eba:ebas) = case eba of
  Right a -> a : onlyRights ebas
  Left _  -> onlyRights ebas
