{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.IGES.RunIgesReader 
  ( getIgesEntities
  ) where 

import Text.Parsec 
  ( runParser
  , ParseError
  )
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Error 
  ( newErrorMessage
  , Message(..)
  , errorMessages
  , messageString
  )
import Data.Default
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import RIO 
  ( logInfo
  , logError
  , displayShow
  , display
  ) 

import Geometry.File.IGES.BuilderIgesRaw (readIGESfile, buildIgesRaw)
import Geometry.File.IGES.BuilderDirectory (buildDEs)
import Geometry.File.IGES.BuilderParameter (formatParameter)
import Geometry.File.IGES.ParameterParser (surface128parser)
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Type
import Geometry.File.TranslatorAppType (TranslatorApp(..))

-----------
-- SCHEMA
-----------
-- readIGESfile
--   -> buildIgesRaw
--   -> buildDEs
--   -> formatParameter
--   -> surface128parser

-- | only working for Surface128data for now. As it is the only one 
-- supported as of writing, but it may need polymorphism later.
-- the @Maybe@ is in case the parameter text wasn't retrievable. 
-- the @Either@ is for parsing errors.
getIgesEntities 
  :: FilePath 
  -> TranslatorApp [Surface128data]
getIgesEntities location = do 
  logInfo $ "Reading IGES file: " <> displayShow location
  rawLines <- liftIO $ readIGESfile location
  logInfo $ "Loaded " <> displayShow (length rawLines) <> " raw lines"
  let igs = buildIgesRaw rawLines
  logInfo "Built raw IGES map"
  des <- catMaybes <$> buildDEs igs
  logInfo $ "Found " <> displayShow (length des) <> " IGES directory entries"
  eParams <- mapM (fishParameterFromDE igs) des
  logInfo "Finished IGES parameter parsing."
  let params = onlyRights eParams
  logInfo $ "Attempted " <> displayShow (length eParams) <> " parameters parsing"
  logInfo $ "Successful parsings: " <> displayShow (length params)
  return params

fishParameterFromDE 
  :: IgesRaw 
  -> DirEntry 
  -> TranslatorApp (Either ParseError Surface128data)
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
          mapM_ (logInfo . displayShow . messageString) $ errorMessages err
          return $ Left err
        Right ent -> return $ Right ent

toParseError :: String -> ParseError
toParseError msg = newErrorMessage (Message msg) (initialPos "<input>")

onlyRights :: [Either b a] -> [a]
onlyRights [] = []
onlyRights (eba:ebas) = case eba of
  Right a -> a : onlyRights ebas
  Left _  -> onlyRights ebas
