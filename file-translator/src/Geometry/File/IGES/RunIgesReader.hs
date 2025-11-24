{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.IGES.RunIgesReader 
  ( getIgesEntities
  ) where 

import Text.Parsec (runParser, ParseError)
import Text.Parsec.Pos (initialPos)
import Text.Parsec.Error (newErrorMessage, Message(..))
import Data.Default
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)

import Geometry.File.IGES.BuilderIgesRaw (readIGESfile, buildIgesRaw)
import Geometry.File.IGES.BuilderDirectory (buildDEs)
import Geometry.File.IGES.BuilderParameter (formatParameter)
import Geometry.File.IGES.ParameterParser (surface128parser)
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Type

import Geometry.File.TranslatorAppType (Env, TranslatorApp(..), logInfo)

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
  logInfo $ "Reading IGES file: " <> T.pack location
  rawLines <- liftIO $ readIGESfile location
  logInfo $ "Loaded " <> T.pack (show (length rawLines)) <> " raw lines"
  let igs = buildIgesRaw rawLines
  logInfo "Built raw IGES map"
  let des = buildDEs igs
  logInfo $ "Found " <> T.pack (show (length des)) <> " IGES directory entries"
  let eParams = fishParameterFromDE igs <$> des
  logInfo "Finished IGES parameter parsing."
  let params = onlyRights eParams
  logInfo $ "Attempted " <> T.pack (show (length eParams)) <> " parameters parsing"
  logInfo $ "Successful parsings: " <> T.pack (show (length params))
  return $ params

fishParameterFromDE 
  :: IgesRaw 
  -> DirEntry 
  -> Either ParseError Surface128data
fishParameterFromDE igs de = do
  let mP = formatParameter (pointerP de) (countPlines de) igs
  case mP of
    Nothing  -> Left (toParseError "Missing parameter text block.")
    Just p   -> runParser surface128parser def "" p

toParseError :: String -> ParseError
toParseError msg = newErrorMessage (Message msg) (initialPos "<input>")

onlyRights :: [Either b a] -> [a]
onlyRights [] = []
onlyRights (eba:ebas) = case eba of
  Right a -> a : onlyRights ebas
  Left _  -> onlyRights ebas
