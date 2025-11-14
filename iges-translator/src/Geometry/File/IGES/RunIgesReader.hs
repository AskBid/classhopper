{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.IGES.RunIgesReader 
  ( getIgesEntities
  ) where 

import Text.Parsec
import Data.Default
import qualified Data.Text as T

import Geometry.File.IGES.BuilderIgesRaw (readIGESfile, buildIgesRaw)
import Geometry.File.IGES.BuilderDirectory (buildDEs)
import Geometry.File.IGES.BuilderParameter (formatParameter)
import Geometry.File.IGES.ParameterParser (surface128parser)
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Type

-- | only working for Surface128data for now. As it is the only one 
-- supported as of writing, but it may need polymorphic later.
-- the @Maybe@ is in case the parameter text wasn't retrievable. 
-- the @Either@ is for parsing errors.
getIgesEntities 
  :: FilePath 
  -> IO [Maybe (Either ParseError Surface128data)] 
getIgesEntities location = do 
  rawLines <- readIGESfile location
  let igs = buildIgesRaw rawLines
      des = buildDEs igs
      mParams = readDE igs <$> des
  return $ fmap parse <$> mParams 
  where
    readDE igs de = formatParameter (pointerP de) (countPlines de) igs 
    parse p = runParser surface128parser def (T.unpack $ T.intercalate "," p) p
