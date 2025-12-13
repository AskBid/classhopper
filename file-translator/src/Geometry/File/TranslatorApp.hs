{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.TranslatorApp where 

import Geometry.File.TranslatorAppType
import Geometry.File.IGES.RunIgesReader (getIgesEntities)
import Geometry.File.IGES.TypeEntity (Surface128)
import RIO

openFile :: FilePath -> IO [Surface128]
openFile filepath = do 
  logOptions <- logOptionsHandle stdout True
  let logOptionsNoTime = setLogUseTime False logOptions
  withLogFunc logOptionsNoTime $ \lf -> do
    let env = TranslatorEnv lf
    runRIO env $ do
      logInfo $ "Opening IGES file..." <> displayShow filepath
      getIgesEntities filepath
