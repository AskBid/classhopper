{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.TranslatorApp where 

import Geometry.File.TranslatorAppType
import Geometry.File.IGES.RunIgesReader (getIgesEntities)
import Geometry.File.IGES.TypeEntity (Surface128)
import RIO
import qualified Data.IntMap.Strict as IM


openFile :: FilePath -> IO [Surface128]
openFile filepath = do 
  logOptions <- logOptionsHandle stdout True
  let logOptionsNoTime = setLogUseTime False logOptions
  withLogFunc logOptionsNoTime $ \lf -> do
    deMapRef <- newIORef IM.empty
    let env = TranslatorEnv lf deMapRef
    runRIO env $ do
      logInfo $ "Opening IGES file..." <> displayShow filepath
      getIgesEntities filepath
