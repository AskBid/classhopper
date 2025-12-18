{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.TranslatorApp where 

import qualified Data.IntMap.Strict as IM
import RIO

import Geometry.File.TranslatorAppType
import Geometry.File.IGES.RunIgesReader (getIgesEntities)
import Geometry.File.IGES.TypeEntity (IgesScene)


openFile :: FilePath -> IO IgesScene
openFile filepath = do 
  logOptions <- logOptionsHandle stdout True
  let logOptionsNoTime = setLogUseTime False logOptions
  withLogFunc logOptionsNoTime $ \lf -> do
    deMapRef <- newIORef IM.empty
    let env = TranslatorEnv lf deMapRef
    runRIO env $ do
      logInfo $ "Opening IGES file..." <> displayShow filepath
      getIgesEntities filepath
