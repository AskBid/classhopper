{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module File.Translate.Iges.Scene where 

import Data.Text (Text)
import RIO ( logInfo, logError, logWarn
           , stdout, logOptionsHandle
           , setLogUseTime, withLogFunc
           , runRIO, display ,displayShow
           )
import Data.Map
import Data.IORef
import Data.Foldable (foldlM)
import Control.Monad.IO.Class
import Data.Maybe (catMaybes)


import Geometry.File.IGES.TypeEntity
import Type 
import Scene.Scene
import Scene.GeometryAction 
import Scene.Class 
import File.Translate.Iges.Surface128
import File.Translate.Iges.TrimmedSurface144
import File.Translate.Class
import Geometry.Surface


fromIgesSceneToScene :: IgesScene -> IO Scene
fromIgesSceneToScene igs@IgesScene{..} = do
  logOptions <- logOptionsHandle stdout True
  let logOptionsNoTime = setLogUseTime False logOptions
  withLogFunc logOptionsNoTime $ \lf -> do
    let env = RenderEnv lf 10
    runRIO env $ do
      logInfo txt
      scene <- liftIO zeroScene

      srfs <- ( catMaybes <$> mapM 
                  (takeRightLogLeft . convert) 
                  srfs128 
              ) :: RenderApp [Surface]

      trimSrfs <- ( catMaybes <$> mapM 
                    (takeRightLogLeft . convert) 
                    trimmedSrfs144 
                  ) :: RenderApp [Surface]

      scene1 <- liftIO $ foldlM (flip addToScene) scene srfs
      scene2 <- liftIO $ foldlM (flip addToScene) scene1 trimSrfs

      liftIO $ print scene2
      return scene2
  where  
    liftAddToScene srf scene = do 
      liftIO $ addToScene srf scene
    txt = 
      "Starting to translate IGES types to Classhopper Scene"


takeRightLogLeft 
  :: Show a => Either Text a -> RenderApp (Maybe a)
takeRightLogLeft (Right x) = do 
  logInfo "successfully received:"
  -- logInfo $ displayShow x
  pure $ Just x
takeRightLogLeft (Left err) = do 
  logError $ display err
  pure $ Nothing
