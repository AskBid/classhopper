{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenFile where 

import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Linear.V3
import Control.Monad.Reader
import Control.Lens
import Control.Monad (unless)
import Data.Text (Text)
import RIO ( logInfo, logError, logWarn
           , stdout, logOptionsHandle
           , setLogUseTime, withLogFunc
           , runRIO, displayShow
           )
import Data.Map
import Data.IORef
import Data.Foldable (foldrM)

import Geometry.File.IGES.TypeEntity
import Geometry.File.TranslatorApp (openFile)
import qualified Geometry.Surface as S
import qualified Geometry.Curve as C
import qualified Geometry.Bezier as B
import Geometry.Surface (mkBSpline)
import Geometry.Type (ParamRep(..))
import Type 
import Scene

-- | to transfer the scene to the Scene in bezier-rnder module.
newtype SceneFromIGES = SceneFromIGES
  { surfaces  :: [Surface128data]
  -- , curves    :: [Curve]
  -- , unit      :: Float
  -- , maxSize   :: Int 
  -- , tolerance :: Float
  -- , errors :: 
  } deriving Show

openIGES :: FilePath -> IO SceneFromIGES
openIGES file = do 
  entities <- openFile file
  return $ SceneFromIGES entities

fromIgesSceneToScene :: SceneFromIGES -> IO Scene
fromIgesSceneToScene SceneFromIGES{..} = do
  logOptions <- logOptionsHandle stdout True
  let logOptionsNoTime = setLogUseTime False logOptions
  withLogFunc logOptionsNoTime $ \lf -> do
    let env = RenderEnv lf 10
    runRIO env $ do
      logInfo "Starting to translate parsed IGES to Classhopper Scene..."
      srfs <- processSceneSurfaces surfaces
      scene <- liftIO zeroScene
      foldrM liftAddToScene scene srfs
  where  
    liftAddToScene srf scene = do 
      liftIO $ addToScene srf scene

validateSurface128 
  :: Surface128data 
  -> RenderApp (Maybe Surface128data)
validateSurface128 s0 = do -- Flags check
  let okFlags = 
        and [ not $ s0 ^. flags . periodicU
            , not $ s0 ^. flags . periodicV
            , not $ s0 ^. flags . closedU
            , not $ s0 ^. flags . closedV 
            ]
  unless okFlags $ logWarn "Flags check failed. (Surface128)"
  if not okFlags
  then return Nothing 
  else return (Just s0)  

processSceneSurfaces :: [Surface128data] -> RenderApp [S.Surface]
processSceneSurfaces srf128s = do
  fmap catMaybes $ mapM validateSurface128 srf128s >>= mapM convert 
  where
    convert :: Maybe Surface128data -> RenderApp (Maybe S.Surface)
    convert Nothing  = do 
      logWarn "An IGES surface has NOT passed the validation checks."
      return Nothing
    convert (Just s128) = do
      logInfo "Attempting Classhopper Surface creation from IGES surface."
      let mSrf = mkBSpline (s128 ^. degreeU)
                           (s128 ^. knotsU)
                           (s128 ^. degreeV)
                           (s128 ^. knotsV)
                           (s128 ^. controlPoints)
      case mSrf of
        Nothing -> do 
          logError "The following IGES surface failed to compute to Classhopper Scene."
          logError $ displayShow s128
          pure Nothing
        Just srf -> do 
          if s128 ^. flags . polynomial -- Irrational if True
          then do 
            logInfo "IGES surface succesfully added to Classhopper Scene."
            pure $ Just srf
          else do 
            let rationalSrf = S.mkNURBS srf (s128 ^. weights) 
            case rationalSrf of 
              Nothing -> pure Nothing 
              Just nurbs -> do 
                logInfo $ displayShow nurbs 
                pure $ Just nurbs 
            

