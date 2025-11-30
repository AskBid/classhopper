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
        emptyScene = Scene empty empty empty empty
    runRIO env $ do
      logInfo "Starting to translate parsed IGES to Classhopper Scene..."
      srfs <- processSceneSurfaces surfaces
      idCountRef <- liftIO $ newIORef 0
      filledScene <- foldrM (liftInsertSrf idCountRef) emptyScene srfs
      return $ filledScene
  where 
    insertSrf 
      :: IORef Int 
      -> S.Surface 
      -> Scene 
      -> IO Scene
    insertSrf idCountRef srf scene = do
      objId <- nextId idCountRef
      let gs = GeometrySurface objId srf
          scene' = scene {geometrySRFS = insert objId gs (geometrySRFS scene)}
      ts <- tessellate gs
      let scene'' = scene' {cachedSRFS = insert objId ts (cachedSRFS scene)}
      return scene''

    liftInsertSrf idCountRef srf scene = do 
      liftIO $ insertSrf idCountRef srf scene

validateSurface128 
  :: Surface128data 
  -> RenderApp (Maybe Surface128data)
validateSurface128 s0 = do -- Flags check
  let okFlags = 
        and [ not $ s0 ^. flags . periodicU
            , not $ s0 ^. flags . periodicV
            ,       s0 ^. flags . polynomial
            , not $ s0 ^. flags . closedU
            , not $ s0 ^. flags . closedV 
            ]
  unless okFlags $ logWarn "Flags check failed. (Surface128)"
  if not okFlags
  then return Nothing 
  else do -- Irrational check
    let all1 = all (==1) (s0 ^. weights)
    unless all1 $ logWarn "Irrational weight check failed. (Surface128)"
    if all1 
    then do 
      return (Just s0)  
    else return Nothing

processSceneSurfaces :: [Surface128data] -> RenderApp [S.Surface]
processSceneSurfaces srf128s = do
  fmap catMaybes $ mapM validateSurface128 srf128s >>= mapM convert 
  where
    convert :: Maybe Surface128data -> RenderApp (Maybe S.Surface)
    convert Nothing  = do 
      logWarn "An IGES surface has NOT passed the validation checks."
      return Nothing
    convert (Just s) = do
      logInfo "Attempting Classhopper Surface creation from IGES surface."
      let mSrf = mkBSpline (s ^. degreeU)
                           (s ^. knotsU)
                           (s ^. degreeV)
                           (s ^. knotsV)
                           (s ^. controlPoints)
      case mSrf of
        Nothing -> do 
          logError "The following IGES surface failed to compute to Classhopper Scene."
          logError $ displayShow s
          pure Nothing
        Just srf -> do 
          logInfo "IGES surface succesfully added to Classhopper Scene."
          pure $ Just srf
