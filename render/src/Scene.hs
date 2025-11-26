{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


module Scene where 

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

import Geometry.File.IGES.TypeEntity
import Geometry.File.TranslatorApp (openFile)
import qualified Geometry.Surface as S
import qualified Geometry.Curve as C
import qualified Geometry.Bezier as B
import Geometry.Surface (mkBSpline)
import Geometry.Type (ParamRep(..))
import Type 

fileLocation :: FilePath
-- fileLocation = "../file-translator/iges-examples/NegativeEdgeFix_WiP_220913.igs"
-- fileLocation = "../file-translator/iges-examples/4Classhopper_trimmed.igs"
-- fileLocation = "../file-translator/iges-examples/A-Pill_fillet_srfs_fromRhino.igs"
fileLocation = "../file-translator/iges-examples/A-pill_Classhopper.igs"
-- fileLocation = "../file-translator/iges-examples/hp/1srf_5spansU.igs"
-- fileLocation = "../file-translator/iges-examples/hp/1srf_5spansU8V_trimmed.igs"
-- fileLocation = "../file-translator/iges-examples/saddle.igs"
-- fileLocation = "../file-translator/iges-examples/hp/1srf_normal_trimmed.igs"
-- fileLocation = "../file-translator/iges-examples/221110_Previous IM lights.igs"

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
      return $ Scene srfs cs

data Scene = Scene 
  { srfs :: [S.Surface]
  , crvs :: [C.Curve]
  }

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
      logSurface s0
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

logSurface :: Surface128data -> RenderApp ()
logSurface s0 = do 
  -- logInfo $  [ show (s0 ^. degreeU)
  --      , show (s0 ^. degreeV)
  --      , show (length (s0 ^. knotsU))
  --      , show (s0 ^. knotsU)
  --      , show (length (s0 ^. knotsV))
  --      , show (s0 ^. knotsV)
  --      , show (length (s0 ^. controlPoints))
  --      , show (s0 ^. controlPoints)
  --      ]
  return ()


---------
----TESTS
---------
srftest :: Maybe a -> [a]
srftest Nothing  = []
srftest (Just a) = [a]
-- s1 = srftest $ S.mkBSpline 1 [0,0,1,1] 1 [0,0,1,1] [0,0,0, 100,0,0, 0,100,0, 100,100,0 ]
s1 = srftest $ 
  S.mkBSpline 1 [0,0,0.5,1,1] 
              1 [0,0,1,1] 
              [ -10,-30,0, 50,-30,0, 100,-30,60
              , -10,100,0, 50,100,0, 100,100,60 
              ]

s2 = srftest $ 
  S.mkBSpline 1 [0,0,0.5,1,1]
              1 [0,0,0.5,1,1]
              [ -10,-30,0, 50,-30,0, 100,-30,60
              , -10,50,20, 50,50,20, 100,50,80
              , -10,100,0, 50,100,0, 100,100,60
              ]

c1 = srftest $ C.mkBSpline 1 [0,0,1,1] [0,5,0, 100,5,0]
c2 = srftest $ C.mkBSpline 1 [0,0,0.5,1,1] [0,30,0, 50,30,-60, 100,30,0]
c3 = srftest $ C.mkBSpline 2 [0,0,0,0.5,1,1,1] [0,50,0,  50,50,-60,  80,50,-60,  100,50,0]
c4 = srftest $ C.mkBSpline 3 [0,0,0,0,0.5,1,1,1,1] [0,30,-20,  10,80,-60,  50,100,-70,  80,80,-60,  100,10,-20]

cs = c1 <> c2 <> c3 <> c4
