{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Scene where 

import Data.Maybe (catMaybes, mapMaybe)
import Linear.V3
import Control.Monad.Writer
import Control.Lens
import Control.Monad (unless)

import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.RunIgesReader
import qualified Geometry.Surface as S
import qualified Geometry.Curve as C
import qualified Geometry.Bezier as B
import Geometry.Surface (mkSurface)


-- | to transfer the scene to the Scene in bezier-rnder module.
newtype SceneFromIGES = SceneFromIGES
  { surfaces  :: [Surface128data]
  -- , curves    :: [Curve]
  -- , unit      :: Float
  -- , maxSize   :: Int 
  -- , tolerance :: Float
  -- , errors :: 
  } deriving Show

fileLocation :: FilePath
-- fileLocation = 
-- "../iges-translator/iges-examples/NegativeEdgeFix_WiP_220913.igs"
-- fileLocation = "../iges-translator/iges-examples/4Classhopper_trimmed.igs"
fileLocation = "../file-translator/iges-examples/A-pill_Classhopper.igs"
-- fileLocation = 
-- "../iges-translator/iges-examples/221110_Previous IM lights.igs"

openIGES :: FilePath -> IO SceneFromIGES
openIGES file = do 
  mParsedEntities <- getIgesEntities file
  -- ^ with unfound Parameters
  let parsedEntities = catMaybes mParsedEntities
      -- ^ with parsing errors
      entities = mapMaybe filterParsed parsedEntities
  -- putStrLn "ENTITIES:::::::::"
  -- print entities
  return $ SceneFromIGES entities
  where 
    filterParsed = \case
      Right x -> Just x
      Left _  -> Nothing

fromIgesSceneToScene :: SceneFromIGES -> SceneValidator Scene
fromIgesSceneToScene SceneFromIGES{..} = do
  srfs <- processSceneSurfaces surfaces
  return $ Scene srfs tempCurves

data Scene = Scene 
  { srfs :: [S.Surface]
  , crvs :: [C.Curve]
  }

type SceneValidator a = WriterT [String] Identity a
-- ^ type Validator a = WriterT [String] (Reader ValidationEnv) a
-- example of how we can add a ReaderT to read a config env in 
-- the future
-- one of the big strengths of WriterT and monad transformers: 
-- you can stack multiple WriterTs or combine them with other 
-- monads to accumulate logs from different layers, and it all 
-- merges naturally if you design the log type correctly.

validateSurface128 :: Surface128data -> SceneValidator (Maybe Surface128data)
validateSurface128 s0 = do -- Flags check
  let okFlags = 
        and [ not $ s0 ^. flags . periodicU
            , not $ s0 ^. flags . periodicV
            ,       s0 ^. flags . polynomial
            , not $ s0 ^. flags . closedU
            , not $ s0 ^. flags . closedV 
            ]
  unless okFlags $ tell ["Flags check failed. (Surface128)"]

  if not okFlags
  then return Nothing 
  else do -- Irrational check
    let all1 = all (==1) (s0 ^. weights)
    unless all1 $ tell ["Irrational weight check failed. (Surface128)"]

    if not all1 
    then return Nothing 
    else do -- Degree + knot check
      let degU = s0 ^. degreeU
          degV = s0 ^. degreeV
          m1m2 = (length (s0 ^. knotsU) - 1) + (length (s0 ^. knotsV) - 1)
          n1n2 = length (s0 ^. controlPoints) - 2
          pmnOk = degU + degV == m1m2 - n1n2 -2
      unless (degOK && pmnOk) $ tell ["Degree/knot check (p=m-n-1) failed. (Surface128)"]
      -- ^ this check may not be necessary as we have already 
      -- a robust mkBSpline that would return Nothing for wrong 
      -- points count

      if not pmnOk
      then return Nothing
      else return (Just s0) -- all checks passed

processSceneSurfaces :: [Surface128data] -> SceneValidator [S.Surface]
processSceneSurfaces srf128s = do
  fmap catMaybes $ mapM validateSurface128 srf128s >>= mapM convert
  where
    convert :: Maybe Surface128data -> SceneValidator (Maybe S.Surface)
    convert Nothing  = return Nothing
    convert (Just s) = do 
      let mBSplineU = length (s ^. knotsU) > (s ^. degreeU * 2) + 2
          mBSplineV = length (s ^. knotsV) > (s ^. degreeV * 2) + 2
      mkSurface 
      
      -- case S.mkBSpline (s ^. degreeU) (s ^. degreeV) (s ^. controlPoints) of
      --   Nothing   -> tell ["mkSurface failed"] >> return Nothing
      --   Just surf -> return (Just surf)



------------
-- temporary
------------
-- Just to visualise some curves before we handle them from IGES.
tempCurves = 
  [ C.Curve B.deg2_bfs [ V3 (-3) (-3) 0,   V3 0.0 (-3) (-3), V3 3 (-3) 0   ]
  , C.Curve B.deg2_bfs [ V3 (-1) 0 (-0.5), V3 0.0 0 0.5,     V3 1 0 (-0.5) ]
  , C.Curve B.deg2_bfs [ V3 (-1) 1 (-1),   V3 0.0 1 0,       V3 1 1 (-1)   ]
  ]
