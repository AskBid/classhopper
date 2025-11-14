{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Scene where 

import Data.Maybe (catMaybes, mapMaybe)
import Linear.V3

import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.RunIgesReader
import qualified Geometry.Surface as S
import qualified Geometry.Curve as C
import qualified Geometry.Bernstein as B

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
fileLocation = "../iges-translator/iges-examples/A-pill_Classhopper.igs"
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

fromIgesSceneToScene :: SceneFromIGES -> Scene
fromIgesSceneToScene SceneFromIGES{..} = 
  let srfs = mapMaybe fromSrf128toSuf surfaces
   in Scene srfs [ 
        C.Curve B.deg2_bfs 
          [ V3 (-3) (-3) 0,   V3 0.0 (-3) (-3), V3 3 (-3) 0   ]
      , C.Curve B.deg2_bfs 
          [ V3 (-1) 0 (-0.5), V3 0.0 0 0.5,     V3 1 0 (-0.5) ]
      , C.Curve B.deg2_bfs 
          [ V3 (-1) 1 (-1),   V3 0.0 1 0,       V3 1 1 (-1)   ]
      ]

fromSrf128toSuf :: Surface128data -> Maybe S.Surface 
fromSrf128toSuf Surface128data{..} = do 
  let deg = (6 > max _degreeU _degreeV) && (min _degreeU _degreeV > 0)
      mu = (_degreeU*2) + 2 == length _knotsU
      mv = (_degreeV*2) + 2 == length _knotsV
  if deg && mu && mv
  then S.mkSurface _degreeU _degreeV _controlPoints
  else Nothing

data Scene = Scene 
  { srfs :: [S.Surface]
  , crvs :: [C.Curve]
  }

-- scene = Scene 
--   { srfs = []
--   , crvs =
--       [ C.Curve B.deg2_bfs [ V3 (-1) (-1) 0,   V3 0.0 (-1) (-1), V3 1 (-1) 0   ]
--       , C.Curve B.deg2_bfs [ V3 (-1) 0 (-0.5), V3 0.0 0 0.5,     V3 1 0 (-0.5) ]
--       , C.Curve B.deg2_bfs [ V3 (-1) 1 (-1),   V3 0.0 1 0,       V3 1 1 (-1)   ]
--       ]
--   }
--
-- data Surface128data = Surface128data
--   { _degreeU :: Int
--   , _degreeV :: Int
--   , _knotsU  :: [Double]
--   , _knotsV  :: [Double]
--   , _weights :: [Double]
--   , _controlPoints :: [Double]
--   , _flags :: Flags128
--   } deriving (Show, Eq)
--
-- data Flags128 = Flags128 
--   { _periodicU  :: Bool 
--   , _periodicV  :: Bool 
--   , _polynomial :: Bool
--   , _closedU :: Bool 
--   , _closedV :: Bool 
--   , _accepted :: Maybe Bool
--   } deriving (Show, Eq)

