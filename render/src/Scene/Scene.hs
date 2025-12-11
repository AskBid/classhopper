{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-} 

module Scene.Scene where 

import Linear.V3
import Data.Map
import Control.Lens
import qualified Graphics.Rendering.OpenGL as GL
import GHC.Float (double2Float)
import Foreign.Marshal.Array (withArray) 
-- effeccient ready solution to translate a 
-- haskell array into contiguous memory. 
import Foreign.Storable (sizeOf)
import Foreign.Ptr (nullPtr)
import Data.IORef

import qualified Geometry.Surface as S
import qualified Geometry.Point as P
import qualified Geometry.Curve as C
import Type 
import qualified Data.IntMap as Map

import Scene.Common (ObjectId(..))
import Scene.GPU
import Scene.WorldRefs

data GeometrySurface = GeometrySurface
  { gsId  :: ObjectId
  , gsDef :: S.Surface
  } deriving Show

data GeometryCurve = GeometryCurve
  { gcId  :: ObjectId
  , gcDef :: C.Curve
  }

-- | each srf and crv ids relates to their counterpart 
-- identical ids in cached-srf and cached-crv
data Scene = Scene
  { _geometrySRFS :: Map ObjectId GeometrySurface
  , _geometryCRVS :: Map ObjectId GeometryCurve
  , _cachedSRFS   :: Map ObjectId CachedSurface
  , _cachedCRVS   :: Map ObjectId CachedCurve
  , _cachedCVS    :: Map ObjectId CachedCVS
  , _cachedHulls  :: Map ObjectId CachedHulls 
  , _cachedAxes   :: Axes
  , _cachedGrid   :: Grid
  , _idCounterRef :: IORef Int
  , _bbox         :: P.BBox
  }
makeLenses ''Scene

zeroScene :: IO Scene
zeroScene = do
  ref <- newIORef 0
  (axes, grid) <- mkWorldRefs 10 10
  pure Scene
    { _geometrySRFS = empty
    , _geometryCRVS = empty
    , _cachedSRFS   = empty
    , _cachedCRVS   = empty
    , _cachedCVS    = empty
    , _cachedHulls  = empty
    , _cachedAxes   = axes
    , _cachedGrid   = grid
    , _idCounterRef = ref
    , _bbox = P.BBox (V3 0 0 0) (V3 0 0 0)
    }

-- | makes sure every new object inserted in 
-- Scene has a unique id Int.
nextId :: IORef Int -> IO ObjectId
nextId ref = do
  i <- readIORef ref
  writeIORef ref (i+1)
  return (ObjectId i) 

deleteAndErase :: ObjectId -> Scene -> Scene
deleteAndErase id scene@Scene{..} = 
  scene & geometryCRVS %~ delete id
        & cachedCRVS   %~ delete id

findSceneBBox :: [GeometrySurface] -> P.BBox
findSceneBBox [] = P.BBox (V3 0 0 0) (V3 0 0 0)
findSceneBBox (srf:srfs) = go srfs (thisBBox srf)
  where 
    thisBBox GeometrySurface{gsDef = S.Surface{..}} = bbox
    
    go :: [GeometrySurface] -> P.BBox -> P.BBox
    go [] accBBox = accBBox
    go (s:rest) accBBox = 
      let sBBox = thisBBox s
          combinedBBox = combineBBox accBBox sBBox
      in go rest combinedBBox
    
    combineBBox :: P.BBox -> P.BBox -> P.BBox
    combineBBox (P.BBox (V3 minX1 minY1 minZ1) (V3 maxX1 maxY1 maxZ1))
                (P.BBox (V3 minX2 minY2 minZ2) (V3 maxX2 maxY2 maxZ2)) =
      P.BBox (V3 (min minX1 minX2) (min minY1 minY2) (min minZ1 minZ2))
             (V3 (max maxX1 maxX2) (max maxY1 maxY2) (max maxZ1 maxZ2))
