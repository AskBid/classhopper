{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE AllowAmbiguousTypes   #-}

module Scene where 

import Linear.V3
import Data.Map
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

newtype ObjectId = ObjectId Int 
  deriving (Eq, Ord, Show)

data GeometrySurface = GeometrySurface
  { gsId  :: ObjectId
  , gsDef :: S.Surface
  } deriving Show

data GeometryCurve = GeometryCurve
  { gcId  :: ObjectId
  , gcDef :: C.Curve
  }

data CachedCurve = CachedCurve
  { ccId   :: ObjectId
  , ccVBO  :: GL.BufferObject
  , ccVAO  :: GL.VertexArrayObject
  , ccVertexCount :: GL.NumArrayIndices
  }

data CachedSurface = CachedSurface
  { csId      :: ObjectId
  , csBorders :: [CachedCurve]
  , csIsoCrvs :: [CachedCurve]
  -- , csSpanIsos :: [CachedCurve]
  }

data CachedHulls = CachedHulls
  { chId    :: ObjectId
  , chHulls :: [CachedCurve]
  }

data CachedCVS = CachedCVS 
  { ccvId   :: ObjectId
  , ccvVBO  :: GL.BufferObject
  , ccvVAO  :: GL.VertexArrayObject
  , ccvVertexCount :: GL.NumArrayIndices
  }

-- | each srf and crv ids relates to their counterpart 
-- identical ids in cached-srf and cached-crv
data Scene = Scene
  { geometrySRFS :: Map ObjectId GeometrySurface
  , geometryCRVS :: Map ObjectId GeometryCurve
  , cachedSRFS   :: Map ObjectId CachedSurface
  , cachedCRVS   :: Map ObjectId CachedCurve
  , cachedCVS    :: Map ObjectId CachedCVS
  , cachedHulls  :: Map ObjectId CachedHulls 
  , idCounterRef :: IORef Int
  , bbox         :: P.BBox
  }

zeroScene :: IO Scene
zeroScene = do
  ref <- newIORef 0
  pure Scene
    { geometrySRFS = empty
    , geometryCRVS = empty
    , cachedSRFS   = empty
    , cachedCRVS   = empty
    , cachedCVS    = empty
    , cachedHulls  = empty
    , idCounterRef = ref
    , bbox = P.BBox (V3 0 0 0) (V3 0 0 0)
    }

-- | makes sure every new object inserted in 
-- Scene has a unique id Int.
nextId :: IORef Int -> IO ObjectId
nextId ref = do
  i <- readIORef ref
  writeIORef ref (i+1)
  return (ObjectId i) 

-- | defines how to get the VBO/VAO of a type.
-- perhaps Tesselate is not most appropriate 
-- naming for all elements this class it used 
-- with. But makes sense to me.
class Tessellatable a cached where
  tessellate :: a -> IO cached

-- | add a geometrical entity to the Scene.
-- adding its GPU representation too (cached<Geometry>)
class Stageable a where 
  addToScene :: a -> Scene -> IO Scene 

deleteAndErase :: ObjectId -> Scene -> Scene
deleteAndErase id scene@Scene{..} = do 
  let newGeometryMap = delete id geometryCRVS
      newCachedMap   = delete id cachedCRVS 
  scene { geometryCRVS = newGeometryMap
        , cachedCRVS   = newCachedMap 
        }

-- | Used to show in the scene a GPU representation 
-- of the control pooints of a Geometry.
class GeometryHandle a where 
  showHandle :: a -> Scene -> IO Scene 
  -- hideHandle :: a -> Scene -> Scene 

-- | Takes an exisiting GeometrySurface in the Scene 
-- and add the VBO VAO of the control points - hulls.
instance GeometryHandle GeometrySurface where 
  showHandle GeometrySurface{..} scene@Scene{..} = do
    let pts = concat $ S.getPoints (S.cvs gsDef)
        vtxs = pts2flattenXYZvertices pts
    (vbo, vao) <- cacheVBOVAO vtxs 
    let newCachedCVS = CachedCVS { ccvId = gsId
                         , ccvVBO = vbo 
                         , ccvVAO = vao 
                         , ccvVertexCount = fromIntegral $ length vtxs
                         }  
        scene' = scene 
          { cachedCVS = insert gsId newCachedCVS cachedCVS
          }
    -- TODO should add hulls cachedHulls too.
    pure scene'

instance Stageable S.Surface where 
  addToScene crv scene@Scene{..} = do
    objId <- nextId idCounterRef
    let gs = GeometrySurface objId crv
        scene' = scene 
          { geometrySRFS = insert objId gs geometrySRFS
          }
    ts <- tessellate gs
    let scene'' = scene' 
          { cachedSRFS = insert objId ts cachedSRFS
          }
    return scene''

instance Stageable C.Curve where 
  addToScene crv scene@Scene{..} = do
    objId <- nextId idCounterRef
    let gc = GeometryCurve objId crv
        scene' = scene 
          { geometryCRVS = insert objId gc geometryCRVS
          }
    tc <- tessellate gc
    let scene'' = scene' 
          { cachedCRVS = insert objId tc cachedCRVS
          }
    return scene''


samplingAmount :: Int 
samplingAmount = 9

instance Tessellatable GeometryCurve CachedCurve where
  tessellate GeometryCurve{..} = do
    let pts = C.sampleCrv gcDef samplingAmount
        verticesFloats = pts2flattenXYZvertices pts
        vertexCount = length pts  
        -- needed for draw call
    (vbo, vao) <- cacheVBOVAO verticesFloats
    return $ CachedCurve
      { ccId = gcId
      , ccVBO = vbo
      , ccVAO = vao
      , ccVertexCount = fromIntegral vertexCount
      }

instance Tessellatable GeometrySurface CachedSurface where
  tessellate GeometrySurface{..} = do
    b1 <- cachedCrv S.U 0 
    b2 <- cachedCrv S.U 1 
    b3 <- cachedCrv S.V 0 
    b4 <- cachedCrv S.V 1 
    i1 <- cachedCrv S.U 0.5
    i2 <- cachedCrv S.V 0.5
    return $ CachedSurface
      { csId = gsId
      , csBorders = [b1,b2,b3,b4]
      , csIsoCrvs = [i1,i2]
      }
    where 
      cachedCrv dir param = do
        let pts = S.sampleIsocrv dir param samplingAmount gsDef
            verticesFloats = pts2flattenXYZvertices pts
            vertexCount = length pts
        (vbo, vao) <- cacheVBOVAO verticesFloats
        return $ CachedCurve 
          { ccId  = gsId
          , ccVBO = vbo 
          , ccVAO = vao
          , ccVertexCount = fromIntegral vertexCount
          }

cacheVBOVAO :: [Float] -> IO (GL.BufferObject, GL.VertexArrayObject)
cacheVBOVAO vertices = do
  vbo <- GL.genObjectName -- :: IO GL.BufferObject
  vao <- GL.genObjectName -- :: IO GL.VertexArrayObject
  -- ^ These are just handles (IDs) allocated by OpenGL.
  -- They represent GPU memory locations for the buffer and VAO,
  -- but don’t contain any data themselves yet.
  GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
  GL.bindVertexArrayObject GL.$= Just vao 
  -- ^ bindBuffer ArrayBuffer returns a StateVar (Nohting)
  -- with ($=) we assign to that memory slot in the GPU 
  -- the value on the right of it, our vbo.
  withArray vertices $ \ptr -> do
    let sizeInBytes = fromIntegral (length vertices * sizeOf (0::Float))
    GL.bufferData GL.ArrayBuffer GL.$= (sizeInBytes, ptr, GL.StaticDraw)
  -- ^ Converts the Haskell [Float] into a contiguous pointer for OpenGL.
  GL.vertexAttribPointer (GL.AttribLocation 0) GL.$=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
  -- ^ is saying that at attribute location 0 (the position one) 
  -- (there are 4) there is 3 components of Float, 0 tight 
  -- (because there is no normals or colors or UV, so no other 
  -- attribute to space each vertex by some spaces) 
  -- and 0 offset.
  -- because for instance if you were doing the normal you will 
  -- give the offset of the coordinates first. so a @nullPtr 
  -- `plusPtr` 12@ as 3 float are 12 bytes.
  GL.vertexAttribArray (GL.AttribLocation 0) GL.$= GL.Enabled
  -- ^ If you don’t enable it, the GPU will ignore it.
  GL.bindVertexArrayObject GL.$= Nothing
  GL.bindBuffer GL.ArrayBuffer GL.$= Nothing
  -- ^ unbinding both binds to avoid accidental modifications. 
  -- Close the WiP gates.
  return (vbo, vao)

pts2flattenXYZvertices :: [P.Point3d] -> [Float]
pts2flattenXYZvertices [] = []
pts2flattenXYZvertices (V3 x y z :pts) = 
  double2Float x : 
  double2Float y : 
  double2Float z : pts2flattenXYZvertices pts

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
