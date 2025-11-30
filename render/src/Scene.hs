{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

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

newtype ObjectId = ObjectId Int 
  deriving (Eq, Ord, Show)

nextId :: IORef Int -> IO ObjectId
nextId ref = do
  i <- readIORef ref
  writeIORef ref (i+1)
  return (ObjectId i) 

data GeometrySurface = GeometrySurface
  { gsId  :: ObjectId
  , gsDef :: S.Surface
  }

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
  }

data Scene = Scene
  { geometrySRFS :: Map ObjectId GeometrySurface
  , geometryCRVS :: Map ObjectId GeometryCurve
  , cachedSRFS   :: Map ObjectId CachedSurface
  , cachedCRVS   :: Map ObjectId CachedCurve
  -- , idCounterRef :: IORef Int
  }

-- | defines how to get the VBO/VAO of a type.
class Tessellatable a cached where
  tessellate :: a -> IO cached

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
  -- ^ is saying that at attribute location 0 (the position one) (there are 4)
  -- there is 3 components of Float, 0 tight (because there is no normals 
  -- or colors or UV, so no other attribute to space each vertex by some spaces) 
  -- and 0 offset.
  -- because for instance if you were doing the normal you will give the offset
  -- of the coordinates first. so a @nullPtr `plusPtr` 12@ as 3 float are 12 bytes.
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
