{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | Probably the base types and operation of Scene 
-- those are the types and relative operations that 
-- are related to GPU elements and operations. 
-- All the res ultimately refers to this as it is whot 
-- represents on screen most directly.
-- The term Cache and Cached is meant as entity being 
-- transalted in GPE language and stored into the GPU 
-- buffers.
module Scene.GPU where 

import Control.Lens
import Linear.V3
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))
import GHC.Float (double2Float)
import Foreign.Storable (sizeOf)
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (withArray) 
-- effeccient ready solution to translate a 
-- haskell array into contiguous memory. 

import qualified Geometry.Point as P
import Scene.Common (ObjectId(..))


data GPUData = GPUData
  { _vbo  :: GL.BufferObject
  , _vao  :: GL.VertexArrayObject
  , _nvxs :: GL.NumArrayIndices
  }
makeLenses ''GPUData

-- | below types of the GPU memory location where 
-- geometry entities get stored in GPU vertices form 
-- to be visualised on screen. ObjectId reconnects 
-- them back to their actual geometry type definiton 
-- within the Scene state.
data CachedCurve = CachedCurve
  { _curveSceneId :: ObjectId
  , _curveGpu     :: GPUData
  }
makeLenses ''CachedCurve
emptyCachedCurve :: CachedCurve
emptyCachedCurve = CachedCurve
  (ObjectId 0) (GPUData undefined undefined 0)

data CachedSurface = CachedSurface
  { _surfaceSceneId :: ObjectId
  , _surfaceBorders :: [CachedCurve]
  , _surfaceIsos    :: [CachedCurve]
  }
makeLenses ''CachedSurface
emptyCachedSurface :: CachedSurface
emptyCachedSurface = CachedSurface
  (ObjectId 0) [] []

data CachedHulls = CachedHulls
  { _hullsSceneId :: ObjectId
  , _hullsHulls   :: [CachedCurve]
  }
makeLenses ''CachedHulls

data CachedCVS = CachedCVS
  { _cvsSceneId :: ObjectId
  , _cvsGpu     :: GPUData
  }
makeLenses ''CachedCVS
emptyCachedCVS :: CachedCVS
emptyCachedCVS = CachedCVS
  (ObjectId 0) (GPUData undefined undefined 0)

-- | Classes to have similar lenses for all cashed
-- Types
class HasGpu s where
  gpu :: Lens' s GPUData

class HasSceneId s where
  sceneId :: Lens' s ObjectId

instance HasGpu CachedCurve where
  gpu = curveGpu

instance HasGpu CachedCVS where
  gpu = cvsGpu

instance HasSceneId CachedCurve where
  sceneId = curveSceneId

instance HasSceneId CachedSurface where
  sceneId = surfaceSceneId

instance HasSceneId CachedHulls where
  sceneId = hullsSceneId

instance HasSceneId CachedCVS where
  sceneId = cvsSceneId


-- | from a flatten [x1, y1, z1, x2, y2, z2, ...] list of 
-- vertices creates the memory location reverencs (VBO, VAO) 
-- to refer to to access this GPU information.
cacheVBOVAO :: [Float] -> IO (GL.BufferObject, GL.VertexArrayObject)
cacheVBOVAO vertices = do
  vbo <- GL.genObjectName -- :: IO GL.BufferObject
  vao <- GL.genObjectName -- :: IO GL.VertexArrayObject
  -- ^ These are just handles (IDs) allocated by OpenGL.
  -- They represent GPU memory locations for the buffer and VAO,
  -- but don’t contain any data themselves yet.
  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  GL.bindVertexArrayObject $= Just vao 
  -- ^ bindBuffer ArrayBuffer returns a StateVar (Nohting)
  -- with ($=) we assign to that memory slot in the GPU 
  -- the value on the right of it, our vbo.
  withArray vertices $ \ptr -> do
    let sizeInBytes = fromIntegral (length vertices * sizeOf (0::Float))
    GL.bufferData GL.ArrayBuffer $= (sizeInBytes, ptr, GL.StaticDraw)
  -- ^ Converts the Haskell [Float] into a contiguous pointer for OpenGL.
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 nullPtr)
  -- ^ is saying that at attribute location 0 (the position one) 
  -- (there are 4) there is 3 components of Float, 0 tight 
  -- (because there is no normals or colors or UV, so no other 
  -- attribute to space each vertex by some spaces) 
  -- and 0 offset.
  -- because for instance if you were doing the normal you will 
  -- give the offset of the coordinates first. so a @nullPtr 
  -- `plusPtr` 12@ as 3 float are 12 bytes.
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
  -- ^ If you don’t enable it, the GPU will ignore it.
  GL.bindVertexArrayObject $= Nothing
  GL.bindBuffer GL.ArrayBuffer $= Nothing
  -- ^ unbinding both binds to avoid accidental modifications. 
  -- Close the WiP gates.
  return (vbo, vao)

-- | Helper function to translate Points into flatten 
-- [x1, y1, z1, x2, ...] vertices to be fed into GPU memory 
-- via @cacehVBOVAO@
pts2flattenXYZvertices :: Real a =>  [V3 a] -> [Float]
pts2flattenXYZvertices [] = []
pts2flattenXYZvertices (V3 x y z : pts) = 
  realToFrac x : 
  realToFrac y : 
  realToFrac z : pts2flattenXYZvertices pts
