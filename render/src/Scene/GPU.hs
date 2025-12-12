{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}

-- | Probably the base types and operation of Scene 
-- those are the types and relative operations that 
-- are related to GPU elements and operations. 
-- All the res ultimately refers to this as it is whot 
-- represents on screen most directly.
-- The term Cache and Cached is meant as entity being 
-- transalted in GPE language and stored into the GPU 
-- buffers.
module Scene.GPU where 

import Data.List (foldl')
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

import Geometry.Point
import Scene.Common (ObjectId(..))

newtype GLBox = GLBox BBox 
  deriving Show

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
  { _cachedCurveSceneId :: ObjectId
  , _cachedCurveGpu     :: GPUData
  , _cachedCurveBbox    :: GLBox
  }
makeFields ''CachedCurve

emptyCachedCurve :: CachedCurve
emptyCachedCurve = CachedCurve
  (ObjectId 0) 
  (GPUData undefined undefined 0)
  (GLBox (BBox (V3 0 0 0) (V3 0 0 0)))


data CachedSurface = CachedSurface
  { _cachedSurfaceSceneId :: ObjectId
  , _cachedSurfaceBorders :: [CachedCurve]
  , _cachedSurfaceIsos    :: [CachedCurve]
  }
makeFields ''CachedSurface

emptyCachedSurface :: CachedSurface
emptyCachedSurface = CachedSurface
  (ObjectId 0) [] []


data CachedHulls = CachedHulls
  { _cachedHullsSceneId :: ObjectId
  , _cachedHullsHulls   :: [CachedCurve]
  }
makeFields ''CachedHulls


data CachedCVS = CachedCVS
  { _cachedCVSSceneId :: ObjectId
  , _cachedCVSGpu     :: GPUData
  }
makeFields ''CachedCVS
emptyCachedCVS :: CachedCVS
emptyCachedCVS = CachedCVS
  (ObjectId 0) (GPUData undefined undefined 0)


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
flat2xyz :: Real a =>  [V3 a] -> [Float]
flat2xyz [] = []
flat2xyz (V3 x y z : pts) = 
  realToFrac x : 
  realToFrac y : 
  realToFrac z : flat2xyz pts

-- | while flattening the points for OpenGL it also finds the 
-- vertexes bounding box that is used to ray cast selection.
flat2xyzAndBox :: [Point3d] -> ([Float], GLBox)
flat2xyzAndBox [] = ([], GLBox $ BBox (V3 0 0 0) (V3 0 0 0))
flat2xyzAndBox pts@(p:_) = 
  let (vertices, GLBox (BBox minPt maxPt)) = foldl' flatPtAndBox ([], initBox) pts
      -- Ensure minimum thickness of 1.0 in each dimension
      minPt' = minPt
      maxPt' = V3 (ensureThickness (minPt ^. _x) (maxPt ^. _x))
                  (ensureThickness (minPt ^. _y) (maxPt ^. _y))
                  (ensureThickness (minPt ^. _z) (maxPt ^. _z))
      ensureThickness minVal maxVal = 
        if abs (maxVal - minVal) < 0.001  -- essentially zero
        then minVal + 1.0
        else maxVal
  in (vertices, GLBox $ BBox minPt' maxPt')
  where 
    initBox = GLBox $ BBox p p
    flatPtAndBox (accPts, GLBox bbox) pt =  
      let flatPt (V3 x y z) = [realToFrac x, realToFrac y, realToFrac z]
          newBBox = GLBox $ updateBox pt bbox
      in (accPts <> flatPt pt, newBBox)
