{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

-- | Scene Geometries interface with the GPU
-- via Type Classes that transforms them in VBO VAO 
-- keeping a relationship thanks to the ObjectIds.
module Scene.GeometryAction where 

import Data.Map (insert)
import Control.Lens

import Scene.Common (ObjectId)
import qualified Geometry.Surface as S
import qualified Geometry.Curve as C
import Scene.Class 
import qualified Scene.GPU as GPU
import Scene.GPU
import Scene.Scene

import Geometry.Point
import qualified Graphics.Rendering.OpenGL as GL

samplingAmount :: Int 
samplingAmount = 9

-- | Takes an exisiting GeometrySurface in the Scene 
-- and add the VBO VAO of the control points - hulls.
instance GeometryHandle GeometrySurface where 
  showHandle GeometrySurface{..} scene@Scene{..} = do
    let pts = concat $ S.getPoints (S.cvs gsDef)
        count = fromIntegral $ length pts
        vtxs = pts2flattenXYZvertices pts
    (vbo, vao) <- GPU.cacheVBOVAO vtxs 
    let cvs = emptyCachedCVS
          & sceneId .~ gsId
          & gpu     .~ GPUData vbo vao count
    pure $ scene & cachedCVS %~ insert gsId cvs

instance Stageable S.Surface where 
  addToScene srf scene = do
    objId <- nextId (scene ^. idCounterRef)
    let gs = GeometrySurface objId srf
    let scene' = scene & geometrySRFS %~ insert objId gs
    ts <- tessellate gs
    pure $ scene' & cachedSRFS %~ insert objId ts

instance Stageable C.Curve where 
  addToScene crv scene = do
    objId <- nextId $ (scene ^. idCounterRef)
    let gc = GeometryCurve objId crv
        -- %~ modify
        scene' = scene & geometryCRVS %~ insert objId gc
    tc <- tessellate gc
    pure $ scene' & cachedCRVS %~ insert objId tc

instance Tessellatable GeometryCurve GPU.CachedCurve where
  tessellate GeometryCurve{..} = do
    let pts = C.sampleCrv gcDef samplingAmount
        verticesFloats = pts2flattenXYZvertices pts
        count = fromIntegral $ length pts  
        -- needed for draw call
    (vbo, vao) <- GPU.cacheVBOVAO verticesFloats
    pure $ emptyCachedCurve
             & sceneId .~ gcId 
             & gpu     .~ GPUData vbo vao count

-- | TODO it probably does not need to be 4 different VBOVAo 
-- all borders can be in one.. but could be useful when it comes 
-- to selections.
instance Tessellatable GeometrySurface GPU.CachedSurface where
  tessellate GeometrySurface{..} = do
    b1 <- cachedCrv S.U 0 
    b2 <- cachedCrv S.U 1 
    b3 <- cachedCrv S.V 0 
    b4 <- cachedCrv S.V 1 
    i1 <- cachedCrv S.U 0.5
    i2 <- cachedCrv S.V 0.5
    pure $ emptyCachedSurface
             & sceneId .~ gsId 
             & surfaceBorders .~ [b1,b2,b3,b4]
             & surfaceIsos    .~ [i1,i2]
    where 
      cachedCrv dir param = do
        let pts = S.sampleIsocrv dir param samplingAmount gsDef
            verticesFloats = pts2flattenXYZvertices pts
            vtxCount = fromIntegral $ length pts
        (vbo, vao) <- GPU.cacheVBOVAO verticesFloats
        pure $ emptyCachedCurve
                 & sceneId .~ gsId
                 & gpu     .~ GPUData vbo vao vtxCount

-- verticesBBox :: BBox -> [Float]
-- verticesBBox (BBox (Point3d xmin ymin zmin) (Point3d xmax ymax zmax)) =
--   pts2flattenXYZvertices points
--   where points = [ Point3d x y z 
--               | x <- [xmin, xmax]
--               , y <- [ymin, ymax]
--               , z <- [zmin, zmax]
--               ]
--
-- vbovaoBBox :: BBox -> IO (GL.BufferObject, GL.VertexArrayObject)
-- vbovaoBBox box = cacheVBOVAO $ verticesBBox BBox
