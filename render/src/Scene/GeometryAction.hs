{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
        vtxs = flat2xyz pts
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
        count = fromIntegral $ length pts  
        (vertices, box) = flat2xyzAndBox pts
        -- needed for draw call
    (vbo, vao) <- GPU.cacheVBOVAO vertices
    pure $ emptyCachedCurve
             & sceneId .~ gcId 
             & gpu     .~ GPUData vbo vao count
             & bbox    .~ box

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
             & borders .~ [b1,b2,b3,b4]
             & isos    .~ [i1,i2]
    where 
      cachedCrv :: S.DirectionUV -> Parameter -> IO CachedCurve
      cachedCrv dir param = do
        let pts = S.sampleIsocrv dir param samplingAmount gsDef
            count = fromIntegral $ length pts
            (vertices, box) = flat2xyzAndBox pts
        (vbo, vao) <- GPU.cacheVBOVAO vertices
        pure $ emptyCachedCurve
                 & sceneId .~ gsId
                 & gpu     .~ GPUData vbo vao count
                 & bbox    .~ box
