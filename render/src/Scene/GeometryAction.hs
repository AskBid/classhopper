{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FlexibleInstances     #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- | Scene Geometries interface with the GPU
-- via Type Classes that transforms them in VBO VAO 
-- keeping a relationship thanks to the ObjectIds.
module Scene.GeometryAction where 

import Data.Map (insert)

import Scene.Common (ObjectId)
import qualified Geometry.Surface as S
import qualified Geometry.Curve as C
import Scene.Class 
import qualified Scene.GPU as GPU
import Scene.GPU (pts2flattenXYZvertices)
import Scene.Scene

samplingAmount :: Int 
samplingAmount = 9

-- | Takes an exisiting GeometrySurface in the Scene 
-- and add the VBO VAO of the control points - hulls.
instance GeometryHandle GeometrySurface where 
  showHandle GeometrySurface{..} scene@Scene{..} = do
    let pts = concat $ S.getPoints (S.cvs gsDef)
        vtxs = pts2flattenXYZvertices pts
    (vbo, vao) <- GPU.cacheVBOVAO vtxs 
    let newCachedCVS = GPU.CachedCVS { ccvId = gsId
                         , ccvVBO = vbo 
                         , ccvVAO = vao 
                         , ccvVertexCount = fromIntegral $ length pts
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

instance Tessellatable GeometryCurve GPU.CachedCurve where
  tessellate GeometryCurve{..} = do
    let pts = C.sampleCrv gcDef samplingAmount
        verticesFloats = pts2flattenXYZvertices pts
        vertexCount = length pts  
        -- needed for draw call
    (vbo, vao) <- GPU.cacheVBOVAO verticesFloats
    return $ GPU.CachedCurve
      { ccId = gcId
      , ccVBO = vbo
      , ccVAO = vao
      , ccVertexCount = fromIntegral vertexCount
      }

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
    return $ GPU.CachedSurface
      { csId = gsId
      , csBorders = [b1,b2,b3,b4]
      , csIsoCrvs = [i1,i2]
      }
    where 
      cachedCrv dir param = do
        let pts = S.sampleIsocrv dir param samplingAmount gsDef
            verticesFloats = pts2flattenXYZvertices pts
            vertexCount = length pts
        (vbo, vao) <- GPU.cacheVBOVAO verticesFloats
        return $ GPU.CachedCurve 
          { ccId  = gsId
          , ccVBO = vbo 
          , ccVAO = vao
          , ccVertexCount = fromIntegral vertexCount
          }
