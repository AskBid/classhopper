-- | This module is probably only temporary
-- used in the development stage to visualise
-- experiment and understand how to use Bounding
-- Boxes to select elements in the viewport/scene.
module Scene.GLBox where

import Linear.V3
import Control.Lens

import Scene.Scene
import Data.Map
import Scene.GPU
import Scene.Common
import Geometry.Point
-- data CachedBox = CachedBox 
--   { _cachedBoxSceneId :: ObjectId

getSceneCurves :: Scene -> [CachedCurve]
getSceneCurves sc = do 
  let bords = concatMap ((^. borders) . snd) (toList (sc ^. cachedSRFS))
      isos' = concatMap ((^. isos) . snd) (toList (sc ^. cachedSRFS))
  bords <> isos' 

-- | given a a list of GPU curves, creates a single
-- VBOVAO with all the bounding box edges.
cacheBoxes :: [CachedCurve] -> IO [CachedCurve]
cacheBoxes crvs = do 
  let ptsBoxes = fromBBox2pts . (^. bbox) <$> crvs 
      floatsBoxes = (\pts -> (flat2xyz pts, fromIntegral (length pts))) <$> ptsBoxes
  mapM mkCurve floatsBoxes
  where 
    mkCurve (floats, n) = do 
      (vbo, vao) <- cacheVBOVAO floats 
      let box = GLBox $ BBox (V3 0 0 0) (V3 0 0 0)
      pure $ emptyCachedCurve 
               & sceneId .~ ObjectId 0
               & gpu     .~ GPUData vbo vao n
               & bbox    .~ box

-- | create pts for all bounding box edges.
--            8-------7 max
--           /|     / |
--         /  |   /   | 
--       /    5-/-----6
--     2-------3     /   z     y
--     |   /   |   /     |   /
--     | /     | /       | / 
-- min 1-------4         |------x
fromBBox2pts :: GLBox -> [Point3d]
fromBBox2pts (GLBox (BBox (V3 minX minY minZ) (V3 maxX maxY maxZ))) =
    let pt1 = V3 minX minY minZ  -- bottom-front-left
        pt2 = V3 maxX minY minZ  -- bottom-front-right
        pt3 = V3 maxX maxY minZ  -- bottom-back-right
        pt4 = V3 minX maxY minZ  -- bottom-back-left
        pt5 = V3 minX minY maxZ  -- top-front-left
        pt6 = V3 maxX minY maxZ  -- top-front-right
        pt7 = V3 maxX maxY maxZ  -- top-back-right
        pt8 = V3 minX maxY maxZ  -- top-back-left
     in [ pt1, pt2, pt2, pt3, pt3, pt4, pt4, pt1  -- bottom face
        , pt5, pt6, pt6, pt7, pt7, pt8, pt8, pt5  -- top face
        , pt1, pt5, pt2, pt6, pt3, pt7, pt4, pt8  -- vertical edges
        ]
