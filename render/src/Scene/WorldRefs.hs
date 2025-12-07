module Scene.WorldRefs where 

import Linear.V3

import qualified Graphics.Rendering.OpenGL as GL
import Scene.GPU
import Data.IORef (mkWeakIORef)
import Scene.Common (ObjectId(..))

data Axes = Axes 
  { xWorld :: CachedCurve 
  , yWorld :: CachedCurve 
  , zWorld :: CachedCurve
  }

newtype Grid = Grid CachedCurve

mkWorldRefs :: Int -> Int -> IO (Axes, Grid)
mkWorldRefs unit amount = do 
  grid <- mkGrid unit amount 
  axes <- mkAxes (unit * amount)
  pure (axes, grid)

mkAxes :: Int -> IO Axes 
mkAxes len = do 
  let x = [0, 0, 0, l, 0, 0] 
      y = [0, 0, 0, 0, l, 0]
      z = [0, 0, 0, 0, 0, l/2]
  (xvbo, xvao) <- cacheVBOVAO x
  (yvbo, yvao) <- cacheVBOVAO y
  (zvbo, zvao) <- cacheVBOVAO z
  pure $ Axes 
    { xWorld = CachedCurve (ObjectId 0) xvbo xvao 2 
    , yWorld = CachedCurve (ObjectId 0) yvbo yvao 2
    , zWorld = CachedCurve (ObjectId 0) zvbo zvao 2
    }
  where 
    l = fromIntegral len / 2

mkGrid :: Int -> Int -> IO Grid
mkGrid unit amount = do 
  let steps       = [-size, -size + step .. size] 
      xParallels  = concatMap (\y -> [V3 (-size) y 0, V3 size y 0]) steps
      yParallels  = concatMap (\x -> [V3 x (-size) 0, V3 x size 0]) steps
      allLinesPts = xParallels <> yParallels
      vertices    = pts2flattenXYZvertices allLinesPts 
      ptCount     = fromIntegral $ length allLinesPts
  (vbo, vao) <- cacheVBOVAO vertices
  pure $ Grid $ CachedCurve (ObjectId 0) vbo vao ptCount
  where 
    step = fromIntegral unit
    size = (step * fromIntegral amount) / 2
