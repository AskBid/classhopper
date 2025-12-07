{-# LANGUAGE MultiParamTypeClasses #-}

module Scene.Class where 

import Scene.Scene

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

-- | Used to show in the scene a GPU representation 
-- of the control pooints of a Geometry.
class GeometryHandle a where 
  showHandle :: a -> Scene -> IO Scene 
  -- hideHandle :: a -> Scene -> Scene 
