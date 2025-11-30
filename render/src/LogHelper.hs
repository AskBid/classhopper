module LogHelper where 

import Geometry.File.IGES.TypeEntity
import Type 
import RIO

logSurface :: Surface128data -> RenderApp ()
logSurface s0 = do 
  logInfo $ displayShow 
              [ show (s0 ^. degreeU)
              , show (s0 ^. degreeV)
              , show (length (s0 ^. knotsU))
              , show (s0 ^. knotsU)
              , show (length (s0 ^. knotsV))
              , show (s0 ^. knotsV)
              , show (length (s0 ^. controlPoints))
              , show (s0 ^. controlPoints)
              ]
  return ()
