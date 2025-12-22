module Geometry.File.IGES.Composable.CurveOnSurface142 where 

import Control.Lens
import Data.Default

import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.BuilderParameter
import Geometry.File.IGES.ParameterParser.PointersEntity142
import Geometry.File.IGES.Composable.Common
import Geometry.File.IGES.Composable.ComposeCurveOrComposite

instance Composable CurveOnSurface142 where 
  entityLabel _ = CurveOnSurface142_label

  composeEntity de igs = do 
    let pointersEntity = runParameterParser 
                           (pointerP de) 
                           (countPlines de) 
                           igs 
                           pointersEntity142parser
    case pointersEntity of 
      Left errPtr -> pure $ Left errPtr 
      
      Right pe -> do 
        let srfPtr    = pe ^. surfacePtr
            crv3dPtr  = pe ^. curve3DPtr
            crvUvPtr  = pe ^. curveUVPtr
            preferred = pe ^. preferredRepPtr
            creation  = pe ^. curveCreationPtr
        
        -- TODO only in cases where there is a curve 
        -- on surface for an untrimmed surface, or 
        -- perhaps a non bounndary cos for a trimmed 
        -- surface, we need to bound this cos to that 
        -- surface but if we do compose it here for record 
        -- sake we will delete it from the state. 
        -- Since we don't care for now of non boundary COS, 
        -- we will skip to record on the COS's sruface for 
        -- now as it is in theory always a composition coming 
        -- from a 144 surface.
        -- I think it is fine to just enable this.. because 144 
        -- will take and delete the 128 before 142. So if that's 
        -- the case here will just turn into a Left err... if 144 
        -- did not take the 128 and this 142 is jsut a COS not a 
        -- boundary, the 128 will be found and we'll get a Right.
        -- eSrf <- composeFromPointer @Surface128 igs srfPtr
        
        -- Compose the 3D curve 
        -- (either Curve126 or CompositeCurve102)
        eCrv3D <- composeCurveOrComposite igs crv3dPtr
        
        -- Compose the UV curve 
        -- (either Curve126 or CompositeCurve102)
        eCrvUV <- composeCurveOrComposite igs crvUvPtr

        case (eCrv3D, eCrvUV) of
          (Right crv3d, Right crvUv) -> do
            let cos142 = def
                  & curveCreation .~ creation
                  & surface       .~ Nothing
                  & curve3D       .~ crv3d
                  & curveUV       .~ crvUv
                  & preferredRep  .~ preferred
            
            pure $ Right cos142
          
          (Left err, _) -> pure $ Left err
          (_, Left err) -> pure $ Left err
