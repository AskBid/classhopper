{-# LANGUAGE TypeApplications #-}

module Geometry.File.IGES.Composable.TrimmedSurface144 where 

import Data.Default
import Control.Lens

import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.BuilderParameter
import Geometry.File.IGES.ParameterParser.PointersEntity144
import Geometry.File.IGES.Composable.Common
import Geometry.File.IGES.Composable.Primitive
import Geometry.File.IGES.Composable.CurveOnSurface142


instance Composable TrimmedSurface144 where 
  entityLabel _ = TrimmedSurface144_label

  composeEntity de igs = do 
    let pointersEntity = runParameterParser 
                           (pointerP de) 
                           (countPlines de) 
                           igs 
                           pointersEntity144parser
    case pointersEntity of 
      Left errPtr -> pure $ Left errPtr

      Right pe -> do
        let srfPtr     = pe ^. trimmedEntityPtr
            isBound    = pe ^. boundaryIsBoundaryPtr
            countInner = pe ^. countInnerBoundariesPtr
            outerPtr   = pe ^. outerBoundaryPtr
            innerPtrs  = pe ^. innerBoundariesPtr 
        
        eSrf <- composeFromPointer @Surface128 igs srfPtr

        eCos <- composeFromPointer @CurveOnSurface142 igs outerPtr

        -- TODO I have not implemented inner boundaries yet 
        -- complete inner boundaries 
        case (eSrf, eCos) of
          (Right srf, Right cos) -> do
            let srf144 = def
                  & trimmedEntity        .~ srf
                  & boundaryIsBoundary   .~ isBound 
                  & countInnerBoundaries .~ countInner 
                  & outerBoundary        .~ cos 
                  & innerBoundaries      .~ [] -- TODO
            pure $ Right srf144

          (Left err, _) -> pure $ Left err
          (_, Left err) -> pure $ Left err
