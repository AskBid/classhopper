{-# LANGUAGE TypeApplications #-}

module Geometry.File.IGES.Composable.CompositeCurve102 where 

import Control.Lens
import Data.Default

import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Helper
import Geometry.File.IGES.BuilderParameter
import Geometry.File.IGES.ParameterParser.PointersEntity102
import Geometry.File.IGES.Composable.Common 
import Geometry.File.IGES.Composable.Primitive


instance Composable CompositeCurve102 where 
  entityLabel _ = CompositeCurve102_label

  composeEntity de igs = do 
    let pointersEntity = runParameterParser 
                           (pointerP de) 
                           (countPlines de) 
                           igs 
                           pointersEntity102parser
    
    case pointersEntity of 
      Left errPtr -> pure $ Left errPtr 
      
      Right pe -> do 
        let pointers = _curvesPointers pe
            compositeCurve = def
        
        eCrvs <- mapM 
                   (composeFromPointer @Curve126 igs) 
                   pointers

        case catEithers eCrvs of 
          Left ls -> do 
            -- TODO you may want to implement 
            -- all the errors with: 
            -- @unlines . map show ls@
            pure $ Left $ toParseError err126
          
          Right crvs -> do 
            let crv102 = compositeCurve 
                  & curves  .~ crvs
                  & nCurves .~ _nCurvesPointer pe
          
            pure $ Right crv102 

    where 
      err126 = "Curve126 parsing during a CompositeCurve102"
             ++ "composition has failed"
