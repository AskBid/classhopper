{-# LANGUAGE TypeApplications #-}

module Geometry.File.IGES.Composable.ComposeCurveOrComposite where

import Text.Parsec

import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Helper
import Geometry.File.TranslatorAppType
import Geometry.File.IGES.Composable.Common
import Geometry.File.IGES.Composable.Primitive
import Geometry.File.IGES.Composable.CompositeCurve102

-- | Helper function to compose either a single curve or 
-- composite curve based on what the pointer references
composeCurveOrComposite 
  :: SectionedIgesLines
  -> SeqNumDE 
  -> TranslatorApp (Either ParseError CurveOrComposite)
composeCurveOrComposite igs pointer = do 
  eDE <- getDEfromPointer igs pointer

  case eDE of 
    Left err -> pure $ Left err 

    Right de -> do 
      case entityType de of 
        CompositeCurve102_label -> do   
          eComp <- composeEntity @CompositeCurve102 de igs
          case eComp of
            Left err   -> pure $ Left err
            Right comp -> pure $ Right $ Composite comp

        Curve126_label -> do 
          eCrv <- composeEntity @Curve126 de igs
          case eCrv of
            Left err  -> pure $ Left err
            Right crv -> pure $ Right $ SingleCurve crv
        
        _ -> do
          let err = "Expected Curve126 or CompositeCurve102" 
                  <> " at pointer " 
                  <> show pointer
                  <> " but found entity type: "
                  <> show (entityType de)
          pure $ Left $ toParseError err
