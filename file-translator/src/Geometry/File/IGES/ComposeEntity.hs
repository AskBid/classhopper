{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.IGES.ComposeEntity where 

import Control.Lens
import Data.Proxy
import Data.Default
import Data.IntMap.Strict
import Prelude hiding (lookup)
import Text.Parsec
import RIO
  ( logInfo
  , logError
  , display, displayShow
  )

import Geometry.File.IGES.Helper (toParseError)
import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.BuilderParameter 
import Geometry.File.IGES.ParameterParser.Surface128 
import Geometry.File.IGES.ParameterParser.Curve126 
import Geometry.File.IGES.ParameterParser.PointersEntity102 
import Geometry.File.IGES.ParameterParser.PointersEntity142 
import Geometry.File.IGES.ParameterParser.PointersEntity144 
import Geometry.File.TranslatorAppType 
  ( TranslatorApp(..)
  , TranslatorEnv(..)
  , removeDirEntry 
  , getDirEntries
  )

-- | A Parser parses the Text cells of the entity in 
-- Parameter section/form. but then we need to compose 
-- what was parsed into the actual entity. (Sometime the 
-- parser already composes the Entity as it is not 
-- referential.
-- SectionedIges -> DirEntriesMap ->
-- DirEntry -> Parameter -> Entity 
class Composable a where
  entityLabel 
    :: Proxy a 
    -> EntityType_label
  -- ^ tie the type to its tag. Proxy use is to provide 
  -- type information, even though there is no value 
  -- available of that type
  composeEntity 
    :: DirEntry 
    -> SectionedIgesLines 
    -> TranslatorApp (Either ParseError a)



instance Composable Surface128 where 
  entityLabel _ = Surface128_label 

  composeEntity de igs = do 
    let result = runParameterParser 
                   (pointerP de) 
                   (countPlines de) 
                   igs 
                   surface128parser 
    karakiriDE de
    case result of 
      Left err  -> pure $ Left err
      Right ent -> pure $ Right ent



instance Composable Curve126 where 
  entityLabel _ = Curve126_label

  composeEntity de igs = do 
    let result = runParameterParser 
                   (pointerP de) 
                   (countPlines de) 
                   igs 
                   curve126parser 
    karakiriDE de
    case result of 
      Left err  -> pure $ Left err
      Right ent -> pure $ Right ent



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



instance Composable TrimmedSurface144 where 
  entityLabel _ = TrimmedSurface144_label

  composeEntity de igs = do 
    let pointersEntity = runParameterParser 
                           (pointerP de) 
                           (countPlines de) 
                           igs 
                           pointersEntity144parser
    pure $ Right def



-------------
-- HELPERS 
-------------
getDEfromPointer 
  :: SectionedIgesLines
  -> SeqNumDE 
  -> TranslatorApp (Either ParseError DirEntry)
getDEfromPointer igs pointer = do 
    des <- getDirEntries

    case lookup pointer des of 
      Nothing -> do 
        let err = "Directory Entry for line number " 
                <> show pointer
                <> " could not be retrieved." 
        logError $ displayShow err
        pure $ Left $ toParseError err 

      Just de -> pure $ Right de 

-- | given the correct line number, takes care of 
-- the logic to go fish the Directory Entry and feed 
-- it into the composable action for the Entity. 
composeFromPointer 
  :: forall a. Composable a 
  => SectionedIgesLines
  -> SeqNumDE 
  -> TranslatorApp (Either ParseError a)
composeFromPointer igs pointer = do 
  eDE <- getDEfromPointer igs pointer
  case eDE of 
    Left err -> pure $ Left err
    Right de -> composeEntity @a de igs 

-- | once a DE has been composed, it should be 
-- deleted from the DirEntriesMap so that is not 
-- parsed again. KarakiriDE does that.
karakiriDE 
  :: DirEntry 
  -> TranslatorApp ()
karakiriDE de = removeDirEntry $ seqNum de


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


catEithers :: [ Either a b ] -> Either [a] [b]
catEithers zs = case ls of
  [] -> Right rs
  _  -> Left ls
  where
    ls = [ l | Left  l <- zs ]
    rs = [ r | Right r <- zs ]
