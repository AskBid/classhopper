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
import Geometry.File.IGES.ParameterParser.CompositeCurve102 
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
      Left err -> undefined 
      
      Right pe -> do 
        let pointers = _curvesPointers pe
            compositeCurve = def
        
        eCrvs <- mapM (composeFromPointer @Curve126 igs) pointers

        case catEithers eCrvs of 
          Left ls -> do 
            -- TODO you may want to implement 
            -- all the errors with: 
            -- @unlines . map show ls@
            pure $ Left $ toParseError err
          
          Right crvs -> do 
            let crv102 = compositeCurve 
                  & curves  .~ crvs
                  & nCurves .~ _nCurvesPointer pe
          
            pure $ Right crv102 

    where 
      err = "Curve126 parsing during a CompositeCurve102"
          ++ "composition has failed"


-------------
-- HELPERS 
-------------

-- | given the correct line number, takes care of 
-- the logic to go fish the Directory Entry and feed 
-- it into the composable action for the Entity. 
composeFromPointer 
  :: forall a. Composable a 
  => SectionedIgesLines
  -> SeqNumDE 
  -> TranslatorApp (Either ParseError a)
composeFromPointer igs pointer = do 
    des <- getDirEntries
    case lookup pointer des of 
      Nothing -> do 
        let err = "Directory Entry for line number " 
                <> show pointer
                <> " could not be retrieved." 
        logError $ displayShow err
        pure $ Left $ toParseError err 
      Just de -> composeEntity @a de igs 

-- | once a DE has been composed, it should be 
-- deleted from the DirEntriesMap so that is not 
-- parsed again. KarakiriDE does that.
karakiriDE 
  :: DirEntry 
  -> TranslatorApp ()
karakiriDE de = removeDirEntry $ seqNum de

catEithers :: [ Either a b ] -> Either [a] [b]
catEithers zs = case ls of
  [] -> Right rs
  _  -> Left ls
  where
    ls = [ l | Left  l <- zs ]
    rs = [ r | Right r <- zs ]
