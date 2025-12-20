{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geometry.File.IGES.RunIgesReader 
  ( getIgesEntities
  ) where 

import Data.Default
import Data.Proxy
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import Text.Parsec
import RIO ( logInfo
           , logError
           , displayShow
           , display
           , asks
           ) 


import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.Type
import Geometry.File.IGES.Composable.Common
import Geometry.File.IGES.Composable.Primitive
import Geometry.File.IGES.Composable.TrimmedSurface144
import Geometry.File.IGES.BuilderSectionedIges 
          (readIGESfile, buildSectionedIges)
import Geometry.File.IGES.BuilderDirectory 
          (buildDEs)
import Geometry.File.IGES.ParameterParser.Surface128 
          (surface128parser)
import Geometry.File.TranslatorAppType 
          ( TranslatorApp(..)
          , TranslatorEnv(..)
          , removeDirEntry 
          , getDirEntries
          )


-- | only working for Surface128data for now. As it is 
-- the only one supported as of writing, but it may need 
-- polymorphism later. the @Maybe@ is in case the parameter 
-- text wasn't retrievable. 
-- the @Either@ is for parsing errors.
getIgesEntities 
  :: FilePath 
  -> TranslatorApp IgesScene
getIgesEntities location = do 

  logInfo $ "Reading IGES file: " 
          <> displayShow location
  rawLines <- liftIO $ readIGESfile location
  logInfo $ "Loaded " 
          <> displayShow (length rawLines) 
          <> " raw lines"

  let igs = buildSectionedIges rawLines
  logInfo "Built raw IGES map"

  buildDEs igs
  
  -- Here is important the order of processing DEs 
  -- most structured entity first. 
  -- If child entity are consumed first they will be 
  -- deleted and missing when parents look for them.
  eComposed144s <- processDEs @TrimmedSurface144 igs
  eComposed128s <- processDEs @Surface128 igs
  -- TODO missing COS on non trimmed surfaces. (142)
  eComposed126s <- processDEs @Curve126 igs

  logInfo "Finished Composing Entities from Directory Entries + Parameters."
  
  trimSrfs <- logAndFilter eComposed144s
  srfs     <- logAndFilter eComposed128s
  crvs     <- logAndFilter eComposed126s
  
  pure $ IgesScene srfs crvs trimSrfs


-- | consumes the Entity type given trying to compose 
-- it into the corresponding Entity.
processDEs 
  :: forall a. Composable a
  => SectionedIgesLines 
  -> TranslatorApp [Either ParseError a]
processDEs igs = go []
  where
    go acc = do
      let lab = entityLabel (Proxy @a)
      -- ^ defined in each type Composable class instance.
      -- Given the type a, tell me which IGES entity label 
      -- it corresponds to.
      mde <- popWhere (\de -> entityType de == lab)
      case mde of
        Nothing -> pure (reverse acc)
        Just de -> do
          pe <- composeEntity de igs
          go (pe : acc)


-- | given a predicate, returns nothing if it finds
-- an object from Map, 
-- deletes that element from the map if found.
popWhere
  :: (DirEntry -> Bool)
  -> TranslatorApp (Maybe DirEntry)
popWhere p = do
  ref <- asks teDirEntries
  liftIO $ atomicModifyIORef' ref func
  where 
    func m = case IM.minViewWithKey (IM.filter p m) of
      Nothing           -> (m, Nothing)
      Just ((k, de), _) -> (IM.delete k m, Just de)
    -- ^ the map goes to modify IORef and a result is 
    -- given,

logAndFilter :: [Either ParseError a] -> TranslatorApp [a] 
logAndFilter es = do 
  -- logInfo $ displayShow $ length builtE126s
  -- let params = onlyRights eParams
  -- logInfo $ "Attempted " 
  --         <> displayShow (length []) 
  --         <> " parameters parsing"
  -- logInfo $ "Successful parsings: " 
  --         <> displayShow (length [])
  pure $ onlyRights es 


onlyRights :: [Either b a] -> [a]
onlyRights [] = []
onlyRights (eba:ebas) = case eba of
  Right a -> a : onlyRights ebas
  Left _  -> onlyRights ebas
