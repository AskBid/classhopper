{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Geometry.File.IGES.Composable.Common where 

import Text.Parsec
import Data.Proxy
import RIO (logError, displayShow)
import Prelude hiding (lookup)
import Data.IntMap.Strict (lookup)

import Geometry.File.IGES.Helper
import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.TranslatorAppType

-- | A Parser parses the Text cells of the entity in 
-- Parameter section. but then we need to compose what 
-- was parsed into the actual entity. Sometime the 
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



-------------
-- HELPERS 
-------------

-- | this is a Helper for the Helpers.. looks after
-- the Directory Entry collection from a Sequence Number
-- aka Line Number of the Directory Entry Section.
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



catEithers :: [ Either a b ] -> Either [a] [b]
catEithers zs = case ls of
  [] -> Right rs
  _  -> Left ls
  where
    ls = [ l | Left  l <- zs ]
    rs = [ r | Right r <- zs ]
