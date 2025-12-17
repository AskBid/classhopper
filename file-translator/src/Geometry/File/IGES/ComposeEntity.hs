module Geometry.File.IGES.ComposeEntity where 

import Data.Proxy
import Text.Parsec
import Data.Default

import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.BuilderParameter 
import Geometry.File.IGES.ParameterParser.Curve126 
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
  -- ^ not used for now but good to tie the type 
  -- to its tag. for double check pourposes.
  composeEntity 
    :: DirEntry 
    -> SectionedIges 
    -> TranslatorApp (Either ParseError a)

instance Composable Curve126 where 
  entityLabel _ = Curve126_label
  composeEntity de igs = do 
    let result = runParameterParser 
                   (pointerP de) 
                   (countPlines de) 
                   igs 
                   curve126parser 
    case result of 
      Left err  -> pure $ Left err
      Right ent -> pure $ Right ent


