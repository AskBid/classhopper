module Geometry.File.IGES.Composable.Primitive where 

import Geometry.File.IGES.Type
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.BuilderParameter
import Geometry.File.IGES.ParameterParser.Curve126
import Geometry.File.IGES.ParameterParser.Surface128
import Geometry.File.IGES.Composable.Common

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
