{-# LANGUAGE OverloadedStrings #-}

module Geometry.File.IGES.ParameterParser.PointersEntity142
  ( pointersEntity142parser 
  ) where 

import Data.Text
import Text.Parsec
import Data.Default
import Control.Monad (when)
import Control.Lens

import Geometry.File.IGES.Type 
import Geometry.File.IGES.TypeEntity
import Geometry.File.IGES.ParameterParser.Common

-- | using user state from the parser to build up the 
-- return Surface type.
type Parser142 a = Parsec Parameter PointersEntity142 a

pointersEntity142parser :: Parser142 PointersEntity142
pointersEntity142parser = do 
  modifyState (const def)

  et <- entity 
  when (et /= CurveOnSurface142_label) $
    fail "Expected entity 142 but got something else"

  cc    <- parseCurveCreation
  srf   <- num
  crv3d <- num
  crvUV <- num
  pr    <- parsePreferredRep
  
  modifyState 
    $ (curveCreationPtr .~ cc)
    . (surfacePtr       .~ srf)
    . (curve3DPtr       .~ crv3d)
    . (curveUVPtr       .~ crvUV)
    . (preferredRepPtr  .~ pr)

  getState  


parseCurveCreation :: Parser142 CurveCreation
parseCurveCreation = do
  tok <- anyToken
  case getCurveCreation tok of
    Nothing -> fail "Could not retrieve Curve Creation"
    Just cc -> return cc  

getCurveCreation :: Text -> Maybe CurveCreation 
getCurveCreation "0" = Just CreationUnspecified 
getCurveCreation "1" = Just Projection 
getCurveCreation "2" = Just SurfacesIntersection
getCurveCreation "3" = Just IsoCurve 
getCurveCreation _   = Nothing 


parsePreferredRep :: Parser142 PreferredRep
parsePreferredRep = do
  tok <- anyToken
  case getPreferredRep tok of
    Nothing -> fail "Could not retrieve Curve Creation"
    Just cc -> return cc

getPreferredRep :: Text -> Maybe PreferredRep 
getPreferredRep "0" = Just RepUnspecified 
getPreferredRep "1" = Just SBt3D 
getPreferredRep "2" = Just CtUV
getPreferredRep "3" = Just BothEqual 
getPreferredRep _   = Nothing
