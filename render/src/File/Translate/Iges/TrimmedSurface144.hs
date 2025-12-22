{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module File.Translate.Iges.TrimmedSurface144 where 

import Control.Lens

import Geometry.File.IGES.TypeEntity
import Geometry.Surface  
import File.Translate.Class


instance Convertible TrimmedSurface144 Surface where 
  convert TrimmedSurface144{_trimmedEntity = s128} 
    | not (validate s128) =
        Left "Flags check failed. (Surface128)"
    | otherwise =
        case mkBSpline (s128 ^. degreeU)
                       (s128 ^. knotsU)
                       (s128 ^. degreeV)
                       (s128 ^. knotsV)
                       (s128 ^. controlPoints) of
          Nothing  -> Left err
          Just srf ->
            if s128 ^. polynomial
              then Right srf
              else case mkNURBS srf (s128 ^. weights) of
                Nothing    -> Left err
                Just nurbs -> Right nurbs

    where 
      validate :: Surface128 -> Bool
      validate s0 =
        and [ not $ s0 ^. periodicU
            , not $ s0 ^. periodicV
            , not $ s0 ^. closedU
            , not $ s0 ^. closedV 
            ]
      err = "IGES surface128 failed to convert to Scene."
