{-# LANGUAGE TemplateHaskell #-}

-- | All the types related to the IGES file entities.
module Geometry.File.IGES.TypeEntity where 

import Control.Lens
import Data.Default

data TrimmedSurface144data = TrimmedSurface144data
  { _trimmedEntity :: Surface128data
  , _boundaryIsBoundary :: Bool
    -- ^ this is a legacy thing.. it is always True
    -- is nly if sometimes the surface is untrimmed 
    -- but they stil give boudnaries for contruction
  , _countInnerBoundaryLoops :: Int
  , _outerBoundary :: CurveOnParametricSurface142data
  , _innerBoundaries :: [CurveOnParametricSurface142data]
  } -- deriving (Show, Eq)

data Boundary141data = Boundary141data 
 { _undefined141 :: ()
 }

data CurveOnParametricSurface142data = 
  CurveOnParametricSurface142data
  { _undefined142 :: ()
  }

data Surface128data = Surface128data
  { _degreeU :: Int
  , _degreeV :: Int
  , _knotsU  :: [Double]
  , _knotsV  :: [Double]
  , _weights :: [Double]
  , _controlPoints :: [Double]
  , _flags :: Flags128
  } deriving (Show, Eq)

data Flags128 = Flags128 
  { _periodicU  :: Bool 
  , _periodicV  :: Bool 
  , _polynomial :: Bool
  , _closedU :: Bool 
  , _closedV :: Bool 
  } deriving (Show, Eq)

makeLenses ''Flags128
makeLenses ''Surface128data

instance Default Flags128 where
  def = Flags128
    { _periodicU  = True
    , _periodicV  = True
    , _polynomial = False
    , _closedU    = True
    , _closedV    = True
    }

instance Default Surface128data where
  def = Surface128data
    { _degreeU       = 0
    , _degreeV       = 0
    , _knotsU        = []
    , _knotsV        = []
    , _weights       = []
    , _controlPoints = []
    , _flags         = def  -- uses Default Flags128
    }

