{-# LANGUAGE TemplateHaskell #-}

-- | All the types related to the IGES file entities.
module Geometry.File.IGES.TypeEntity where 

import Control.Lens
import Data.Default


--------------------------
  --  144
--------------------------
data TrimmedSurface144data = TrimmedSurface144data
  { _trimmedEntity           :: Surface128data
  , _boundaryIsBoundary      :: Bool
    -- ^ this is a legacy thing.. it is always True
    -- is only if sometimes the surface is untrimmed 
    -- but they stil give boudnaries for contruction
  , _countInnerBoundaryLoops :: Int
  , _outerBoundary           :: CurveOnSurface142data
  , _innerBoundaries         :: Maybe [CurveOnSurface142data]
  } -- deriving (Show, Eq)


--------------------------
  --  142
--------------------------
data CurveOnSurface142data = CurveOnSurface142data
  { _curveCreation :: CurveCreation
  , _surface       :: Maybe Surface128data
  -- ^ in theory this surface should already be
  -- represented in the 144 entity the 142 belongs
  -- to. But maybe 142 can be on its own sometime. COS?
  , _curve3D      :: CurveOrComposite
  , _curveUV      :: CurveOrComposite
  , _preferredRep :: PreferredRep
  }

data CurveOrComposite 
  = Composite CompositeCurve102data 
  | SingleCurve RBSplineCurve126data

-- | Preferred method of representation
-- for the curve on surface.
data PreferredRep 
  = RepUnspecified
  | SBt3D
  | CtUV
  | BothEqual
  deriving Show

-- | how was the curve created.
data CurveCreation 
  = CreationUnspecified
  | Projection
  | SurfacesIntersection
  | IsoCurve
  deriving Show


--------------------------
  --  102
--------------------------
data CompositeCurve102data = CompositeCurve102data 
  { _countCurves :: Int
  , _curves      :: [RBSplineCurve126data]
    -- ^ could also be: 
    -- Entity 110 - Line (for straight segments)
    -- Entity 100 - Circular Arc (for arc segments)
    -- Entity 104 - Conic Arc(ellipses, parabolas, hyperbolas)
  }

--------------------------
  --  126
--------------------------
data RBSplineCurve126data = RBSplineCurve126data
  { _variousData128style :: Int
  , _andmore             :: String
  }


--------------------------
  --  128
--------------------------
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

