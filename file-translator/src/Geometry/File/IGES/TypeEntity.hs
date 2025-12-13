{-# LANGUAGE TemplateHaskell #-}

-- | All the types related to the IGES file entities.
module Geometry.File.IGES.TypeEntity where 

import Control.Lens
import Data.Default


--------------------------
  --  144
--------------------------
data TrimmedSurface144 = TrimmedSurface144d
  { _trimmedEntity           :: Surface128
  , _boundaryIsBoundary      :: Bool
    -- ^ this is a legacy thing.. it is always True
    -- is only if sometimes the surface is untrimmed 
    -- but they stil give boudnaries for contruction
  , _countInnerBoundaryLoops :: Int
  , _outerBoundary           :: CurveOnSurface142
  , _innerBoundaries         :: Maybe [CurveOnSurface142]
  } -- deriving (Show, Eq)


--------------------------
  --  142
--------------------------
data CurveOnSurface142 = CurveOnSurface142
  { _curveCreation :: CurveCreation
  , _surface       :: Maybe Surface128
  -- ^ in theory this surface should already be
  -- represented in the 144 entity the 142 belongs
  -- to. But maybe 142 can be on its own sometime. COS?
  , _curve3D      :: CurveOrComposite
  , _curveUV      :: CurveOrComposite
  , _preferredRep :: PreferredRep
  }

data CurveOrComposite 
  = Composite CompositeCurve102
  | SingleCurve RBSplineCurve126

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
data CompositeCurve102 = CompositeCurve102
  { _countCurves :: Int
  , _curves      :: [RBSplineCurve126]
    -- ^ could also be: 
    -- Entity 110 - Line (for straight segments)
    -- Entity 100 - Circular Arc (for arc segments)
    -- Entity 104 - Conic Arc(ellipses, parabolas, hyperbolas)
  }

--------------------------
  --  126
--------------------------
data RBSplineCurve126 = RBSplineCurve126
  { _variousData128style :: Int
  , _andmore             :: String
  }


--------------------------
  --  128
--------------------------
data Surface128 = Surface128
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
makeLenses ''Surface128

instance Default Flags128 where
  def = Flags128
    { _periodicU  = True
    , _periodicV  = True
    , _polynomial = False
    , _closedU    = True
    , _closedV    = True
    }

instance Default Surface128 where
  def = Surface128
    { _degreeU       = 0
    , _degreeV       = 0
    , _knotsU        = []
    , _knotsV        = []
    , _weights       = []
    , _controlPoints = []
    , _flags         = def  -- uses Default Flags128
    }

