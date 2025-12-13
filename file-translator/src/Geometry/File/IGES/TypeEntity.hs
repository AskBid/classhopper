{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-} 
{-# LANGUAGE FunctionalDependencies #-}

-- | All the types related to the IGES file entities.
module Geometry.File.IGES.TypeEntity where 

import Control.Lens
import Data.Default

--------------------------
  --  128
--------------------------
data Surface128 = Surface128
  { _surface128DegreeU :: Int
  , _surface128DegreeV :: Int
  , _surface128KnotsU  :: [Double]
  , _surface128KnotsV  :: [Double]
  , _surface128Weights :: [Double]
  , _surface128ControlPoints :: [Double]
  , _surface128PeriodicU  :: Bool 
  , _surface128PeriodicV  :: Bool 
  , _surface128Polynomial :: Bool
  , _surface128ClosedU    :: Bool 
  , _surface128ClosedV    :: Bool
  } deriving (Show, Eq)

makeFields ''Surface128

instance Default Surface128 where
  def = Surface128
    { _surface128DegreeU       = 0
    , _surface128DegreeV       = 0
    , _surface128KnotsU        = []
    , _surface128KnotsV        = []
    , _surface128Weights       = []
    , _surface128ControlPoints = []
    , _surface128PeriodicU  = True
    , _surface128PeriodicV  = True
    , _surface128Polynomial = False
    , _surface128ClosedU    = True
    , _surface128ClosedV    = True
    }


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
  { _rBSplineCurve126DegreeU :: Int
  , _rBSplineCurve126KnotsU  :: [Double]
  , _rBSplineCurve126Weights :: [Double]
  , _rBSplineCurve126ControlPoints :: [Double]
  , _rBSplineCurve126PeriodicU  :: Bool 
  , _rBSplineCurve126Polynomial :: Bool
  , _rBSplineCurve126ClosedU    :: Bool 
  } deriving (Show, Eq)

makeFields ''RBSplineCurve126

