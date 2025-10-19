{-# LANGUAGE TemplateHaskell #-}

module Geometry.File.IGES.TypeEntity where 

import Control.Lens
import Data.Default

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
  , _accepted :: Maybe Bool
  } deriving (Show, Eq)

makeLenses ''Flags128
makeLenses ''Surface128data

ckFlags128 :: Surface128data -> Surface128data
ckFlags128 srf =
  if and [ not $ srf ^. flags . periodicU
         , not $ srf ^. flags . periodicV
         ,       srf ^. flags . polynomial 
         , not $ srf ^. flags . closedU
         , not $ srf ^. flags . closedV
         ]
  then srf & flags . accepted ?~ True
  else srf & flags . accepted ?~ False

instance Default Flags128 where
  def = Flags128
    { _periodicU  = True
    , _periodicV  = True
    , _polynomial = False
    , _closedU    = True
    , _closedV    = True
    , _accepted   = Nothing
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

