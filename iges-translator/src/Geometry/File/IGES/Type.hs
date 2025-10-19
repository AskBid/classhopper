-- {-# LANGUAGE RecordWildCards #-}

module Geometry.File.IGES.Type where 

import Geometry.Surface
import Geometry.Curve
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Text.Parsec.Prim (Reply(Error))

-- | to transfer the scene to the Scene in bezier-rnder module.
data SceneFromIGES = SceneFromIGES
  { surfaces  :: [Surface]
  , curves    :: [Curve]
  , unit      :: Float
  , maxSize   :: Int 
  , tolerance :: Float
  }

type FileLine = T.Text
type SeqNumDE = Int
type SeqNumP = Int

data Section 
  = Start 
  | Global 
  | Directory 
  | Parameter 
  | Terminate
  deriving (Show, Eq, Ord)

type SeqNumRawLine = IM.IntMap T.Text

type IgesRaw = M.Map Section SeqNumRawLine

data DirEntry = DirEntry
  { dirSeqNum   :: SeqNumDE
  , entityType  :: EntityType
  , pointerP    :: SeqNumP
  , countPlines :: Int
  } deriving Show

type Parameter = [T.Text]

data EntityType 
  = Surface128 
  | TrimSurface144
  | CompCurve102
  | Curve126
  | Line110
  | LoopOfCurves141
  deriving (Show, Eq) 

ckEntityType :: Int -> Maybe EntityType
ckEntityType 128 = Just Surface128
-- mkEntityType 144 = TrimSurface_144
-- mkEntityType 102 = CompCurve_102
-- mkEntityType 126 = Curve_126
-- mkEntityType 110 = Line_110
-- mkEntityType 141 = LoopOfCurves_141
ckEntityType _ = Nothing

data Surface128data = Surface128data
  { degreeU :: Int
  , degreeV :: Int
  , knotsU  :: [Double]
  , knotsV  :: [Double]
  , weights  :: [Double]
  , controlPoints :: [Double]
  , flags :: Flags128
  } deriving (Show, Eq)

data Flags128 = Flags128 
  { periodicU  :: Bool 
  , periodicV  :: Bool 
  , polynomial :: Bool
  , closedU :: Bool 
  , closedV :: Bool 
  , accepted :: Maybe Bool
  } deriving (Show, Eq)

ckFlags128 :: Surface128data -> Surface128data 
ckFlags128 srf@(Surface128data {flags = f}) =
  let check Flags128 
        { periodicU  = False 
        , periodicV  = False 
        , polynomial = True 
        , closedU    = False 
        , closedV    = False 
        , accepted   = _
        }     = f {accepted = Just True}
      check _ = f {accepted = Just False}
   in srf {flags = check f}
                     
