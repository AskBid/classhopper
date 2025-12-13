-- {-# LANGUAGE RecordWildCards #-}

module Geometry.File.IGES.Type where 

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

type FileLine = T.Text
-- | Sequence number is non other than the line
-- number for the section considered.
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
  | TrimmedSurface144
  | CurveOnSurface142
  | CompositeCurve102
  | RBSplineCurve126
  deriving (Show, Eq) 

-- | makes sure we accept only supported 
-- integers for directory entity.
ckEntityType :: Int -> Maybe EntityType
ckEntityType 128 = Just Surface128
ckEntityType 144 = Just TrimmedSurface144
ckEntityType 142 = Just CurveOnSurface142
ckEntityType 102 = Just CompositeCurve102
ckEntityType 126 = Just RBSplineCurve126
--
-- ckEntityType 141 = Boundary141
-- mkEntityType 110 = Line_110
-- mkEntityType 100 = CircularArc_110 (arc segment)
-- mkEntityType 104 = ConicArc_104 (ellipses, parabolas, hyperbolas)
ckEntityType _ = Nothing              
