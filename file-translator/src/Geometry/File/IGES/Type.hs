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
  | CurveOnParametricSurface142
  | Boundary141
  | CompositeCurve102
  | Curve126
  | Line110
  deriving (Show, Eq) 

ckEntityType :: Int -> Maybe EntityType
ckEntityType 128 = Just Surface128
ckEntityType 144 = Just TrimmedSurface144
ckEntityType 142 = Just CurveOnParametricSurface142
-- ckEntityType 102 = CompositeCurve102
--
-- ckEntityType 141 = Boundary141
-- mkEntityType 126 = Curve_126
-- mkEntityType 110 = Line_110
ckEntityType _ = Nothing              
