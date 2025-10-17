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

mkEntityType :: Int -> Maybe EntityType
mkEntityType 128 = Just Surface128
-- mkEntityType 144 = TrimSurface_144
-- mkEntityType 102 = CompCurve_102
-- mkEntityType 126 = Curve_126
-- mkEntityType 110 = Line_110
-- mkEntityType 141 = LoopOfCurves_141
mkEntityType _ = Nothing
