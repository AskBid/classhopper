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

data Section = Start | Global | Directory | Parameter | Terminate
  deriving (Show, Eq, Ord)

type SeqNumRawLine = IM.IntMap T.Text

type IgesRaw = M.Map Section SeqNumRawLine

data DirEntry = DirEntry
  { dirSeqNum   :: Int
  , entityType  :: Int
  , pointerP    :: Int
  , nLinesP     :: Int
  }

