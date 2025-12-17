{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Geometry.File.TranslatorAppType where

import RIO
import RIO.Text (Text)
import qualified Data.IntMap.Strict as IM

import Geometry.File.IGES.Type

-- LogFunc is just a function that performs logging.
data TranslatorEnv = TranslatorEnv
  { teLogFunc    :: LogFunc
  , teDirEntries :: IORef DirEntriesMap
    -- ^ why IORef? bc RIO is a ReaderT not a StateT 
    -- it is not writable, hence we need to introduce a IORef.
    -- you could  StateT DirEntriesMap (RIO TranslatorEnv)
    -- but then need a lot of lift, 
    -- RIO discourages to have two environments.
  }
  -- so RIO gives us this type to which our defined log 
  -- function needs to comply to.

-- RIO uses a typeclass to retrieve the LogFunc
instance HasLogFunc TranslatorEnv where
  logFuncL = lens teLogFunc (\x y -> x { teLogFunc = y })
  -- A lens is just a pair of functions:
  --   Getter – get the value from a record
  --   Setter – update the value in a record
  -- Getter: \env -> teLogFunc env 
  -- ^ gets the logger
  -- Setter: \env newLog -> env { teLogFunc = newLog }
  -- ^ updates the logger
  -- lens :: (s -> a) -> (s -> a -> s) -> Lens' s a 
  -- s as structure, a as the element inside it

-- | Your application monad is now "RIO TranslatorEnv a"
-- The monad is RIO, which is just ReaderT env IO with helper functions
type TranslatorApp a = RIO TranslatorEnv a
-- ^ newtype RIO env a = RIO {unRIO :: ReaderT env IO a}
--
-- type TranslatorApp a = RIO TranslatorEnv a 
-- is basically just:
-- ReaderT env IO

getDirEntries :: TranslatorApp DirEntriesMap
getDirEntries = do
  ref <- asks teDirEntries
  readIORef ref

putDirEntries :: DirEntriesMap -> TranslatorApp ()
putDirEntries m = do
  ref <- asks teDirEntries
  writeIORef ref m

modifyDirEntries :: (DirEntriesMap -> DirEntriesMap) -> TranslatorApp ()
modifyDirEntries f = do
  ref <- asks teDirEntries
  atomicModifyIORef' ref (\m -> (f m, ()))

removeDirEntry :: IM.Key -> TranslatorApp ()
removeDirEntry k = modifyDirEntries $ IM.delete k 
