{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Type where

import RIO
import RIO.Text (Text)

-- LogFunc is just a function that performs logging.
data RenderEnv = RenderEnv
  { teLogFunc :: LogFunc
  , curvePrecision :: Int -- Unique to RenderEnv.
  }
  -- You do NOT share the actual Env type.
  -- You only share the interface ("this env can log" with HasLogFunc).

-- RIO uses a typeclass to retrieve the LogFunc
instance HasLogFunc RenderEnv where
  logFuncL = lens teLogFunc (\x y -> x { teLogFunc = y })
  -- look in Geometry.File.TranslatorAppType for more info

-- | Your application monad is now "RIO TranslatorEnv a"
-- The monad is RIO, which is just ReaderT env IO with helper functions
type RenderApp a = RIO RenderEnv a
-- newtype RIO env a = RIO {unRIO :: ReaderT env IO a}
