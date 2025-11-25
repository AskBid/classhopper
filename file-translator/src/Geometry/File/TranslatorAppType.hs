{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Geometry.File.TranslatorAppType where

import Control.Monad.Reader
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Simple environment with a logger function
newtype Env = Env
  { logFunc :: Text -> IO () 
  }

type TranslatorApp a = ReaderT Env IO a

-- Logging helper
logInfo :: Text -> TranslatorApp ()
logInfo msg = do
  Env logger <- ask
  liftIO $ logger msg

logMaybe :: Maybe a -> Text -> Text -> TranslatorApp ()
logMaybe maybe ok err = do 
  Env logger <- ask
  case maybe of
    Nothing -> liftIO $ logger err
    Just _  -> liftIO $ logger ok 
