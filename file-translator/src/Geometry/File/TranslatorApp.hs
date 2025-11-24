module Geometry.File.TranslatorApp where 

import Control.Monad.Reader

import Geometry.File.TranslatorAppType
import Geometry.File.IGES.RunIgesReader (getIgesEntities)
import Geometry.File.IGES.TypeEntity (Surface128data)
import qualified Data.Text as T

openFile :: FilePath -> IO [Surface128data]
openFile filepath = do 
  let logger = putStrLn . T.unpack 
  -- ^ you can change the logger to different types..
  -- or even \_ -> pure () for not logging at all.
  let env = Env logger
  runReaderT (getIgesEntities filepath) env

