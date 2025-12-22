{-# LANGUAGE MultiParamTypeClasses #-}

module File.Translate.Class where 

import Data.Text

import Scene.Class

-- | make a type convertible into a type that can be 
-- added to the Scene.
class Convertible a s where 
  convert :: Stageable s => a -> Either Text s
