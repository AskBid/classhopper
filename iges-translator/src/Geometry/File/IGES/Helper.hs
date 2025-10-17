module Geometry.File.IGES.Helper where 

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
import Data.Text.Read (decimal)

textToInt :: T.Text -> Maybe Int
textToInt txt = 
  case decimal (T.takeWhile isDigit (T.strip txt)) of
    Left _       -> Nothing
    Right (n, _) -> Just n

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0     = Nothing
  | otherwise = listToMaybe (drop i xs)

splitText :: Char -> T.Text -> [T.Text]
splitText sep = T.split (== sep)

textToChar :: T.Text -> Maybe Char
textToChar t
  | T.length t == 1 = Just (T.head t)
  | otherwise       = Nothing

safeInit :: T.Text -> Maybe T.Text
safeInit t
  | T.null t  = Nothing
  | otherwise = Just $ T.init t
