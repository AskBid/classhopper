module Geometry.Helper where 

-- | Chunks array, returns only completed sub arrays.
chunk :: Int -> [a] -> [[a]]
chunk n xs 
  | n <= length xs = take n xs : chunk n (drop n xs)
  | otherwise      = []

-- | Safe indexing
(!?) :: [a] -> Int -> Maybe a
(!?) xs i =
  if i < 0 || i >= length xs
  then Nothing
  else Just (xs !! i)
