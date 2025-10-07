module Geometry.Helper where 

chunk :: Int -> [a] -> [[a]]
chunk n xs 
  | n <= length xs = take n xs : chunk n (drop n xs)
  | otherwise      = []

