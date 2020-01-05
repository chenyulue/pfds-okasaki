-- Exercise 4.2
module Ch04.Ex02 where

-- Implement insertion sort on streams
sort :: Ord a => [a] -> [a]
sort [] = []
sort (x:xs) = ins x $ sort xs
  where
    ins x [] = [x]
    ins x ys'@(y:ys)
      | x <= y = x : ys'
      | otherwise = y : ins x ys
