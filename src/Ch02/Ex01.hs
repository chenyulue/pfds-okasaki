-- Exercise 2.1:
-- Write a function suffixes of type [a] -> [[a]] that takes a list xs and returns a list of
-- all the suffixes of xs in decreasing order of length. For example,
-- suffixes [1, 2, 3, 4] = [[1,2,3,4], [2,3,4], [3,4], [4], []]
module Ch02.Ex01 where

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes s@(_:xs) = s : suffixes xs
