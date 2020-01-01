-- Exercise 3.3
module Ch03.Ex03 where

import Ch03.LeftistHeap

-- fromList that produces a leftist heap from an unordered list
fromList :: Ord a => [a] -> LHeap a
fromList [] = Empty
fromList [x] = singleton x
fromList (x1:x2:xs) = head . mergePair . map singleton $ xs
  where
    mergePair (h1:h2:hs) = mergePair $ merge h1 h2 : mergePair hs
    mergePair hs = hs
