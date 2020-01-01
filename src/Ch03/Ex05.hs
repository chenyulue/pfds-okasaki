-- Exercise 3.5
module Ch03.Ex05 where

import Ch03.BinomialHeap
-- Define findMin directly without calling to removeMinTree
findMin' :: Ord a => BHeap a -> a
findMin' [] = error "Empth Heaps"
findMin' [t] = root t
findMin' (t:ts) = min (root t) (findMin' ts)
