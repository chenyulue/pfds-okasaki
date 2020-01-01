-- Exercise 3.2
module Ch03.Ex02 where

import Ch03.LeftistHeap 

-- Define insert directly without calling to merge
insert' :: Ord a => a -> LHeap a -> LHeap a
insert' v Empty = Node 1 v Empty Empty
insert' v h@(Node _ x l r)
  | v < x = Node 1 v h Empty
  | otherwise = makeT x l (insert' v r)
