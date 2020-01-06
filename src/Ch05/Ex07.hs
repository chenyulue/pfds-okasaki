-- Exercise 5.7
module Ch05.Ex07 where

import Ch05.SplayHeap
import Ch03.LeftistHeap (Heap (..))

-- Write a sorting function that inserts elements into a splay tree
-- and then performs an inorder traversal of the tree, dumping the
-- elements into a list.
sort :: Ord a => [a] -> [a]
sort = toList . foldr insert Empty

toList :: Ord a => SplayTree a -> [a]
toList Empty = []
toList (Node a x b) = toList a ++ [x] ++ toList b 
