-- 5.4 Splay Heaps
module SplayHeap where

import Ch03.LeftistHeap (Heap (..))

data SplayTree a = Empty
                 | Node (SplayTree a) a (SplayTree a)
                 deriving (Show, Eq)

instance Ord a => Heap (SplayTree a) where
  type Elem (SplayTree a) = a

  insert x t = Node (smaller x t) x (bigger x t)

bigger :: Ord a => a -> SplayTree a -> SplayTree a
bigger _ Empty = Empty
bigger pivot (Node a x b)
  | x <= pivot = bigger pivot b
  | otherwise =
      case a of
        Empty -> Node Empty x b
        Node a1 y a2 | y <= pivot -> Node (bigger pivot a2) x b
                     | otherwise -> Node (bigger pivot a1) y (Node a2 x b)



