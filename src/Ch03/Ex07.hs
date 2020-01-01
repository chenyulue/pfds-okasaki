-- Exercise 3.7
{-# LANGUAGE TypeFamilies #-}
module Ch03.Ex07 where

import Ch03.LeftistHeap (Heap(..))
-- findMin that takes only O(1) time by storing the minimum element
-- separately from the rest of the heap
data ExplicitMin h a = E
                     | NE a (h a)
                     deriving (Show, Eq)

instance (Ord a, Heap (h a)) => Heap (ExplicitMin h a) where
  type Elem (ExplicitMin h a) = a
  
  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = NE x (singleton x)

  insert x E = singleton x
  insert x (NE y hp) = NE (min x y) (insert x hp)

  merge E hp = hp
  merge hp E = hp
  merge (NE x hp1) (NE y hp2) = NE (min x y) (merge hp1 hp2)

  findMin E = error "Empty Heaps"
  findMin (NE x _) = x

  deleteMin E = error "Empty Heaps"
  deleteMin (NE _ hp) = NE (findMin hp) (deleteMin hp)
