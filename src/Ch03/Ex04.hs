-- Exercise 3.4
{-# LANGUAGE TypeFamilies #-}
module Ch03.Ex04 where

import Ch03.LeftistHeap

-- Weight-biased leftist heaps
-- (a) It's the same reason as Exercise 3.1
-- (b) An implementation of heaps using weight-biased leftist heap
data WHeap a = E
             | T Int a (WHeap a) (WHeap a)
             deriving (Show, Eq)

instance Ord a => Heap (WHeap a) where
  type Elem (WHeap a) = a

  empty = E

  isEmpty E = True
  isEmpty _ = False

  singleton x = T 1 x E E

  merge h E = h
  merge E h = h
  merge h1@(T _ x l1 r1) h2@(T _ y l2 r2)
    | x < y = makeTW x l1 (merge r1 h2)
    | otherwise = makeTW y l2 (merge h1 r2)

  insert x h = merge (singleton x) h

  findMin E = error "Empty Heaps"
  findMin (T _ x _ _) = x

  deleteMin E = error "Empty Heaps"
  deleteMin (T _ _ l r) = merge l r

-- (c) merge in a single top-down pass
merge' :: Ord a => WHeap a -> WHeap a -> WHeap a
merge' E h = h
merge' h E = h
merge' h1@(T w1 x l1 r1) h2@(T w2 y l2 r2)
  | x < y = if weight r1 + w2 <= weight l1
               then T (w1+w2) x l1 (merge' r1 h2)
               else T (w1+w2) x (merge' r1 h2) l1
  | otherwise = if weight r2 + w1 <= weight l2
                   then T (w1+w2) y l2 (merge' h1 r2)
                   else T (w1+w2) y (merge' h1 r2) l2

-- (d) Advantage in a lazy environments:
--     Advantage in a concurrent environment:

-- Some auxiliary functions
weight :: WHeap a -> Int
weight E = 0
weight (T s _ _ _) = s

makeTW :: a -> WHeap a -> WHeap a -> WHeap a
makeTW x a b =
  if weight a >= weight b
     then T (1 + weight a + weight b) x a b
     else T (1 + weight a + weight b) x b a
