-- Exercise 3.6
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ch03.Ex06 where

import Ch03.LeftistHeap (Heap(..))

-- Reimplement binomial heaps with a new data type, which has no field
-- for the rank annotations.
data BTree a = Node a [BTree a] deriving (Eq, Show)

type Rank = Int
type BHeap a = [(Rank, BTree a)]

instance Ord a => Heap (BHeap a) where
  type Elem (BHeap a) = a

  empty = []

  isEmpty = null

  singleton x = [(0, Node x [])]

  insert x ts = insTree (Node x []) ts

  merge ts [] = ts
  merge [] ts = ts
  merge ts1@(t1':ts1') ts2@(t2':ts2') =
    case compare (fst t1') (fst t2') of
      LT -> t1' : merge ts1' ts2
      GT -> t2' : merge ts1 ts2'
      EQ -> (fst t1' + 1, link (snd t1') (snd t2')) : merge ts1' ts2'

  findMin ts = let (t, _) = removeMinTree ts in root t

  deleteMin ts =
    let (Node _ ts1, ts2) = removeMinTree ts
     in merge (zip [0..] $ reverse ts1) ts2
  
-- Some auxiliary functions
rank :: BTree a -> Int
rank (Node _ ts) = length ts

root :: BTree a -> a
root (Node x _) = x

link :: Ord a => BTree a -> BTree a -> BTree a
link t1@(Node x1 c1) t2@(Node x2 c2)
  | x1 <= x2 = Node x1 (t2:c1)
  | otherwise = Node x2 (t1:c2)

insTree :: Ord a => BTree a -> BHeap a -> BHeap a
insTree t [] = [(rank t, t)]
insTree t ts@(t':ts')
  | let r = rank t, r < fst t' = (r, t) : ts
  | otherwise = insTree (link t (snd t')) ts'

removeMinTree :: Ord a => BHeap a -> (BTree a, BHeap a)
removeMinTree [] = error "Empty Heaps"
removeMinTree [t] = (snd t, [])
removeMinTree (t:ts) =
  let (t', ts') = removeMinTree ts
      t'' = snd t 
   in if root t'' <= root t'
         then (t'', ts)
         else (t', t:ts')
