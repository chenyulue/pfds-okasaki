-- 3.2 Binomial Heaps
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Ch03.BinomialHeap where

import Ch03.LeftistHeap (Heap(..))

type Rank = Int
data BTree a = Node Rank a [BTree a] deriving (Show, Eq)

type BHeap a = [BTree a]

instance Ord a => Heap (BHeap a) where
  type Elem (BHeap a) = a

  empty = []

  isEmpty = null

  singleton x = [Node 0 x []]

  insert x ts = insTree (Node 0 x []) ts

  merge [] ts = ts
  merge ts [] = ts
  merge h1@(t1:ts1) h2@(t2:ts2) =
    case compare (rank t1) (rank t2) of
      LT -> t1 : merge ts1 h2
      GT -> t2 : merge h1 ts2
      EQ -> insTree (link t1 t2) (merge ts1 ts2)

  findMin ts = let (t, _) = removeMinTree ts in root t

  deleteMin ts =
    let (Node _ _ ts1, ts2) = removeMinTree ts
     in merge (reverse ts1) ts2

  
-- Some auxiliary functions
link :: Ord a => BTree a -> BTree a -> BTree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2)
  | x1 <= x2 = Node (r+1) x1 (t2:c1)
  | otherwise = Node (r+1) x2 (t1:c2)

rank :: BTree a -> Int
rank (Node r _ _) = r

root :: BTree a -> a
root (Node _ x _) = x

insTree :: Ord a => BTree a -> BHeap a -> BHeap a
insTree t [] = [t]
insTree t ts@(t':ts')
  | rank t < rank t' = t : ts
  | otherwise = insTree (link t t') ts'

removeMinTree :: Ord a => BHeap a -> (BTree a, BHeap a)
removeMinTree [] = error "Empty Heaps"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
  let (t', ts') = removeMinTree ts
   in if root t < root t' then (t, ts) else (t', t:ts')
