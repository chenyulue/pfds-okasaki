-- Exercise 5.8
{-# LANGUAGE TypeFamilies #-}
module Ch05.Ex08 where

import Ch03.LeftistHeap (Heap(..))
import Ch05.PairingHeap

data BinTree a = E
               | T a (BinTree a) (BinTree a)
               deriving (Eq, Show)

toBinary :: Ord a => PairingHeap a -> BinTree a
toBinary hs = loop hs []
  where
    loop Empty _ = E
    loop (Node x []) [] = T x E E
    loop (Node x []) (r:rs) = T x E (loop r rs)
    loop (Node x (c:cs)) [] = T x (loop c cs) E
    loop (Node x (c:cs)) (r:rs) = T x (loop c cs) (loop r rs)


instance Ord a => Heap (BinTree a) where
  type Elem (BinTree a) = a

  empty = E
  isEmpty E = True
  isEmpty _ = False

  singleton x = T x E E

  findMin E = error "Empty Heap"
  findMin (T x _ _) = x

  merge E h = h
  merge h E = h
  merge h1@(T x1 c1 E) h2@(T x2 c2 E)
    | x1 < x2 = T x1 (T x2 c2 c1) E
    | otherwise = T x2 (T x1 c1 c2) E

  insert x h = merge (singleton x) h

  deleteMin E = error "Empty Heap"
  deleteMin (T a c E) = mergePairs c
    where
      mergePairs E = E
      mergePairs (T x1 c1 (T x2 c2 s2)) = merge
                 (merge (T x1 c1 E) (T x2 c2 E))
                 (mergePairs s2)
      mergePairs h = h
  
