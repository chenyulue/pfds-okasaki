-- 5.5 Pairing Heaps
{-# LANGUAGE TypeFamilies #-}
module Ch05.PairingHeap where

import Ch03.LeftistHeap (Heap (..))

data PairingHeap a = Empty
                   | Node a [PairingHeap a]
                   deriving (Eq, Show)

instance Ord a => Heap (PairingHeap a) where
  type Elem (PairingHeap a ) = a

  empty = Empty
  isEmpty Empty = True
  isEmpty _     = False

  singleton x = Node x []

  findMin Empty = error "Empty Heaps"
  findMin (Node x hs) = x

  merge h Empty = h
  merge Empty h = h
  merge h1@(Node x hs1) h2@(Node y hs2)
    | x <= y = Node x (h2:hs1)
    | otherwise = Node y (h1:hs2)

  insert x h = merge (singleton x) h

  deleteMin Empty = error "Empty Heaps"
  deleteMin (Node _ hs) = mergePairs hs

mergePairs :: Ord a => [PairingHeap a] -> PairingHeap a
mergePairs [] = Empty
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)
