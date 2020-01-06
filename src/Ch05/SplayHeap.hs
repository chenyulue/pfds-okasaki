-- 5.4 Splay Heaps
{-# LANGUAGE TypeFamilies #-}
module Ch05.SplayHeap where

import Ch03.LeftistHeap (Heap (..))

data SplayTree a = Empty
                 | Node (SplayTree a) a (SplayTree a)
                 deriving (Show, Eq)

instance Ord a => Heap (SplayTree a) where
  type Elem (SplayTree a) = a

  empty = Empty
  isEmpty Empty = True
  isEmpty _ = False

  singleton x = Node Empty x Empty

  insert x t = let (a, b) = partition x t in Node a x b

  merge Empty t = t
  merge (Node a x b) t =
    let (ta, tb) = partition x t
     in Node (merge ta a) x (merge tb b)

  findMin Empty = error "Empty Splay Heap" 
  findMin (Node Empty x _) = x
  findMin (Node a _ _) = findMin a

  deleteMin Empty = error "Empty Splay Heap"
  deleteMin (Node Empty x b) = b
  deleteMin (Node (Node Empty x b) y c) = Node b y c
  deleteMin (Node (Node a x b) y c) = Node (deleteMin a) x (Node b y c)

bigger :: Ord a => a -> SplayTree a -> SplayTree a
bigger _ Empty = Empty
bigger pivot (Node a x b)
  | x <= pivot = bigger pivot b
  | otherwise =
      case a of
        Empty -> Node Empty x b
        Node a1 y a2 | y <= pivot -> Node (bigger pivot a2) x b
                     | otherwise -> Node (bigger pivot a1) y (Node a2 x b)

-- Exercise 5.4
-- Implement smaller.
smaller :: Ord a => a -> SplayTree a -> SplayTree a
smaller _ Empty = Empty
smaller pivot (Node a x b)
  | x > pivot = smaller pivot a
  | otherwise =
      case b of
        Empty -> Node a x Empty
        Node b1 y b2 | y > pivot -> Node a x (smaller pivot b1)
                     | otherwise -> Node (Node a x b2) y (smaller pivot b2)

partition :: Ord a => a -> SplayTree a -> (SplayTree a, SplayTree a)
partition _ Empty = (Empty, Empty)
partition pivot t@(Node a x b)
  | x <= pivot =
      case b of
        Empty -> (t, Empty)
        Node b1 y b2 | y <= pivot -> let (small, big) = partition pivot b2
                                      in (Node (Node a x b1) y small, big)
                     | otherwise -> let (small, big) = partition pivot b1
                                     in (Node a x small, Node big y b2)
  | otherwise =
      case a of
        Empty -> (Empty, t)
        Node a1 y a2 | y <= pivot -> let (small, big) = partition pivot a2
                                      in (Node a1 y small, Node big x b)
                     | otherwise -> let (small, big) = partition pivot a1
                                     in (small, Node big y (Node a2 x b))
