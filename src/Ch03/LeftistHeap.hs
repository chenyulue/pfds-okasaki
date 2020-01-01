-- 3.1 Leftist Heaps
{-# LANGUAGE TypeFamilies #-}
module Ch03.LeftistHeap where

-- A general typeclass for heaps
class Heap p where
  type Elem p
  empty :: p
  isEmpty :: p -> Bool
  singleton :: Elem p -> p
  insert :: Elem p -> p -> p
  merge :: p -> p -> p
  findMin :: p -> Elem p
  deleteMin :: p -> p

-- A data type for leftist heaps
data LHeap a = Empty
             | Node Int a (LHeap a) (LHeap a)
             deriving (Show, Eq)

instance Ord a => Heap (LHeap a) where
  type Elem (LHeap a) = a

  empty = Empty

  isEmpty Empty = True
  isEmpty _     = False

  singleton x = Node 1 x Empty Empty

  merge Empty h = h
  merge h Empty = h
  merge h1@(Node _ x l1 r1) h2@(Node _ y l2 r2)
    | x < y = makeT x l1 (merge r1 h2)
    | otherwise = makeT y l2 (merge h1 r2)

  insert x h = merge (Node 1 x Empty Empty) h

  findMin Empty = error "Empty Heaps"
  findMin (Node _ x _ _) = x

  deleteMin Empty = error "Empty Heaps"
  deleteMin (Node _ _ l r) = merge l r


-- Some auxiliary functions
rank :: LHeap a -> Int
rank Empty = 0
rank (Node r _ _ _) = r

makeT :: a -> LHeap a -> LHeap a -> LHeap a
makeT x a b =
  if rank a >= rank b
     then Node (rank b + 1) x a b
     else Node (rank a + 1) x b a

-- Exercise 3.1:
-- Since a leftist heap is constructed such that the depth of
-- the left subtree is great than or equal to that of the right
-- subtree, the right spine, which is the shortest depth of the
-- heap, has the largest length when the heap is a complete
-- balanced tree. In such a case, the depth is the number of
-- elements along the right spine, which is fllor(log(n+1))
