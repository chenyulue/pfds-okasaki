-- 3.3 Red-Black Trees
{-# LANGUAGE TypeFamilies #-}
module Ch03.RBTree where

import Ch02.BST (Set(..))

data Color = R | B deriving (Show, Eq)

data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)
              deriving (Eq, Show)

instance Ord a => Set (RBTree a) where
  type Elem (RBTree a) = a

  empty = Empty

  member _ Empty = False
  member x (Node _ l y r) =
    case compare x y of
      LT -> member x l
      GT -> member x r
      EQ -> True

  insert v tr =
    let ins Empty = Node R Empty v Empty
        ins s@(Node color a y b) =
          case compare v y of
            LT -> balance color (ins a) y b
            GT -> balance color a y (ins b)
            EQ -> s
        Node _ a y b = ins tr
    in Node B a y b

-- Auxiliary functions
balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (Node R (Node R a x b) y c) z d
  = Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d
  = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d)
  = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d))
  = Node R (Node B a x b) y (Node B c z d)
balance color l z r = Node color l z r



-- Exercise 3.8:
-- Prove that the maximum depth of a node in a red-black tree
-- of size n is at most 2*floor(log(n+1))
-- Answer:
-- The maximum depth of a red-black tree is the root-null path that
-- contains the most number of nodes. According to the Invariant 2,
-- the maximum number of black nodes in any root-null path is restricted
-- by the number of nodes in the shortest root-null path. Assume the
-- shortest root-null path has depth k, then there are k + 1 nodes in the
-- root-null path at depth k, and there is a full and complete binary
-- subtree of depth k. Suppose the full and complete binary subtree has
-- n nodes, then using the fact that n = 2^(k+1) - 1, the number of nodes
-- in the root-null path is logarithmic in n, that is k + 1 = log(n+1).
-- Due to the Invariant 2, the maximum number of black nodes in any root-null
-- path is log(n+1). So the longest root-null path will have log(n+1) black
-- nodes (the maximum that it can have due to Invariant 2). By Invariant 1,
-- a red node can only have black node children, which restricts the maximum
-- number of red nodes to be placed between the black nodes. So the maximum
-- number of red nodes in a root-null path is contrained by the maximum
-- number of black nodes, which is log(n+1). Therefore, the length of the
-- longest root-null path is max black nodes plus max red nodes, which is
-- 2*log(n+1)
