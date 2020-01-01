module Ch02.Ex05 where

import Ch02.BST

type Depth = Int
type Size = Int
-- (a) A complete binary tree of depth d with x stored in every node
complete :: a -> Depth -> Tree a
complete _ 0 = Empty
complete x d = 
  let subtree = complete x (d - 1)
   in Node subtree x subtree
   
-- (b) A balanced tree of arbitrary size, with any node of which 
-- two subtrees should differ in size by at most one.
balanced :: a -> Size -> Tree a 
balanced _ 0 = Empty
balanced x n = 
  let m = div n 2
      subtr1 = balanced x m 
      subtr2 = balanced x (n-1-m)
   in Node subtr1 x subtr2