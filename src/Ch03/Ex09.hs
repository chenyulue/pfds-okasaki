-- Exercise 3.9
module Ch03.Ex09 (
  fromOrdList,
  module Ch03.RBTree) where

import Ch03.RBTree

-- Write a function fromOrdList that converts a sorted list with no
-- duplicates into a red-black tree
-- Algorithm:
-- To construct a red-black tree from a linked list in O(n) time complexity,
-- take the left n/2 nodes and recursively contruct the left subtree, where n
-- is the length of the list. After left subtree is constructed, use the remaining
-- list to construct the root, which is the first element of the remaining list,
-- and recursively construct the right subtree.
fromOrdList :: Ord a => [a] -> RBTree a
fromOrdList xs =
  let (t, _, _) = sortedListToTree (length xs) xs
   in t

type Depth = Int    -- Indicating a black depth of a tree

type Size = Int     -- Indicating the number of nodes

sortedListToTree :: Ord a => Size -> [a] -> (RBTree a, Depth, [a])
sortedListToTree 0 xs = (Empty, 0, xs)                     -- Construct a tree without any nodes.
sortedListToTree 1 (x:xs) = (Node B Empty x Empty, 1, xs)  -- Construct a tree with one black node.
sortedListToTree n xs =
  let (leftNum, rightNum) = (div n 2, n - 1 - div n 2)
      (leftTree, leftDepth, y:ys) = sortedListToTree leftNum xs
      (rightTree, rightDepth, zs) = sortedListToTree rightNum ys
-- If the black depths of left and right subtrees are not equal, it means
-- the Invariant 2 is violated, and then change the root node of left subtree into
-- a red node to reduce the black depth, since if the depths are not equal, the
-- depth of left subtree is one more than that of right subtree.
   in (Node B (if leftDepth == rightDepth then leftTree else paint R leftTree) 
              y rightTree,                                                     
       rightDepth + 1, zs)

paint :: Color -> RBTree a -> RBTree a
paint _ Empty = Empty
paint c (Node _ l x r) = Node c l x r
