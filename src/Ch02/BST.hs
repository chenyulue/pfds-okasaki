-- 2.2 Binary Search Trees
{-# LANGUAGE TypeFamilies #-}
module Ch02.BST where

-- A general typeclass of sets
class Set p where
  type Elem p
  empty :: p
  insert :: Elem p -> p -> p
  member :: Elem p -> p -> Bool

-- An implementation of sets using binary search trees.
data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance Ord a => Set (Tree a) where
  type Elem (Tree a) = a
  
  empty = Empty

  member x Empty =  False
  member x (Node l y r)
    | x < y = member x l
    | x > y = member x r
    | otherwise = True

  insert x Empty = Node Empty x Empty
  insert x t@(Node l y r)
    | x < y = Node (insert x l) y r
    | x > y = Node l y (insert x r)
    | otherwise = t
