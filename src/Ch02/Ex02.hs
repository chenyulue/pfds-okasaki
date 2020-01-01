module Ch02.Ex02 where

import Ch02.BST

-- Rewrite member to take no more than d + 1 comparisons by keeping track of a candidate
-- element that might be equal to the query element and checking for equality only when
-- the bottom of the tree is hit.
member' :: Ord a => a -> Tree a -> Bool
member' v t = loop Nothing v t
  where
    loop candidate x Empty = Just x == candidate
    loop candidate x (Node l y r)
      | x < y = loop candidate x l
      | otherwise = loop (Just y) x r
