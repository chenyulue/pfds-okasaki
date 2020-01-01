module Ch02.Ex04 where

import Ch02.BST

-- Rewrite insert that performs no unnessary copying and uses no more than d + 1 comparisons

insert'' :: Ord a => a -> Tree a -> Tree a 
insert'' v t = either id id $ loop Nothing v t 
  where
    loop candidate x Empty 
      | Just x == candidate = Left t 
      | otherwise = Right (Node Empty x Empty)
    loop candidate x (Node l y r)
      | x < y = (\z -> Node z y r) <$> loop candidate x l 
      | otherwise = Node l y <$> loop (Just y) x r