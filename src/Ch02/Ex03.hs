module Ch02.Ex03 where

import Ch02.BST

-- Rewrite insert using exceptions to avoid unnessary copying. In Haskell, the Either data type is used instead of
-- exceptions, with Left indicating an exception.
insert' :: Ord a => a -> Tree a -> Tree a
insert' v t = either id id $ loop v t 
  where
    loop x Empty = Right (Node Empty x Empty)
    loop x (Node l y r)
      | x < y = (\z -> Node z y r) <$> loop x l 
      | x > y = Node l y <$> loop x r
      | otherwise = Left t