{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Ch02.Ex06 where

import Prelude hiding (lookup)
import Ch02.BST (Tree(..))

type Map k v = Tree (k, v)
-- A finite map
class FiniteMap p where
  type Key p
  type Elem p
  empty :: p
  bind :: Key p -> Elem p -> p -> p
  lookup :: Key p -> p -> Maybe (Elem p)
  
instance (Ord k, Eq v) => FiniteMap (Tree (k, v)) where
  type Key (Tree (k, v)) = k
  type Elem (Tree (k, v)) = v
  
  empty = Empty
  
  -- If the keys equal but the values don't, update the value with the new one.
  bind k v mp = either id id $ loop k v mp 
    where
      loop k v Empty = Right $ Node Empty (k,v) Empty 
      loop k v (Node l y@(k',v') r) = 
        case compare k k' of
          LT -> (\z -> Node z y r) <$> loop k v l 
          GT -> Node l y <$> loop k v r
          EQ -> if v == v' then Left mp else Right (Node l (k',v) r)
  
  -- For lookup function, if the key is not found, return Nothing,
  -- else return Just a, instead of using an exception.
  lookup _ Empty = Nothing
  lookup key (Node l (k,v) r) = 
    case compare key k of 
      LT -> lookup key l 
      GT -> lookup key r
      EQ -> Just v



































      