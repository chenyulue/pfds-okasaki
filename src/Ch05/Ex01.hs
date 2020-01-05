-- Exercise 5.1
{-# LANGUAGE TypeFamilies #-}
module Ch05.Ex01 where

import Ch05.Queue (PairQueue(..))
-- Implement a double-ended queue abstraction, which allows reads and
-- writes to both ends of the queue
class DeQueue q where
  type Elem q
  empty :: q
  isEmpty :: q -> Bool
-- insert, remove, and inspect the fron element   
  pushFront :: Elem q -> q -> q
  popFront :: q -> q 
  getFront :: q -> Elem q
-- insert, remove, and inspect the back element
  pushBack :: q -> Elem q -> q
  popBack :: q -> q 
  getBack :: q -> Elem q 

instance DeQueue (PairQueue a) where
  type Elem (PairQueue a) = a

  empty = Q [] []
  isEmpty = null . front

  pushFront x (Q f r) = checkFR (Q (x:f) r)
  popFront (Q [] _) = error "Empty Queue"
  popFront (Q (x:f) r) = checkFR (Q f r)
  getFront (Q [] _) = error "Empty Queue"
  getFront (Q (x:f) r) = x

  pushBack (Q f r) x = checkFR (Q f (x:r))
  popBack (Q _ []) = error "Empty Queue"
  popBack (Q f (x:r)) = checkFR (Q f r)
  getBack (Q _ []) = error "Empty Queue"
  getBack (Q f (x:r)) = x


checkFR :: PairQueue a -> PairQueue a
checkFR (Q [] r) = Q (reverse . snd . splitHalf $ r) (fst . splitHalf $ r)
checkFR (Q f []) = Q (fst . splitHalf $ f) (reverse . snd . splitHalf $ f)
checkFR q = q

splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt (length xs `div` 2) xs


