-- 5.2 Queues
{-# LANGUAGE TypeFamilies #-}
module Ch05.Queue where

-- A general type class for queues. We use the traditional queue nomenclature
-- (empty, isEmpty, peek, enq, deq, etc) instead of the list nomenclature in
-- order to avoid the name conflicts.
class Queue q where
  type Elem q
  empty :: q             -- Construct a empty queue
  isEmpty :: q -> Bool   -- Check if the queue is empty
  peek :: q -> Elem q    -- Get the element at the front of the queue without removing it
  enq :: q -> Elem q -> q   -- Add an item to the rear of the queue
  deq :: q -> q             -- Remove an item from the front of the queue

data PairQueue a = Q {
  front :: [a],
  rear :: [a]
  } deriving Eq

instance Show a => Show (PairQueue a) where
  show (Q f r) = "Q: " ++ show f ++ show (reverse r)

instance Queue (PairQueue a) where
  type Elem (PairQueue a) = a
  
  empty = Q [] []
  isEmpty = null . front

  peek (Q [] _) = error "Empty Queue"
  peek (Q (x:f) r) = x
  
  enq (Q f r) x = checkf (Q f (x:r))
  
  deq (Q [] _) = error "Empty Queue"
  deq (Q (x:f) r) = checkf (Q f r)

checkf :: PairQueue a -> PairQueue a
checkf (Q [] r) = Q (reverse r) []
checkf q = q
