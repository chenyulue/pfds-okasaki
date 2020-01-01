-- 2.1 Lists:
-- Implement the data structure of Stack, and the stack nomenclature (push, peek, pop, etc) is
-- used instead of list nomenclature in order to avoid the name conficts.
module Ch02.Lists where

-- A general typecalss that supports stack's methods, which is analog to the signatures in
-- Standard ML.
class Stack p where
  empty :: p a
  isEmpty :: p a -> Bool
  peek :: p a -> a         -- raise Empty exception if stack is empty
  pop :: p a -> p a        -- raise Empty exception if stack is empty
  push :: a -> p a -> p a

-- An implemantation of stack using the built-in list
instance Stack [] where
  empty = []
  
  isEmpty = null
  
  peek [] = error "Empty Stack"
  peek xs = head xs
  
  pop [] = error "Empty Stack"
  pop xs = tail xs
  
  push = (:)

-- An implemantation of stack using a custom data type.
data CustomStack a = Nil
                   | Cons a (CustomStack a)
                   deriving (Eq, Show)
instance Stack CustomStack where
  empty = Nil
  
  isEmpty Nil = True
  isEmpty _   = False

  peek Nil = error "Empty Stack"
  peek (Cons x _) = x

  pop Nil = error "Empty Stack"
  pop (Cons x xs) = xs

  push = Cons
