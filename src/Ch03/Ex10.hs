-- Exercise 3.10
module Ch03.Ex10 where

import Ch03.Ex09

-- (a) Splite balance into two functions, lbalance and rbalance
lbalance, rbalance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
lbalance B (Node R (Node R a x b) y c) z d
  = Node R (Node B a x b) y (Node B c z d)
lbalance B (Node R a x (Node R b y c)) z d
  = Node R (Node B a x b) y (Node B c z d)
lbalance color l z r = Node color l z r
                   
rbalance B a x (Node R (Node R b y c) z d)
  = Node R (Node B a x b) y (Node B c z d)
rbalance B a x (Node R b y (Node R c z d))
  = Node R (Node B a x b) y (Node B c z d)
rbalance color l z r = Node color l z r

insert' :: Ord a => a -> RBTree a -> RBTree a
insert' v tr =
  let ins Empty = Node R Empty v Empty
      ins s@(Node color a y b) =
        case compare v y of
          LT -> lbalance color (ins a) y b
          GT -> rbalance color a y (ins b)
          EQ -> s
      Node _ a y b = ins tr
   in Node B a y b

-- (b) Rewrite ins so that it never tests the color of nodes
-- not on the search path.
insert'' :: Ord a => a -> RBTree a -> RBTree a
insert'' v t = let Node _ l x r = ins' v t in Node B l x r

ins' :: Ord a => a -> RBTree a -> RBTree a
ins' v Empty = Node R Empty v Empty
ins' v t@(Node color l x r) =
  case compare v x of
    LT -> case l of
            Empty -> Node color (ins' v l) x r
            Node _ _ y _ | v < y -> llbalance color (ins' v l) x r
                         | v > y -> lrbalance color (ins' v l) x r
                         | otherwise -> t
    GT -> case r of
            Empty -> Node color l x (ins' v r)
            Node _ _ y _ | v < y -> rlbalance color l x (ins' v r)
                         | v > y -> rrbalance color l x (ins' v r)
                         | otherwise -> t
    EQ -> t

llbalance, lrbalance, rlbalance, rrbalance:: Color
                                          -> RBTree a
                                          -> a
                                          -> RBTree a
                                          -> RBTree a
llbalance B (Node R (Node R a x b) y c) z d
  = Node R (Node B a x b) y (Node B c z d)
llbalance color l z r = Node color l z r

lrbalance B (Node R a x (Node R b y c)) z d
  = Node R (Node B a x b) y (Node B c z d)
lrbalance color l z r = Node color l z r
                   
rlbalance B a x (Node R (Node R b y c) z d)
  = Node R (Node B a x b) y (Node B c z d)
rlbalance color l z r = Node color l z r

rrbalance B a x (Node R b y (Node R c z d))
  = Node R (Node B a x b) y (Node B c z d)
rrbalance color l z r = Node color l z r
