-- Introducing Functors 244/284

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

-- take tree of strings and turn into tree of string lengths
treeLengths (Leaf s)   = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

-- more abstract
treeMap :: (a -> b) -> Tree a   -> Tree b
treeMap    f           (Leaf a)  = Leaf (f a)
treeMap    f          (Node l r) = Node (treeMap f l) (treeMap f r)

{-
typeclass Functor (fmap :: (a -> b) -> f a -> f b) can further generalize
a kind of "lifting"
takes a function over ordinary values a-> b
lifts it to a function over containers f a -> f b,
where f is the container type
-}

-- for Tree above
instance Functor Tree where
    fmap = treeMap

{-
Functor imposes restrictions.
Can only make instances of Functor from types that have one type parameter.
  E.g., can't write an fmap implementation for Either a b or (a, b)
  or for Bool or Int (no type parameters).
Also, can't place any constraints on type definitions - page 246/286
But putting type constraints on type definitions is a misfeature in Haskell.
Alternative: only place on functions that need them.
-}