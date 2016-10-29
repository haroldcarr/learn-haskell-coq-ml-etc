type_abstraction
function_type_abstraction

> {-# LANGUAGE Rank2Types #-}
>
> module Ch05_02_rank_n_types where

RankNTypes enables parametric polymorphism.

Enables `tf` to accept a polymorphic function:
the function has a "higher rank type", in this case Rank 2.

> tf :: (Show a1, Show a2)
>    => (forall a . Show a => a -> b)
>    -> (a1, a2) -> (b, b)
> tf f (x, y) = (f x, f y)

`forall` tells compiler to make `f` polymorphic in `a`

the `forall` keyword indicates nested parametric polymorphism

> ch05_02_e1 = tf show (True, 2)

> tfl :: (Foldable t)
>     => (forall a . t a -> b)
>     -> (t a1, t a2) -> (b, b)
> tfl f (x, y) = (f x, f y)


> ch05_02_e2 = tfl length ([True, False], [1, 2, 4])

Rank 0 : absence of polymorphism :

`add2ints :: Int -> Int -> Int`

Rank 1 : "regular" parametric polymorphism :

`identity :: a -> a`

Rank 2 / Rank n : nested polymorphism.

-- Peyton Jones et al., 2007, Practical type inference for arbitrary-rank types
-- http://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/


