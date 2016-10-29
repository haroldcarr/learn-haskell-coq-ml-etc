> module Ch05_01_rank_n_types where

type_abstraction
function_type_abstraction

> tf f (x, y) = (f x, f y)

Haskell 98 compiler infers (assumes that x and y must be of same type):

> -- tf :: (a -> b) -> (a, a) -> (b, b)

but this does not work:

> -- tf show (True, 2)
> -- tf length ([True, False, False], [1, 2])
