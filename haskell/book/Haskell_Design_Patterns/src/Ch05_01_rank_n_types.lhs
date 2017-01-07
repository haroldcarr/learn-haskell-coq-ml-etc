type_abstraction
function_type_abstraction

> module Ch05_01_rank_n_types where

> tf' f (x, y) = (f x, f y)

Haskell 98 compiler infers x and y have same type:

> -- tf' :: (a -> b) -> (a, a) -> (b, b)

therefore, this does not work:

> -- tf' show (True, 2)
> -- tf' length ([True, False, False], [1, 2])
