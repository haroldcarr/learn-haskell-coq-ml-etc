> {-# LANGUAGE DataKinds #-}
>
> module S03 where
>
> import S01

how about:

> vreplicateX :: n -> a -> Vector a n
> vreplicateX = _

    • Expected kind ‘Nat’, but ‘n’ has kind ‘*’
    • In the second argument of ‘Vector’, namely ‘n’


`n` has kind `Nat` : has no inhabitants
cannot pattern match on the first argument
