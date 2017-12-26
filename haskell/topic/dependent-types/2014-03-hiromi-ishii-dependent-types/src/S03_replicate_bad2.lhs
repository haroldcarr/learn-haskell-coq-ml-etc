> {-# LANGUAGE DataKinds #-}
>
> module S03_replicate_bad2 where
>
> import S01_vector

how about:

> vreplicateX :: n -> a -> Vector a n
> vreplicateX = _

    • Expected kind ‘Nat’, but ‘n’ has kind ‘*’
    • In the second argument of ‘Vector’, namely ‘n’


`n` has kind `Nat` : has no inhabitants
cannot pattern match on the first argument
