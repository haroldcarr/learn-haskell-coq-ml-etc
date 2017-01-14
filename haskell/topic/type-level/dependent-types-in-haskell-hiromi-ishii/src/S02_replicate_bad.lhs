> {-# LANGUAGE DataKinds #-}
>
> module S02_replicate_bad where
>
> import S01_vector

`replicate` for vectors

> vreplicate :: Nat -> a -> Vector a n
> vreplicate  Z    _ = Nil
> vreplicate (S n) a = a :- vreplicate n a

code won't compile

    • Couldn't match type ‘n’ with ‘'S n0’
      ‘n’ is a rigid type variable bound by
        the type signature for:
          vreplicate :: forall a (n :: Nat). Nat -> a -> Vector a n
      Expected type: Vector a n
        Actual type: Vector a ('S n0)


type signature says
- 1st arg `Nat` intended to be length of resulting vector.
- but parameter `n` of resulting type can be *any* nat; it is independent of 1st arg
