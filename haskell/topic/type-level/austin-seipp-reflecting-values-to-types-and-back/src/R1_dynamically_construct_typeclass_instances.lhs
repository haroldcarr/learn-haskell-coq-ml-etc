> {-# LANGUAGE FlexibleContexts      #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE RankNTypes            #-}
> {-# LANGUAGE UndecidableInstances  #-}

 https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
 https://www.reddit.com/r/haskell/comments/3hw90k/what_is_the_reflection_package_for/

> module R1_dynamically_construct_typeclass_instances where
>
> import Data.Proxy
> import Data.Reflection

compare Ord-like values, but change out actual dictionary at runtime

> -- Values of type 'a' in dynamically constructed 'Ord' instance
> newtype O a s  = O { runO :: a }

> -- A dictionary describing an 'Ord' instance.
> newtype Ord_ a = Ord_ { compare_ :: a -> a -> Ordering }

> -- Utility
> isEq :: Ordering -> Bool
> isEq EQ = True
> isEq _  = False

instance declaration says `reflect a`, where `a :: O a s`
- want `s` type variables to unify
- but don't care the outer type constructor is `O a :: * -> *`
- so `proxy` unifies with anything specifically because it's irrelevant

> instance Reifies s (Ord_ a) => Eq (O a s) where
>   a == b = isEq (compare_ (reflect a) (runO a) (runO b))

> instance (Eq (O a s), Reifies s (Ord_ a)) => Ord (O a s) where
>   compare a b = compare_ (reflect a) (runO a) (runO b)

> -- Dynamically construct an 'Ord' instance out of a comparsion operator.
> withOrd :: (a -> a -> Ordering) -> (forall s. Reifies s (Ord_ a) => O a s) -> a
> withOrd f v = reify (Ord_ f) (runO . asProxyOf v)
>   where
>     asProxyOf :: f s -> Proxy s -> f s
>     asProxyOf v _ = v

> -- Regular ord instance
> example1 :: Int
> example1 = withOrd       compare  $ max (O 1) (O 2)
> -- => 2

> -- Backwards ord instance
> example2 :: Int
> example2 = withOrd (flip compare) $ max (O 1) (O 2)
> -- => 1
