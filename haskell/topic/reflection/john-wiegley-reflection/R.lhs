> -- for #2
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE UndecidableInstances #-}

> -- for #3
> {-# LANGUAGE FlexibleInstances    #-}

> module R where

> -- for #1
> import Test.QuickCheck.Arbitrary
> import Test.QuickCheck.Gen

> -- for #2
> import Data.Proxy
> import Data.Reflection

> -- for #3
> import Data.Tagged

 http://newartisans.com/2017/02/a-case-of-reflection/

Edward Kmett's reflection library (based on 2004 Oleg Kiselyov and Chung-chieh Shan paper)
- reifying data into types (turning a value into something that can be referenced at the type level)

------------------------------------------------------------------------------

Use-case 1

- QuickCheck facility for generating arbitrary data sets
- QuickCheck property testing uses generation
- generation can used independently to generate data for any purpose

> data Foo1 = Foo1 [String]
>   deriving Show

> instance Arbitrary Foo1 where
>   arbitrary = do
>     len <- choose (1, 5)
>     ys  <- vectorOf len (shuffle "0123") -- 1 to 5 element list of String shuffling given string
>     return $ Foo1 ys

> f1 = print =<< generate (arbitrary :: Gen Foo1)

------------------------------------------------------------------------------

Use-case 2

- pick `len` from value provided by user input
- Arbitrary does not allow use of Reader
- how to get user-supplied value into the arbitrary function above?
  - (without using global IORefs or unsafePerformIO)

reflection library
- reify runtime value into type
  - whose name is not possible to know
    - requiring referencing it through a type variable
- communicate that type via a constraint
- reflect value back out when needed

> data Foo2 s = Foo2 [String] -- phantom type added
>   deriving Show             -- associates reified data with Foo2 for reflecting it in instance for this type

> instance Reifies s Int => Arbitrary (Foo2 s) where -- add constraint : says type represented by s reifies an Int
>   arbitrary = do                                   -- instance requires UndecidableInstances
>     len <- choose (1, reflect (Proxy :: Proxy s))
>     ys  <- vectorOf len (shuffle "0123")
>     return $ Foo2 ys

call reify
- data to go up to type level and then back down
- function with arg Proxy s : to know which type variable to use in the type of the call to arbitrary
- requires ScopedTypeVariables

> f2 =
>   reify (read "10" :: Int) $
>        \(Proxy :: Proxy s) -> print =<< generate (arbitrary :: Gen (Foo2 s))

reflection
- enables "wiring" extra data into instances at runtime
- at cost of adding a phantom type

------------------------------------------------------------------------------

Use-case 3

- avoid phantom type

> instance Reifies s Int => Arbitrary (Tagged s Foo1) where -- get type via tag
>   arbitrary = fmap Tagged $ do
>     len <- choose (1, reflect (Proxy :: Proxy s))
>     ys  <- vectorOf len (shuffle "0123")
>     return $ Foo1 ys

- wrap/unwrap:

> f3 =
>   reify (read "7" :: Int) $
>        \(Proxy :: Proxy s) -> print . unTagged =<< generate (arbitrary :: Gen (Tagged s Foo1))

------------------------------------------------------------------------------

Use-case 4

- Tagged benefit
- change refied information in cases where nested types are involved


e.g.: user specifies that value should be supplied to Bar constructor during data generation:

> newtype Bar = Bar Int
>   deriving Show

> data Foo4 = Foo4 [Bar] [String]
>   deriving Show

> instance Reifies s Int => Arbitrary (Tagged s Bar) where
>   arbitrary = return $ Tagged $ Bar $ reflect (Proxy :: Proxy s)

> instance Reifies s (Int, Int) => Arbitrary (Tagged s Foo4) where
>   arbitrary = fmap Tagged $ do
>     let (len, bar) = reflect (Proxy :: Proxy s)
>     l  <- choose (1, len)
>     xs <- vectorOf l (reify bar $ \(Proxy :: Proxy r) -> unTagged <$> (arbitrary :: Gen (Tagged r Bar)))
>     ys <- vectorOf l (shuffle "0123")
>     return $ Foo4 xs ys

> f4 =
>   reify (read "4" :: Int, read "45" :: Int) $
>        \(Proxy :: Proxy s) -> print . unTagged =<< generate (arbitrary :: Gen (Tagged s Foo4))
