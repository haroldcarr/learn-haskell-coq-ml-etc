> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
>
> module D2Vect where
>
> import qualified GHC.Types as T

p 31

Chapter 3

white        : works in GHC 7.10
light green  : works in GHC 8.0 ([33, 105])
light yellow : does not work in GHC 8.0, could be implemented via singletons [30] etc
light red    : does not work due to bugs

examples of dependently typed programming

- Nicolas Oury and Wouter Swierstra.
  The power of Pi.
  Proc. 13th ACM SIGPLAN international conference on Functional programming, ICFP ’08
  pages 39–50. ACM, 2008.

- Sam Lindley and Conor McBride.
  Hasochism: the pleasure and pain of dependently typed Haskell programming.
  ACM SIGPLAN Haskell Symposium, 2013.

- Adam Gundry.
  Type Inference, Haskell and Dependent Types. PhD thesis
  University of Strathclyde, 2013.
  Chapter 8

------------------------------------------------------------------------------

Length-indexed vectors

> data Nat = Zero | Succ Nat
>
> data Vec :: T.Type -> Nat -> T.Type where
>   Nil  :: Vec a 'Zero
>   (:>) :: a -> Vec a n -> Vec a ('Succ n)
> infixr 5 :>

Note: 'Type' instead of '*'
- '*' can mean a binary operator

> type family a + b where -- TypeFamilies, TypeOperators
>   'Zero + b = b         -- DataKinds
>   'Succ a + b = 'Succ (a + b)

> append :: Vec a n -> Vec a m -> Vec a (n + m)
> append      Nil w =               w
> append (a :> v) w = a :> append v w
