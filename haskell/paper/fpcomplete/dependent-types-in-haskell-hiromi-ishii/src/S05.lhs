> {-# LANGUAGE DataKinds          #-}
> {-# LANGUAGE PolyKinds          #-}
> {-# LANGUAGE QuasiQuotes        #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE TemplateHaskell    #-}
> {-# LANGUAGE TypeOperators      #-}
>
> module S05 where
>
> import Data.Singletons
> import Data.Singletons.TH

Eisenberg's `singletons`
- Dependently Typed Programming with Singletons http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf
- (http://hackage.haskell.org/package/singletons) package
- use to generate original, promoted, singleton versions of nats and operations

> singletons [d|
>  data Nat = Z | S Nat
>             deriving (Show, Eq, Ord)
>
>  (+) :: Nat -> Nat -> Nat
>  Z   + n = n
>  S m + n = S (m + n)
>
>  (*) :: Nat -> Nat -> Nat
>  Z   * _ = Z
>  S n * m = n * m + m
>  |]
>
> deriving instance Show (SNat n)
> deriving instance Eq (SNat n)

Template Haskell generates singletons for given definition.

Naming conventions: http://www.cis.upenn.edu/~eir/packages/singletons/README.html

- type `Name`
  - promoted to kind `Name`
  - has singleton `SName`
- function `name`
  - promoted to type family `Name`
  - has singleton `sName`
- binary operator `+`
  - promoted to type family `:+`
  - has singleton `%:+`

`singletons` package has `Sing` *data family* that treat singleton types in unifom manner.
- Uses `PolyKinds`

above generates:

data family Sing (a :: k) -- from Data.Singletons
data instance Sing (n :: Nat) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)
type SNat (n :: Nat) = Sing n

also generates singleton instances for `Eq`

