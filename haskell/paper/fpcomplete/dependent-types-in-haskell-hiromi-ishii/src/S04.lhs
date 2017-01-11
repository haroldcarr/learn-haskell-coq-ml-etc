> {-# LANGUAGE DataKinds #-}
>
> module S04 where
>
> import S01

1. how to pass type-level natural as function argument
2. how to pattern matching type-level natural to enable writing recursive functions

define data type carrying `Nat` as its parameter
- the structure of its data constructors reflect the one of corresponding type-level natural

> data SNat n where
>   SZ ::           SNat  Z
>   SS :: SNat n -> SNat (S n)


Singleton for promoted types
--------------------------

for each type-level natural `n`, there is exactly one term with the type `SNat n`
- its structure is isomorphic to `n`
- `SNat Z` has `SZ` as its only inhabitant

R. A. Eisenberg and S. Weirich
Dependently Typed Programming with Singletons
http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf

RIGHT HERE

And, we can define the operation between the singlton types to treat the type-level arithmetic. For example, singleton function for natural addition `:+` can be implemented as follows:

```haskell
infixl 6 :+

(%:+) :: SNat n -> SNat m -> SNat (n :+ m)
SZ   %:+ m = m
SS n %:+ m = SS (n %:+ m)
```

Exercise: Define the singleton function for the natural number multiplication `:*`. Be careful about recuring side and addition-multiplication order.

It is too boring to define such singletons by hand for all promoted types and type-level functions. Fortunately, Eisenberg's [singletons](http://hackage.haskell.org/package/singletons) package provides the functionality to automatically do that. In what follows, we assume GHC 7.6.x together with singletons-0.8.6. The most recent version 0.9.x does not work with 7.6.x. For example, we can get all of the original, promoted and singleton versions of natural numbers and operations by following code:

```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes, PolyKinds #-}
import Data.Singletons

singletons [d|
 data Nat = Z | S Nat
            deriving (Show, Eq, Ord)

 (+) :: Nat -> Nat -> Nat
 Z   + n = n
 S m + n = S (m + n)

 (*) :: Nat -> Nat -> Nat
 Z   * _ = Z
 S n * m = n * m + m

 min :: Nat -> Nat -> Nat
 min Z     Z     = Z
 min Z     (S _) = Z
 min (S _) Z     = Z
 min (S m) (S n) = S (min m n)
 |]

deriving instance Show (SNat n)
deriving instance Eq (SNat n)
```

In the above, `singletons` is the Template Haskell macro that generates the singletons for the given definition. `singletons` generates the singletons by following naming convention:

* Type `Name` promoted to the kind `Name`, and its singleton is `SName`.
* Function `name` is promoted to the type family `Name`, and its singleton is `sName`.
* Binary operator `+` is promoted to the type family `:+`, and its singleton is `%:+`.

For more detail, read the package's [README](http://www.cis.upenn.edu/~eir/packages/singletons/README.html).

`singletons` package provides `Sing` *data family* to treat the singleton types in a unifom manner. This can be done with the `PolyKinds` extension, which permits the kind-level polymorphism, as indicated in its name. So, `singletons` macro actually generates the following code:

```haskell
data family Sing (a :: k) -- from Data.Singletons
data instance Sing (n :: Nat) where
  SZ :: Sing Z
  SS :: Sing n -> Sing (S n)
type SNat (n :: Nat) = Sing n
```

In addition, `singletons` generates the singleton version of instances for `Eq` class and so on. For more detail, read the README.

Now that we have the way to pass the type-level argument as the function argument, we can implement `replicate` function as follows:

```active haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (tail, head, replicate)
data Nat = Z | S Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

-- show
replicate :: SNat n -> a -> Vector a n
replicate SZ     _ = Nil
replicate (SS n) a = a :- replicate n a
-- /show
main :: IO ()
main = do
  putStr "replicate (SS (SS (SS SZ))) () == "
  print $ replicate (SS (SS (SS SZ))) ()
```

This code successfully type-checks, and we get the exactly what we need!

Exercise: Write the `sLength` function with following type signature:
```haskell
sLength :: Vector a n -> SNat n
```
That is, `sLength xs` returns the length of `xs` as singleton.

Implicit argument and instance dictionary
-----------------------------------------
Sometimes the length of the vector is clear from its context. For such case, it is convenient if we can omit the singleton argument.

`SingRep` type-class from `singletons` package (this class will be provided as part of `base` package in the future by GHC) provide such a functionality. `SingRep` is the constraint synonym for two type-classes: `SingI` and `SingE`.

`class SingI (a :: k)` provides the member function `sing :: Sing a`. Using `sing`, we can write the *implicit argument* version of `replicate` as follows:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
replicate' :: forall n a. SingRep n => a -> Vector a n
replicate' = replicate (sing :: SNat n)
```

Here, `ScopedTypeVariables` extension enables us to refer to the type-variable `n` in function type signature from function body. To refer to the type variable bound by signature with `ScopedTypeVariables`, we have to quantify over all free variables occuring in type signature. This extension also permits binding type variables with patterns, expression type signature and type-class and instance declarations. For more detail, read the [GHC's Users Guide](http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/other-type-extensions.html#scoped-type-variables).

On the other hand, `SingE (KindParam :: OfKind k)` provides the functionality to convert the singleton type to its original non-morphic type (e.g. convert `SNat n` value into `Nat` value). Here, `KindParam` and `OfKind` is the proxy representing the whole kind `k`. In most recent version of `singletons`, this also provides conversion in the inverse direction with existentially quantified value. We don't discuss these functionalities in detail in this serires, please read `singletons` Haddock for more detail.

Instances for the these classes are also automatically generated by `singletons` macro, so we can use this freely. 

There is one more type class we have not mentioned about so far and `singletons` generates for us. It is `SingKind` together with `SingInstance` data constructor. To describe these usage, let's consider the `transpose` function:

```haskell
transpose :: Vector (Vector a n) m -> Vector (Vector a m) n
```

If the given vector is empty, say `m = Z`, we just have to return the `n`-copies of `Nil`:

```haskell
transpose :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
transpose Nil = replicate' Nil
```

In this case, we have to pass `n` as implicit argument to `replicate'`, so we needed to add the `SingRep n` constraint to the signature.

If `n = 0`, then return the empty list:

```haskell
transpose (Nil :- _) = Nil
```

Then, we can recur on its argument if the content is non-empty:

```active haskell
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (tail, head, replicate, map)
data Nat = Z | S Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

replicate :: SNat n -> a -> Vector a n
replicate SZ     _ = Nil
replicate (SS n) a = a :- replicate n a

replicate' :: forall a n. SingRep n => a -> Vector a n
replicate' = replicate (sing :: SNat n)

head :: Vector a (S n) -> a
head (x :- _) = x

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs

map :: (a -> b) -> Vector a n -> Vector b n
map _ Nil       = Nil
map f (x :- xs) = f x :- map f xs

class SingRep n where
  sing :: SNat n

instance SingRep Z where
  sing = SZ

instance SingRep n => SingRep (S n) where
  sing = SS (sing :: SNat n)

transpose :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
transpose Nil = replicate' Nil
transpose (Nil :- _) = Nil
-- show
transpose ((x :- xs) :- xss) =
  (x :- map head xss) :- transpose (xs :- map tail xss)
-- /show
```

But, this won't type-check! Carefully reading the error message, it seems that GHC says that "I don't know the length of `xs`!". The type signature of `transpose` requires `SingRep` instance for the inner-vector's length, but GHC could not find it. GHC suggests that "add the constraint `SingRep n2` in type context", but it's impossible because we can't refer to the length of `xs` at type signature. So we have to construct the dictionary for its instance by our hand and tell the compiler "here it is".

`SingKind` class and `SingInstance` data-type solves this problem. Here are their definitions for `Nat`s:

```haskell
data SingInstance (n :: Nat) where
  SingInstance :: SingRep n => SingInstance n

singInstance :: SNat n -> SingInstance n
singInstance SZ     = SingInstance
singInstance (SS n) =
  case singInstance n of
    SingInstance -> SingInstance
```

Here, we present the version specialized to the kind `Nat` for simplicity. `singletons` package provides more kind polymorphic version and `singInstance` function is the member of `SingKind` type-class, but mechanism is essentially the same.

What's going on here? `SingInstance n` is the *witness* that there is the `SingRep` instance for `n`. If we have `SingInstance n` value, then we can retrieve the instance ditionary by pattern-matching on it. Then the function `singInstance` recurs on the singleton to inductively retrieve the instance dictionary and save the witness into `SingInstance` step by step.

Solving the previous exercise or copying from `sized-vector` package, we already have `sLength` function to calculate `SNat k`. So we can implement the `transpose` as follows:

```active haskell
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (tail, head, replicate, map)
data Nat = Z | S Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

replicate :: SNat n -> a -> Vector a n
replicate SZ     _ = Nil
replicate (SS n) a = a :- replicate n a

replicate' :: forall a n. SingRep n => a -> Vector a n
replicate' = replicate (sing :: SNat n)

head :: Vector a (S n) -> a
head (x :- _) = x

tail :: Vector a (S n) -> Vector a n
tail (_ :- xs) = xs

map :: (a -> b) -> Vector a n -> Vector b n
map _ Nil       = Nil
map f (x :- xs) = f x :- map f xs

class SingRep n where
  sing :: SNat n

instance SingRep Z where
  sing = SZ

instance SingRep n => SingRep (S n) where
  sing = SS (sing :: SNat n)

data SingInstance (n :: Nat) where
  SingInstance :: SingRep n => SingInstance n

singInstance :: SNat n -> SingInstance n
singInstance SZ     = SingInstance
singInstance (SS n) =
  case singInstance n of
    SingInstance -> SingInstance

sLength :: Vector a n -> SNat n
sLength Nil = SZ
sLength (_ :- xs) = SS $ sLength xs

transpose :: SingRep n => Vector (Vector a n) m -> Vector (Vector a m) n
transpose Nil = replicate' Nil
transpose (Nil :- _) = Nil
-- show
transpose ((x :- xs) :- xss) =
  case singInstance (sLength xs) of
    SingInstance -> (x :- map head xss) :- transpose (xs :- map tail xss)
-- /show
main :: IO ()
main = do
  putStr "transpose [[1,2,3], [2,3,4]] = "
  print $ transpose ((1 :- 2 :- 3 :- Nil) :- (2 :- 3 :- 4 :- Nil) :- Nil)
```

OK, seems good!

Smart constructors
------------------
`singletons` also generates the *smart constructor* for singletons. It is almost the same as original singleton constructors, but carrying additional dictionary information as follows:

```haskell
sZ :: SNat Z
sZ = SZ -- same as the SZ

sS :: SNat n -> SNat (S n)
sS n = case singInstances n of SingInstance -> SS n
```

Using `sZ` and `sS` instead of `SZ`and `SS` sometimes reduces the number of calling `singInstance` function. As indicated above, `singletons`' naming convention for the *smart* constructor is just convert leading capital letter `S` to lower letter `s`. So, if you construct some value with singletons, it is better to use smart constructors instaead of raw constructors.

On the efficiency of `singInstance`
----------------------------------
The above `singInstance k` constructs the instance dictionary by recuring on k, so its runtime cost is linear in `k`. In the most recent `singletons`, `singInstance` is not the member of `SingKind` and implemented safely using `unsafeCoerce` magic to avoid the recursion. So its time complexity is constant. In older version, `singInstance` is not implemented with `unsafeCoerce`, so it is good choice to copy efficient version of `singInstance` from newer code and use it if the efficiency is important.

Ordinals
--------
We have just introduced enough technique to simulate dependent-types in Haskell using singleton patterns. In this section, we focus more on implementation of vectors.

We introduced the `Vector` type to avoid the boundary error. Here, we will implement the `index`ing function with boundary condition statically checked. 

To achieve this, we have to implement the type representing "natural numbers below `n`". Such a type is called a *finite set* or *ordinal*. For instance, `Ordinal Z` has no inhabitant and `Ordinal (S (S Z))` has exactly two elements corresponding to `0` and `1`.

If n is greater than 0, `Ordinal n` has always 0 as its inhabitant. If `k :: Ordinal n`, then `k + 1 :: Ordinal n` might be failed (in the case k = n - 1), but `k + 1 :: Ordinal (n + 1)` always holds. By this observation, we can implement the ordinals as follows:

```haskell
data Ordinal (n :: Nat) where
  OZ :: Ordinal (S n)
  OS :: Ordinal n -> Ordinal (S n)
```

For example, both `OS (OS OZ) :: Ordinal Three` and `OS (OS OZ) :: Ordinal Five` passes type-checking, but `OS (OS OZ) :: Ordinal Two` does not.

OK. Let's write the indexing function:

```active haskell
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude hiding (tail, head, replicate, map)
data Nat = Z | S Nat

infixl 6 :+
infixl 7 :*

type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z     :+ m = m
type instance (S n) :+ m = S (n :+ m)

type family   (n :: Nat) :* (m :: Nat) :: Nat
type instance Z     :* m = Z
type instance (S n) :* m = (n :* m) :+ m

data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
infixr 5 :-

deriving instance Eq a => Eq (Vector a n)

toList :: Vector a n -> [a]
toList Nil = []
toList (x :- xs) = x : toList xs

instance Show a => Show (Vector a n) where
  showsPrec d = showsPrec d . toList

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

data Ordinal (n :: Nat) where
  OZ :: Ordinal (S n)
  OS :: Ordinal n -> Ordinal (S n)

-- show
sIndex :: Ordinal n -> Vector a n -> a
sIndex OZ     (x :- _)  = x
sIndex (OS n) (_ :- xs) = sIndex n xs

main :: IO ()
main = do
  print $ sIndex (OS OZ) (0 :- 1 :- 2 :- 3 :- Nil)
  print $ sIndex OZ (0 :- 1 :- Nil)
  -- | Uncommenting below causes compilation error as expected:
  -- print $ sIndex (OS OZ) (0 :- Nil)
-- /show
```

It is hard to write down such as `OS (OS (OS (OS (OS (OS OZ)))))` thing every time. `type-natural` package provides the quasiquoter to eliminate such a work and enable to write `[od|12|]`, `[od|5|]` and so on.

Exercises:

1. Implement the `sElemIndices :: Eq a => a -> Vector a n -> [Ordinal n]`, which returns all the index with element `a`.
2. Implement the ordinal addition with the *correct* type signature.
4. We can provide `Num` instance for `Ordinal n` (and in fact `type-natural` provides that), but we use quasiquotes here. Why?  
   Hint: consider the type of`fromInteger`.

What's Next?
============

We have now the `Vector` type, which is much alike lists. One missing famous function is `reverse` for them:

```active haskell
{-# LANGUAGE DataKinds, GADTs, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
data Nat = Z | S Nat
type family   (n :: Nat) :+ (m :: Nat) :: Nat
type instance Z   :+ m = m
type instance S n :+ m = S (n :+ m)
data Vector a n where
  Nil  :: Vector a Z
  (:-) :: a -> Vector a n -> Vector a (S n)
-- show
reverse :: Vector a n -> Vector a n
reverse xs0 = go Nil xs0
  where
    go :: Vector a m -> Vector a k -> Vector a (k :+ m)
    go acc Nil = acc
    go acc (x :- xs) = go (x:- acc) xs
-- /show
main = return ()
```

Unfortunately, the above code won't type-check!

```
Couldn't match type `n' with `n :+ 'Z'
```

This means that GHC cannot infer that `n = n + 0`. So we have to tell  the compiler that fact - in other words, we have to *prove* that fact!

In the next article, we will see how to write such proofs comfortablly and the way to express the relation such as equality and ordering.

References
==========


2. [singletons](http://hackage.haskell.org/package/singletons) package


