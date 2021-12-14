> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE GADTs               #-}
> {-# LANGUAGE KindSignatures      #-}
> {-# LANGUAGE RankNTypes          #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeApplications    #-}
> {-# LANGUAGE TypeOperators       #-}
>
> module V1Wrapped where
>
> import           Data.Finite
> import           Data.Proxy
> import           Data.Type.Equality
> import qualified Data.Vector        as V
> import           GHC.TypeLits

https://blog.jle.im/entry/fixed-length-vector-types-in-haskell.html
Fixed-Length Vector Types in Haskell (an Update for 2017)
by Justin Le : Friday August 25, 2017

reference/intro to fixed-length vectors

uses GHC 8.2

two methods

1. non-structural performant fixed-length vector
- implemented using KnownNat mechanisms, and also
- implemented with singletons
- canonical: https://hackage.haskell.org/package/vector-sized

2. structural fixed-length inductive vector
- length enforced
- useful in streaming
- canonical: https://hackage.haskell.org/package/type-combinators

------------------------------------------------------------------------------
1. Non-Structural Way

want constant-time indexing

wrap https://hackage.haskell.org/package/vector

> -- | length is phantom type 'n'
> newtype Vec (n :: Nat) a = UnsafeMkVec { getVector :: V.Vector a }
>   deriving Show

GHC.TypeNats
- type literals
- Nat kind
- type literals 1, 5, 100, : types with kind Nat

    :set -XDataKinds
    :k 5
    => Nat
    :k Vec
    => Vec :: Nat -> * -> *

REFLECT type-level numeral to value using KnownNat typeclass

    :t natVal
    => KnownNat n => proxy n -> Integer

    JL:  :: KnownNat n => p n -> Natural
    Where Numeric.Natural is a non-negative Integer type.

    import Data.Proxy
    natVal (Proxy :: Proxy 10)
    => 10
    :set -XTypeApplications
    natVal (Proxy @10)
    => 10

Nat kind utility functions
- GHC.TypeNats : http://hackage.haskell.org/package/base/docs/GHC-TypeNats.html
- GHC.TypeLits : http://hackage.haskell.org/package/base/docs/GHC-TypeLits.html
  - slightly different API

Smart Constructor

via natVal + KnownNat typeclass

> -- | explicit forall to bring n into scope
> mkVec :: forall n a. KnownNat n => V.Vector a -> Maybe (Vec n a)
> mkVec v | V.length v == l = Just (UnsafeMkVec v)
>         | otherwise       = Nothing
>   where l = fromIntegral (natVal (Proxy @n)) -- TypeApplications + ScopedTypeVariables

    import Data.Vector as V
    Just v = mkVec (V.generate 4 id) :: Maybe (Vec 4 Int)

type-level guarantees

    -- | no guarantee about size of result
    V.map :: (a -> b) -> V.Vector a -> V.Vector b

instead:

> mapVec :: (a -> b) -> Vec n a -> Vec n b
> mapVec f v = UnsafeMkVec $ V.map f (getVector v)

    mapVec show v

but can still cheat:

> mapVec' :: (a -> b) -> Vec n a -> Vec n b
> mapVec' f v =
>   let v' = V.take 2 $ getVector v
>   in UnsafeMkVec $ V.map f v'

    mapVec' show v

> reverse :: Vec n a -> Vec n a
> reverse (UnsafeMkVec xs) = UnsafeMkVec $ V.reverse xs
>
> (++) :: Vec n a -> Vec m a -> Vec (n + m) a
> UnsafeMkVec xs ++ UnsafeMkVec ys = UnsafeMkVec (xs V.++ ys)
>
> zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
> zipVec (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zip xs ys)
>
> takeVec :: forall n m a. KnownNat n => Vec (n + m) a -> Vec n a
> takeVec (UnsafeMkVec xs) = UnsafeMkVec (V.take l xs)
>  where l = fromIntegral (natVal (Proxy @n))
>
> splitVec :: forall n m a. KnownNat n => Vec (n + m) a -> (Vec n a, Vec m a)
> splitVec (UnsafeMkVec xs) = (UnsafeMkVec ys, UnsafeMkVec zs)
>  where
>   l        = fromIntegral (natVal (Proxy @n))
>   (ys, zs) = V.splitAt l xs

(+) in type sigs is type family from GHC.TypeNats

GHC : works well with concrete, monomorphic Nats, e.g., 5 + 3

GHC : but for polymorphic type variables
- n + (m + o) is different type than (n + m) + o
- GHC doesn't reduce + : sees different trees

ghc-typelits-natnormalise
- https://hackage.haskell.org/package/ghc-typelits-natnormalise
- GHC plugin
- GHC flag : -fplugin GHC.TypeLits.NatNormalise
- or, pragma: {-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
- GHC will recognize n + (m + o) ~ (n + m) + o

------------------------------------------------------------------------------
Indexing

finite-typelits
- http://hackage.haskell.org/package/finite-typelits
- to avoid index errors

Finite n
- type with exactly n distinct inhabitants/values
- e.g., Finite 4 contains four "anonymous" inhabitants
- convenience : name them 0, 1, 2, and 3
- sometimes refer to the values of type Finite n as 0 ... (n - 1)

    -- convert back and forth
    packFinite :: KnownNat n => Integer  -> Maybe (Finite n)
    getFinite  ::               Finite n -> Integer

    import Data.Finite
    Prelude.map packFinite [0..3] :: [Maybe (Finite 3)]
    => [Just (finite 0), Just (finite 1), Just (finite 2), Nothing]
    getFinite (finite 2 :: Finite 5)
    => 2
    getFinite (finite 2 :: Finite 0)
    => Exception: finite: Integer 2 is not representable in Finite 0

use Finite n to index a 'Vec n a'

> index :: Vec n a -> Finite n -> a
> index v i = getVector v V.! fromIntegral (getFinite i)

     index v (finite 1)
     => 1
     index v (finite 3)
     => 3

------------------------------------------------------------------------------
Generating

using return-type polymorphism

> -- NOTE:
> -- replicate is pure for the Applicative instance of Vec n
> --   pure ::          Applicative f => a -> f     a
> replicate :: forall n a. KnownNat n => a -> Vec n a
> replicate a = UnsafeMkVec $ V.replicate l a
>  where l = fromIntegral (natVal (Proxy @n))

    V1.replicate 'a' :: Vec 5 Char
    => UnsafeMkVec {getVector = "aaaaa"}

normally : replicate takes an Int arg to specify length

TODO: what would <*> be?
  (<*>) :: f (a -> b) -> f a -> f b

> rr :: forall n a b. KnownNat n => Vec n (a -> b) -> Vec n a -> Vec n b
> rr f v =
>   let f' = getVector f
>       v' = getVector v
>       m  = Prelude.map (\(f'',v'') -> f'' v'') (Prelude.zip (V.toList f') (V.toList v'))
>   in UnsafeMkVec (V.fromList m)

    Just f = mkVec (V.generate 4 (\i -> case i of 0 -> (+1); 1 -> (*1); 2 -> (+2); 3 -> (*2))) :: Maybe (Vec 4 (Int -> Int))
    f `rr` v
    => UnsafeMkVec {getVector = [1,1,4,6]}

    Just m = mkVec (V.generate 4 (\x y z -> x `max` y `max` z)) :: Maybe (Vec 4 (Int -> Int -> Int))
    m `rr` v `rr` (V1.reverse v)
    => UnsafeMkVec {getVector = [3,2,2,3]}

------------------------------------------------------------------------------
Generating with indices

> generate :: forall n a. KnownNat n => (Finite n -> a) -> Vec n a
> generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
>   where l = fromIntegral (natVal (Proxy @n))

Type-Safety and positives and negatives

using Finite n in different way than in index
- index    : Finite in "negative" position : something index    "takes"
- generate : Finite in "positive" position : something generate "gives" (to the f)

negative position
- tells user what values function expects
- indexing a Vec 5 a requires a Finite 5 (i.e., number between 0 and 4)
- guarantees index is valid one : impossible to give invalid index
- so index is allowed to use unsafe indexing in its impl
- develop in "typed-hole" style : if fun requires Finite 4, put an underscore there, GHC will say what is needed

positive position
- tells user what values function returns (that user is expected to handle)
- e.g., in generate, Finite n -> a tells user they must handle every number between 0 and n-1, and nothing else

------------------------------------------------------------------------------
Moving between Sized and Unsized

Converting from sized to unsized : already have it:

    getVector :: Vec n a -> V.Vector a

If size known at compile-time:

    mkVec :: forall n. KnownNat n => V.Vector a -> Maybe (Vec n a)

runtime (requires RankNTypes) : REIFICATION: "reify" runtime value to type-level

implement using GHC.TypeNats someNatVal

    -- n : existentially quantified type
    -- value of SomeNat contains a Proxy n with some specific n
    -- n hidden "inside" constructor
    -- only way to figure it out is to pattern match on the constructor
    --   and use it in a generic and parametrically polymorphic way
    data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)

    -- | converts Natural into SomeNat
    someNatVal :: Natural -> SomeNat
    someNatVal :: Integer -> Maybe SomeNat

> withVec :: V.Vector a -> (forall n. KnownNat n => Vec n a -> r) -> r
> withVec v f = case someNatVal (fromIntegral (V.length v)) of
>   Nothing                           -> error "NO"
>   Just (SomeNat (Proxy :: Proxy m)) -> f (UnsafeMkVec @m v)

- takes
  - unsized vector
  - fun takes Vec n of any size
    - fun then gives handler a Vec n of proper type/size
    - fun choses n
  - Within continuation/handler : use size type
- returns existentially quantified sized vector 'r' (in CPS-style)

    myVector = V.fromList [10,5,8] :: V.Vector Int
    :set -XScopedTypeVariables
    :{
    withVec myVector $ \(v :: Vec n Int) ->
      -- in this body, `v :: Vec 3 Int`, and `n ~ 3`
      -- what is returned here is return value of entire line
      case packFinite 1 :: Maybe (Finite n) of      -- Finite 3
        Nothing -> 0
        Just i  -> v `index` i
    :}
    => 5

> -- | safely get third item:
> getThird :: V.Vector a -> Maybe a
> getThird v = withVec v $ \v' -> fmap (v' `index`) (packFinite 2)

    getThird $ V.fromList [1,2,3]
    => Just 3
    getThird $ V.fromList [1,2]
    => Nothing

> -- | roundtrip (useless)
> vectorToVector :: V.Vector a -> V.Vector a
> vectorToVector v = withVec v getVector

------------------------------------------------------------------------------
Verifying Properties

ensure two vectors have same length (e.g., use use withVec from two different vectors)

use GHC.TypeNats sameNat

    -- Data.Kind : `Type` : synonym for *
    -- Data.Type.Equality
    data (:~:) :: k -> k -> Type where
      Refl :: x :~: x

    sameNat :: (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Maybe (n :~: m)

only way to have non-bottom value of type n :~: m is via Refl constructor
Refl can only be used when n and m are equal
sameNat gives that Refl or if n and m are equal else Nothing

> exactLength :: forall n m a. (KnownNat n, KnownNat m)
>             => Vec n a
>             -> Maybe (Vec m a)
> exactLength v = case sameNat (Proxy @n) (Proxy @m) of
>   Just Refl -> Just v  -- n ~ m, so `Vec n a` is `Vec m a` too
>   Nothing   -> Nothing

(can write this using getVector and mkVec, i.e., un/wrap)

> -- | zip two unsized vectors if lengths the same
> zipSame :: forall a b. V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
> zipSame v1 v2 =
>   withVec v1 $ \(v1' :: Vec n a) ->
>     withVec v2 $ \(v2' :: Vec m b) ->
>       case exactLength v1' of
>         Just v1Same -> Just $ getVector (zipVec v1Same v2') -- v1' same length as v2'
>         Nothing     -> Nothing

other checks
- lengths even/odd
- length greater than N

process
- find runtime way to get witness for property (e.g., n :~: m)
- pattern match on witness
