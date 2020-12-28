{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module L1 where

------------------------------------------------------------------------------
import qualified Data.Finite              as F
import qualified Data.Proxy               as P
import qualified Data.Type.Equality       as Eq
import qualified Data.Singletons          as S
import qualified Data.Singletons.Decide   as SD
import qualified Data.Singletons.TypeLits as STL
import qualified Data.Vector              as V
import qualified GHC.TypeNats             as TN
import qualified Numeric.Natural          as N
import qualified Test.Hspec               as HS
------------------------------------------------------------------------------
{-
Fixed-Length Vector Types in Haskell (an Update for 2017)
Justin Le
August 25, 2017

reference/introduction to fixed-length vectors as of GHC 8.2

two methods here
1. performant fixed-length vector
  - implemented using native KnownNat
  - also using using singletons
  - for most uses, this is what should be used
  - canonical haskell package : 'vector-sized' package

2. structural fixed-length inductive vector
  - like a fixed-length (lazily linked) list
  - length enforced by structure of the data type
  - use cases
    - streaming data
    - situations where use structural characteristics of lengths in context
      of a dependently typed program
  - canonical haskell package : 'type-combinators'

------------------------------------------------------------------------------
The Non-Structural Way

constant-time indexed data structure
wrap 'vector' library
newtype containing length as phantom type
-}
newtype Vec (n :: TN.Nat) a = UnsafeMkVec { getVector :: V.Vector a } deriving (Eq, Show)
{-
:k 5
Nat
:k Vec
Vec :: Nat -> * -> *
utility functions for Nat kind in GHC.TypeNats
(also in GHC.TypeLits for a slightly different API)

------------------------------------------------------------------------------
RECOMMEND SINGLETONS

singletons-style subsumes TypeNats and KnownNat

recommend using singletons-style for anything more than simple usage of KnownNat

------------------------------------------------------------------------------
REFLECTION: type to value

-- TypeNats style
natVal :: KnownNat n => p n -> Natural

-- Singletons style
sing     :: KnownNat n => Sing n
fromSing :: Sing n -> Natural       -- (for n :: Nat)
-}
reflection :: HS.Spec
reflection  = HS.describe "REFLECTION" $ do
  HS.it "TN" $ TN.natVal (P.Proxy @10) `HS.shouldBe` 10
  -------------------------
  HS.it "S"  $ S.fromSing (S.sing @11) `HS.shouldBe` 11
{-
------------------------------------------------------------------------------
REIFICATION :: value -> type

-- TypeNats style
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)
someNatVal :: Natural -> SomeNat

-- Singletons style
data SomeSing Nat = forall n. SomeSing (Sing n)
toSing :: Natural -> SomeSing Nat

withSomeSing :: Natural -> (forall n. Sing n -> r) -> r
-}
reification :: HS.Spec
reification  = HS.describe "REIFICATION" $ do
  HS.it "TN" $ TN.someNatVal   10                        `HS.shouldBe` TN.SomeNat  (P.Proxy @10)
  -------------------------
  HS.it "S1" $ S.toSing        11                        `HS.shouldBe`  S.SomeSing (S.sing  @11)
  HS.it "S2" $ S.withSomeSing (12::N.Natural) S.fromSing `HS.shouldBe`                       12
{-
------------------------------------------------------------------------------
EQUALITY

-- TypeNats style
sameNat :: (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Maybe (n :~: m)

-- Singletons style
-- from Data.Singletons.Decide
-- for our purposes, Decision is basically a fancy Maybe
data Decision a = Proved a | Disproved (a -> Void)
(%~) :: Sing n -> Sing m -> Decision (n :~: m)
-}
equality :: HS.Spec
equality  = HS.describe "EQUALITY" $ do
  HS.it "TNt" $ TN.sameNat   (P.Proxy @10) (P.Proxy @10) `HS.shouldBe` Just Eq.Refl
  HS.it "TNf" $ TN.sameNat   (P.Proxy @10) (P.Proxy @11) `HS.shouldBe` Nothing
  -------------------------
  HS.it "St"  $(case (SD.%~) (S.sing  @10) (S.sing  @10) of
                  SD.Proved    _ -> True
                  SD.Disproved _ -> False)                   `HS.shouldBe` True
  HS.it "Sf"  $(case (SD.%~) (S.sing  @10) (S.sing  @11) of
                  SD.Proved    _ -> True
                  SD.Disproved _ -> False)                   `HS.shouldBe` False
{-
------------------------------------------------------------------------------
Smart Constructor

make a Vec from a Vector iff the length is the correct type:
-}
mkVecTN :: forall n a. TN.KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVecTN v | V.length v == l = Just (UnsafeMkVec v)
          | otherwise       = Nothing
 where
  l = fromIntegral (TN.natVal (P.Proxy @n))

-- Sing n -> and KnownNat n => have the same power
-- think of Sing n as a token that carries KnownNat n =>

-- explicit style : user passes in a Sing
mkVecS :: S.Sing n -> V.Vector a -> Maybe (Vec n a)
mkVecS s v | V.length v == l = Just (UnsafeMkVec v)
           | otherwise       = Nothing
 where
  l = fromIntegral (S.fromSing s)

-- implicit style : length inferred from return type (requiring KnownNat n constraint)
mkVecS' :: forall n a. TN.KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVecS' v | V.length v == l = Just (UnsafeMkVec v)
          | otherwise       = Nothing
 where
  l = fromIntegral (S.fromSing (S.sing :: S.Sing n))

-- alternatively, re-using `mkVecS`
mkVecS'' :: TN.KnownNat n => V.Vector a -> Maybe (Vec n a)
mkVecS''  = mkVecS S.sing

mkvec :: HS.Spec
mkvec  = HS.describe "MKVEC" $ do
  HS.it "TN/S" $ mkVecTN            (V.fromList [1,2::Int]) `HS.shouldBe`
                 mkVecS (S.sing @2) (V.fromList [1,2::Int])
{-
------------------------------------------------------------------------------
Utilizing type-level guarantees

want type of vectors to describe the nature of the operations
-}
-- must be careful that guarantees specified in types are handled correctly
-- in the unsafe operations, because the types do not structurally enforce
-- their type-level lengths
mapVec :: (a -> b) -> Vec n a -> Vec n b
mapVec f = UnsafeMkVec . V.map f . getVector

instance Functor (Vec n) where fmap = mapVec

(++) :: Vec n a -> Vec m a -> Vec (n TN.+ m) a
UnsafeMkVec xs ++ UnsafeMkVec ys = UnsafeMkVec (xs V.++ ys)

zipVec :: Vec n a -> Vec n b -> Vec n (a, b)
zipVec (UnsafeMkVec xs) (UnsafeMkVec ys) = UnsafeMkVec (V.zip xs ys)

takeVecTN :: forall n m a. TN.KnownNat n => Vec (n TN.+ m) a -> Vec n a
takeVecTN (UnsafeMkVec xs) = UnsafeMkVec (V.take l xs)
 where
  l        = fromIntegral (TN.natVal (P.Proxy @n))

splitVecTN :: forall n m a. TN.KnownNat n => Vec (n TN.+ m) a -> (Vec n a, Vec m a)
splitVecTN (UnsafeMkVec xs) = (UnsafeMkVec ys, UnsafeMkVec zs)
 where
  l        = fromIntegral (TN.natVal (P.Proxy @n))
  (ys, zs) = V.splitAt l xs
{-
------------------------------------------------------------------------------
Notes on the typechecker

GHC’s typechecker works well with concrete, monomorphic Nats, e.g., 5 + 3

But GHC treats (+) opaquely when using using it with polymorphic type variables.
means  n + (m  + o) is a different than
      (n +  m) + o
GHC doesn’t reduce +
so the types look like different trees

To augment GHC’s typechecker, use ghc-typelits-natnormalise plugin

flag to GHC (as -fplugin GHC.TypeLits.NatNormalise)
or pragma:
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

Then GHC will recognize n + (m + o) and (n + m) + o are the same

------------------------------------------------------------------------------
Indexing

use finite-typelits package

Finite n type : has exactly n distinct inhabitants/values

reflection:

packFinite :: KnownNat n => Integer  -> Maybe (Finite n)
getFinite  ::               Finite n -> Integer
-}
-- index will never fail at runtime due to a bad index
index :: Vec n a -> F.Finite n -> a
index v i = getVector v V.! fromIntegral (F.getFinite i)
{-
------------------------------------------------------------------------------
Generating
-}
-- implicit : using return-type polymorphism
replicateTN :: forall n a. TN.KnownNat n => a -> Vec n a
replicateTN  = UnsafeMkVec . V.replicate l
 where
  l = fromIntegral (TN.natVal (P.Proxy @n))

-- expicit
replicateS :: S.Sing n -> a -> Vec n a
replicateS s x = UnsafeMkVec $ V.replicate l x
 where
  l = fromIntegral (S.fromSing s)

-- implicit
replicateS' :: TN.KnownNat n => a -> Vec n a
replicateS'  = replicateS S.sing

replicate :: HS.Spec
replicate  = HS.describe "REPLICATE" $ do
  HS.it "TN/S" $ replicateTN             'a' `HS.shouldBe`
                 replicateS  (S.sing @4) 'a'
{-
replicate is pure for the Applicative instance of Vec n

replicate :: KnownNat    n => a -> Vec n a
pure      :: Applicative f => a -> f     a

TODO: what would <*> be?

------------------------------------------------------------------------------
Generating with indices
-}
generate :: forall n a. TN.KnownNat n => (F.Finite n -> a) -> Vec n a
generate f = UnsafeMkVec $ V.generate l (f . fromIntegral)
 where
  l = fromIntegral (TN.natVal (P.Proxy @n))
{-
Some mismatch when using libraries that use KnownNat (e.g., finite-typelits and Finite type)

convert between SNat and withKnownNat

-- given a `KnownNat` constraint, construct a `Sing`
-- pattern match to reveal `KnownNat constraint`
SNat :: KnownNat n => Sing n

-- given `Sing n`
-- execute something in the context where that `n` has a `KnownNat` constraint
withKnownNat :: Sing n -> (KnownNat n => r) -> r
-}
-- explicit
generateS :: S.Sing n -> (F.Finite n -> a) -> Vec n a
generateS s f = STL.withKnownNat s $
  UnsafeMkVec $ V.generate l (f . fromIntegral)
 where
  l = fromIntegral (S.fromSing s)

-- explicit, via pattern matching:
generate'S :: S.Sing n -> (F.Finite n -> a) -> Vec n a
generate'S s@STL.SNat f = UnsafeMkVec $ V.generate l (f . fromIntegral)
 where
  l = fromIntegral (S.fromSing s)

-- implicit
generateS' :: TN.KnownNat n => (F.Finite n -> a) -> Vec n a
generateS'  = generateS S.sing
{-
------------------------------------------------------------------------------
Type-Safety and positives and negative

using Finite n differently than in index
In index   , Finite is in the “negative” position — it’s something that the function “takes”
In generate, Finite is in the “positive” position — it’s something that the function “gives”
(to the f in generate f).

negative position
- what the function expects
- guarantees whatever Finite n index given is valid
- so implementation can use unsafe indexing

- enables “typed-hole” code development : put an underscore and GHC says what is needed

positive position
- what the function can return

------------------------------------------------------------------------------
Moving between Sized and Unsized

getVector :: Vec n a -> V.Vector a
mkVecTN :: forall n. KnownNat n => V.Vector a -> Maybe (Vec n a)

what if n is not known?

return a Vec n, where n is the length of the input vector, determined at runtime

from GHC.TypeNats:

-- existentially quantified type 'n'
data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)
someNatVal :: Natural -> SomeNat

withVec:
- takes an unsized vector
- returns an existentially quantified sized vector
- in CPS-style

give the function a vector
a way to “handle” a Vec n of any possible size
withVec will give handler a Vec n of the proper type/size.
withVec gets to chose the n that you must handle.

Within continnuation/handler, take advantage of the size type/type-level guarantees
CAVEAT : must be able to handle any size

REIFICATION :  value to type
-}
withVecTN :: V.Vector a -> (forall n. TN.KnownNat n => Vec n a -> r) -> r
withVecTN v f = case TN.someNatVal (fromIntegral (V.length v)) of
                  -- pattern match to access existential type 'n'
                  TN.SomeNat (P.Proxy :: P.Proxy m) -> f   (UnsafeMkVec @m v)

-- explicit
withVecS  :: V.Vector a -> (forall n. S.Sing n -> Vec n a -> r) -> r
withVecS  v f = case S.toSing (fromIntegral (V.length v)) of
                  S.SomeSing s                      -> f s (UnsafeMkVec    v)

-- implicit
withVecS' :: V.Vector a -> (forall n. S.Sing n -> Vec n a -> r) -> r
withVecS' v f = S.withSomeSing (fromIntegral (V.length v)) $
                  \s                                -> f s (UnsafeMkVec    v)
{-
convert
- from sized to unsized : getVector
- from unsized to sized : withVec
-}
testVec :: V.Vector Int
testVec  = V.fromList [10,5,8]

withvec :: HS.Spec
withvec  = HS.describe "WITHVEC" $ do
  HS.it "TN" $
    withVecTN testVec $ \(v :: Vec n Int) ->
      -- in this function body, `v :: Vec 3 Int`, and `n ~ 3`
      -- whatever I return here will be the return value of the entire line
      case F.packFinite 1 :: Maybe (F.Finite n) of      -- Finite 3
        Nothing -> 0
        Just i  -> v `index` i
    `HS.shouldBe`
    5
  -------------------------
  HS.it "S" $
    withVecS  testVec $ \s (v :: Vec n Int) ->
      STL.withKnownNat s $
      case F.packFinite 1 :: Maybe (F.Finite n) of
        Nothing -> 0
        Just i  -> v `index` i
    `HS.shouldBe`
    5

getThirdTN :: V.Vector a -> Maybe a
getThirdTN v = withVecTN v $ \v' -> fmap (v' `index`) (F.packFinite 2)

getthird :: HS.Spec
getthird  = HS.describe "GETTHIRD" $ do
  HS.it "TN1" $ getThirdTN (V.fromList [1,2::Int,3]) `HS.shouldBe` Just 3
  HS.it "TN2" $ getThirdTN (V.fromList [1,2::Int])   `HS.shouldBe` Nothing
{-
------------------------------------------------------------------------------
Verifying Properties

ensure two vectors have same length

using sameNat from GHC.TypeNats:

`Type` is a synonym for * (from Data.Kind)

-- from the module Data.Type.Equality
data (:~:) :: k -> k -> Type where
    Refl :: x :~: x

sameNat
    :: (KnownNat n, KnownNat m)
    => Proxy n
    -> Proxy m
    -> Maybe (n :~: m)
-}
exactLengthTN
  :: forall n m a
   . (TN.KnownNat n, TN.KnownNat m)
  => Vec n a
  -> Maybe (Vec m a)
exactLengthTN v = case TN.sameNat (P.Proxy @n) (P.Proxy @m) of
  Just Eq.Refl -> Just v     -- here, n ~ m, so a `Vec n a` is a `Vec m a`, too
  Nothing      -> Nothing

-- explicit
exactLengthS
  :: S.Sing m
  -> S.Sing n
  -> Vec n a
  -> Maybe (Vec m a)
exactLengthS sM sN v = case sM SD.%~ sN of
  SD.Proved SD.Refl -> Just v
  SD.Disproved _    -> Nothing

-- implicit
exactLengthS'
  :: (TN.KnownNat m, TN.KnownNat n)
  => Vec n a
  -> Maybe (Vec m a)
exactLengthS'  = exactLengthS S.sing S.sing

-- could also write using getVector / mkVecTN (i.e., un/wrap)
-- zip two unsized vectors IFF their lengths are the same
zipSame :: forall a b. V.Vector a -> V.Vector b -> Maybe (V.Vector (a, b))
zipSame v1 v2 = withVecTN v1 $ \(v1' :: Vec n a) ->
                withVecTN v2 $ \(v2' :: Vec m b) ->
  case exactLengthTN v1' of
    Just v1Same -> Just (getVector (zipVec v1Same v2')) -- v1' has the same length as v2'
    Nothing     -> Nothing
{-
checking other properites
- find runtime witness of property (e.g., n :~: m)
  - a function (that will probably return Maybe)
- pattern match on witness
  - now property visible to GHC type checker

------------------------------------------------------------------------------
Real-World Examples

above pattern used in many real-world libraries
'vector-sized' : I use for all my sized-vector needs

hmatrix library : I take advantage of in my dependently typed neural networks tutorial series.

linear library : one of first libraries to adopt this style
-}
