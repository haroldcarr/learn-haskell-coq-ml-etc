{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Lib where

import qualified Data.Vector as V
import qualified Prelude
import           Protolude
import           Test.Hspec

-- https://programmable.computer/posts/datakinds_runtime.html

newtype SVec (n :: Nat) a =
        SVec (V.Vector a)
  deriving (Eq, Foldable, Functor, Show)

tryMkSVec
  :: forall n a
   . KnownNat n
  => V.Vector a
  -> Maybe (SVec n a)
tryMkSVec x =
  if V.length x == fromIntegral (natVal (Proxy :: Proxy n))
    then Just (SVec x)
    else Nothing

{-
do not know size

import qualified Data.Vector as V
:set -XDataKinds

n in    map :: forall n a b. (a -> b) -> SVec n a -> SVec n b
- expresses relationship between type of its SVec arg and type of result

n in :t tryMkSVec (V.fromList [1::Int])
               :: GHC.TypeNats.KnownNat n => Maybe (SVec n Int)
different
- only have info about a from the arg
- n has NOT relationship to other terms

tryMkSVec (V.fromList [1::Int])
• No instance for (GHC.TypeNats.KnownNat n0)

problem is: need access to length of arg vector at type level (the opposite of what natVal does)
-}

-- this works - because size 1 is given

t1 :: Spec
t1  = it "t1" $
  (tryMkSVec (V.fromList [1]) :: Maybe (SVec 1 Int))
  `shouldBe` Just (SVec (V.fromList [1]))

------------------------------------------------------------------------------
-- Existential Quantification

{-
GHC.TypeNats contains

data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)

type var to right or fat (instead of in type constructor on left): hides the type
- a fun passed a SomeNat value cannot learn the n hidden inside

same as GADT

data SomeNat where
  SomeNat :: forall n. KnownNat n => Proxy n -> SomeNat

not similarity to (except n on both sides)

tryMkSVec :: forall n a. KnownNat n => V.Vector a -> Maybe (SVec n a)

GHC.TypeLits contains

-- lift an positive Integer to type leve
someNatVal :: Integer -> Maybe SomeNat
-}

-- Definition with forall:
data SomeSizedVectorF a
  =  forall n
  .  KnownNat n
  => SomeSizedVectorF (SVec n a)

deriving instance Show a => Show (SomeSizedVectorF a)

-- Definition with GADT syntax:
data SomeSizedVectorG a where
  SomeSizedVectorG
    :: KnownNat n
    => SVec n a
    -> SomeSizedVectorG a

someSizedVectorVal
  :: forall a
   . V.Vector a
  -> SomeSizedVectorF a
someSizedVectorVal x =
 case someNatVal (fromIntegral (V.length x)) of
   Nothing -> panic "negative length?!"
   Just (SomeNat (_ :: Proxy n)) ->
     SomeSizedVectorF (SVec x :: SVec n a)

{-
-- this works
someSizedVectorVal (V.fromList [1::Int])

-- this does not
case someSizedVectorVal (V.fromList [1::Int]) of SomeSizedVectorF sv -> map (+1) sv

type of someSizedVectorVal (V.fromList [1, 2, 3]) is Num a => SomeSizedVector a

pattern match SomeSizedVector sv gives sv with type forall n. KnownNat n => SizedVector n a

n is hidden by SomeSizedVector constructor

map will leak it
type of map (+1) sv would be like : exists n. KnownNat n => SizedVector n a

know some n existed when SomeSizedVector constructed

no way to recover the type-level information; using the SomeSizedVector destroys the type

type n needs to be "consumed" by something with a type like
  forall n. KnownNat n => (forall r. SizedVector n a -> r)
where r is not allowed to reference n (so map does not work)

foldl meets requirements
-}

{-# ANN t3 ("HLint: ignore Use sum"::Prelude.String) #-}
t3 :: Spec
t3  = it "t3" $
  case someSizedVectorVal (V.fromList [1, 2, 3]) of
    SomeSizedVectorF sv -> foldl (+) 0 (map (*5) sv)
  `shouldBe`
  30

------------------------------------------------------------------------------
-- Quantified Continuations

{-
To use a SVec whose size is not known will need to use this:

\f x -> case someSizedVectorVal x of SomeSizedVector sv -> f sv

If the type of x is V.Vector a
then by applying the type of the SomeSizedVector constructor
we know that the type of f must be something like
  forall r. (forall n. KnownNat n => SizedVector n a) -> r

using RankNTypes can write helper
-}

withSizedVector
  :: forall a r
   . V.Vector a
  -> (forall n. KnownNat n => SVec n a -> r)
  -> r
withSizedVector x f =
  case someNatVal (fromIntegral (V.length x)) of
    Nothing -> panic "negative length?!"
    Just (SomeNat (Proxy :: Proxy n)) ->
      f (SVec x :: SVec n a)

{-
type of withSizedVector shows
- enables any vector to be injected into a type-safe bubble
- if expr in bubble can handle a sized vector of any length
- and type of result does not leak the vector length
-}

t4 :: Spec
t4  = it "t4" $
  withSizedVector (V.fromList [1, 2, 3]) (foldl (+) 0 . map (*5))
  `shouldBe`
  30

-- convert type level length to term level
t5 :: Spec
t5  = it "t5" $
  withSizedVector (V.fromList [1, 2, 3, 4, 5]) (foldl (\i _ -> i+1) 0)
  `shouldBe`
  5

