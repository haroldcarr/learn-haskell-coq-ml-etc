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

-- | Sized vector.
newtype SVec (n :: Nat) a =
        SVec (V.Vector a)
  deriving (Eq, Foldable, Functor, Show)

-- | Given a regular Vector of length n,
-- return a sized vector that includes type n.
-- 'n' is unknown at compiletime.
-- This means it can "create" the 'n' at runtime.
mkSVec
  :: forall n a
   . KnownNat n
  => V.Vector a
  -> Either Text (SVec n a)
mkSVec x =
  if vl == n'
    then Right (SVec x)
    else Left (show vl <> " /= " <> show n')
 where
  vl = V.length x
  n' = fromIntegral (natVal (Proxy :: Proxy n))
{-
import qualified Data.Vector as V
:set -XDataKinds

But this does not work:

mkSVec (V.fromList [1::Int])
• No instance for (GHC.TypeNats.KnownNat n0)

problem is
- TERM-to-TYPE: need access to term level length of arg vector at type level
- TYPE-to-TERM: the opposite of what natVal does : access to type level value at term level
-}
-- This works
-- - because size 1 is given at the type level, and
-- - because the type level is converted to term and checked.
t1 :: Spec
t1  = it "t1" $
  (mkSVec (V.fromList [1]) :: Either Text (SVec 1 Int))
  `shouldBe` Right (SVec (V.fromList [1]))
t1' :: Spec
t1'  = it "t1'" $
  (mkSVec (V.fromList []) :: Either Text (SVec 1 Int))
  `shouldBe` Left "0 /= 1"
{-
note, this works:

n in map :: forall n a b. (a -> b) -> SVec n a -> SVec n b
- expresses relationship between type of its INPUT SVec arg and type of result

but
n in :t mkSVec (V.fromList [1::Int])
               :: GHC.TypeNats.KnownNat n => Maybe (SVec n Int)
different
- only have info about a from the INPUT arg
- n has NO relationship to other terms
-}
------------------------------------------------------------------------------
-- Existential Quantification
{-
GHC.TypeNats contains

data SomeNat = forall n. KnownNat n => SomeNat (Proxy n)

type var on fat right (instead of in type constructor on left): hides the type
- a fun passed a SomeNat value cannot learn the n hidden inside

note similarity to (except n on both sides)

mkSVec :: forall n a. KnownNat n => V.Vector a -> Maybe (SVec n a)

side node: SomeNat defined as GADT

data SomeNat where
  SomeNat :: forall n. KnownNat n => Proxy n -> SomeNat

GHC.TypeLits contains

-- TERM-TO-TYPE
someNatVal :: Integer -> Maybe SomeNat

Use ideas from the above two functions below:
-}
-- Definition with forall:
data SomeSVecF a
  =  forall n
  .  KnownNat n
  => SomeSVecF (SVec n a)

instance Eq a => Eq (SomeSVecF a) where SomeSVecF (SVec vl) == SomeSVecF (SVec vr) = vl == vr
deriving instance Show a => Show (SomeSVecF a)

-- Definition with GADT syntax:
data SomeSVecG a where
  SomeSVecG
    :: KnownNat n
    => SVec n a
    -> SomeSVecG a

-- TERM-TO-TYPE
-- Create sized vector from a regular vector at runtime.
someSVecVal
  :: forall a
   . V.Vector a
  -> SomeSVecF a
someSVecVal x =
 case someNatVal (fromIntegral (V.length x)) of
   Nothing -> panic "negative length"
   Just (SomeNat (_ :: Proxy n)) ->
     SomeSVecF (SVec x :: SVec n a)

t2 :: Spec
t2  = it "t2" $
  someSVecVal (V.fromList [1::Int])
  `shouldBe`
  someSVecVal (V.fromList [1::Int])

t2' :: Spec
t2'  = it "t2'" $ do
  let SomeSVecF (SVec v) = someSVecVal (V.fromList [1::Int])
  v `shouldBe` V.fromList [1::Int]
{-
but
case someSVecVal (V.fromList [1::Int]) of SomeSVecF sv -> fmap (+1) sv
  • Couldn't match expected type ‘p’ with actual type ‘SVec n Int’
        because type variable ‘n’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a pattern with constructor:
          SomeSVecF :: ...

:t someSVecVal (V.fromList [1::Int])
                                     :: SomeSVecF Int

SomeSVec sv gives sv with type forall n. KnownNat n => SVec n a

n is hidden by SomeSVec constructor

map will leak it
type of map (+1) sv would be like : exists n. KnownNat n => SVec n a

know some n existed when SomeSVec constructed

no way to recover the type-level information; using the SomeSVec destroys the type

type n needs to be the INPUT to something with a type like
    forall n. KnownNat n => (forall r. SVec n a -> r)
where r is not allowed to reference n

map does not work that requirement since the n appears in the output
foldl meets the requirement
-}
{-# ANN t3 ("HLint: ignore Use sum"::Prelude.String) #-}
t3 :: Spec
t3  = it "t3" $
  case someSVecVal (V.fromList [1, 2, 3]) of
    SomeSVecF sv -> foldl (+) 0 (map (*5) sv)
  `shouldBe`
  30

------------------------------------------------------------------------------
-- Quantified Continuations
{-
TYPE-TO-TERM

generalize t3 pattern:

\x f -> case someSVecVal x of SomeSVecF sv -> f sv

- x  :: V.Vector a
- SomeSVecF (SVec n a)
- sv :: SVec n a
- f  :: forall r. (forall n. KnownNat n => SVec n a) -> r

using RankNTypes to write helper
-}
withSVec
  :: forall a r
   . V.Vector a
  -> (forall n. KnownNat n => SVec n a -> r)
  -> r
withSVec x f =
  case someNatVal (fromIntegral (V.length x)) of
    Nothing -> panic "negative length"
    Just (SomeNat (Proxy :: Proxy n)) ->
      f (SVec x :: SVec n a)
{-
type of withSVec
- enables any vector to be injected into a type-safe bubble
- if expr in bubble can handle a sized vector of any length, and
- type of result does not leak the vector length
-}
t4 :: Spec
t4  = it "t4" $
  withSVec (V.fromList [1, 2, 3]) (foldl (+) 0 . map (*5))
  `shouldBe`
  30

-- convert type level length to term level
t5 :: Spec
t5  = it "t5" $
  withSVec (V.fromList [1, 2, 3, 4, 5]) (foldl (\i _ -> i+1) 0)
  `shouldBe`
  5

