{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Ch23_System_F_Universals where

import qualified Prelude
import           Protolude
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant id"::Prelude.String) #-}

id :: forall a. a -> a
id a = a

idT :: Spec
idT  = do
  it "id 1" $ id  1  `shouldBe`  1
  it "id 2" $ id '2' `shouldBe` '2'

double :: forall a. (a -> a) -> a -> a
double f a = f (f a)

doubleT :: Spec
doubleT  = do
  it "double 1" $ double (+2)            3  `shouldBe` 7
  it "double 2" $ double (\a -> a ++ a) "1" `shouldBe` "1111"

selfApplication :: (forall a. (a -> a)) -> (forall a. (a -> a))
selfApplication  f = f f

selfAppT :: Spec
selfAppT  = do
  it "selfApp 1" $ selfApplication id  3  `shouldBe`  3
  it "selfApp 1" $ selfApplication id "3" `shouldBe` "3"

quadruple :: forall a. (a -> a) -> a -> a
quadruple  = double double

quadrupleT :: Spec
quadrupleT  = do
  it "quadruple 1" $ quadruple (+1) 0  `shouldBe`  4
  it "quadruple 2" $ quadruple (*2) 1  `shouldBe` 16

data List a where
  Nil  ::                List a
  Cons :: a -> List a -> List a

head :: forall a. List a -> Maybe a
head  Nil       = Nothing
head (Cons a _) = Just a


