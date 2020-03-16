{-# LANGUAGE NoImplicitPrelude #-}

module X01 where

import           Data.Void
import           Protolude
import           Test.Hspec

-- http://dev.stephendiehl.com/hask/#phantom-types

newtype Foo tag a = Foo a deriving (Eq, Show)

combine :: Num a => Foo tag a -> Foo tag a -> Foo tag a
combine (Foo a) (Foo b) = Foo (a + b)

{-
Notice the type variable tag does not appear in the right hand side of the declaration.
Enables expressing invariants at type-level that need not manifest at value-level.
This adds extra information at the type-level.
-}

-- All identical at the value level, but differ at the type level.
aa :: Foo () Int
aa  = Foo 1

bb :: Foo t Int
bb  = Foo 1

cc :: Foo Void Int
cc  = Foo 1

x01a :: Spec
x01a  = describe "X01a" $ do
  it "aa"    $ aa `shouldBe` Foo 1
  it "bb"    $ bb `shouldBe` Foo 1
  it "cc"    $ cc `shouldBe` Foo 1
  -------------------------
  it "aa bb" $ aa `shouldBe` bb -- t ~ ()
  it "bb cc" $ bb `shouldBe` cc -- t ~ Void
--it "aa cc" $ aa `shouldBe` cc -- Couldn't match type ‘Void’ with ‘()’

x01b :: Spec
x01b  = describe "X01b" $ do
  it "ex1" $
    (combine aa aa :: Foo () Int)
    `shouldBe` Foo 2 -- () ~ ()
  it "ex2" $
    (combine aa bb :: Foo () Int)
    `shouldBe` Foo 2 -- t ~ ()
  it "ex3" $
    (combine bb bb :: Foo t Int)
    `shouldBe` Foo 2 -- t0 ~ t1

{-
-- Couldn't match type ‘t’ with ‘()’
exampleX1 :: Foo t Int
exampleX1  = combine bb aa

-- Couldn't match type `t' with `Void'
exampleX2 :: Foo t Int
exampleX2  = combine b c
-}
