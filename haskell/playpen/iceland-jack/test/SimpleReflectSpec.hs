{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module SimpleReflectSpec where

import           Debug.SimpleReflect
import           Test.Hspec

data V2 a = V2 a a deriving stock (Eq, Functor, Show)

spec :: Spec
spec = describe "foo" xxx

xxx :: Spec
xxx = do
  it "sum" $
    sum [a,b,c,d] `shouldBe`
    0 + a + b + c + d
  it "scanl" $
    scanl (@@) mempty [a,b,c] `shouldBe`
    [mempty, mempty @@ a, (mempty @@ a) @@ b, ((mempty @@ a) @@ b) @@ c]
  it "V2 1 2" $
    V2 1 2 `shouldBe`
    V2 1 2
  it "V2 @Expr 1 2" $
    V2 @Expr 1 2 `shouldBe`
    V2 1 2
  it "+ 1" $
    (+) 1 <$> V2 @Expr 1 2 `shouldBe`
    V2 (1 + 1) (1 + 2)
  it "+ 2" $
    (+) 1 <$> V2 1 2 `shouldBe`
    V2 2 3
