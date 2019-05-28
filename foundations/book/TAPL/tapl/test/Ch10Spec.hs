{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch10Spec where

import           Ch10_Simply_Typed_Lambda_Calculus
import           Protolude
import           Test.Hspec

spec :: Spec
spec = describe "ch10" $

  it "typeOf" $
    typeOf [] (TmApp (TmAbs "x" TyBool (TmIf (TmVar "x") TmFalse TmTrue)) -- i.e., negation
                     TmTrue)
    `shouldBe`
    Right TyBool
