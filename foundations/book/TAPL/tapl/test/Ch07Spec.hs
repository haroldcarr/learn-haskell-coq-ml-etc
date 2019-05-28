{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch07Spec where

import           Ch07_Untyped_Lambda_Calculus
import           Protolude
import           Test.Hspec

spec :: Spec
spec = describe "ch07" $ do

  it "print" $
    printTm [] (TmApp (TmAbs "x" (TmVar "x"))
                      (TmAbs "y" (TmVar "y")))
    `shouldBe`
    "((lambda x . x) (lambda y . y))"

  it "eval1" $
    eval1 (TmApp (TmAbs "x" (TmVar "x"))
                 (TmAbs "y" (TmVar "y")))
    `shouldBe`
    (Right $ TmAbs "y" (TmVar "y"))

  it "eval" $
    eval (TmApp (TmAbs "x" (TmVar "x"))
                (TmApp (TmAbs "y" (TmVar "y"))
                       (TmAbs "z" (TmVar "z"))))
    `shouldBe`
    TmAbs "z" (TmVar "z")

