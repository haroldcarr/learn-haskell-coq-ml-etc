{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch10Spec where

import           Ch10_Simply_Typed_Lambda_Calculus
import           Protolude
import           Test.Hspec

spec :: Spec
spec  = describe "ch10" $ do
  bindingTest
  typeOfTest

ctx :: Context
ctx  = addBinding (addBinding [] "y" (VarBind TyBool))
                  "x" (VarBind (TyArr TyBool TyBool))


bindingTest :: Spec
bindingTest  = describe "bindingTest" $ do
  it "get/addBinding" $ do
    getBinding ctx "x" `shouldBe` Right (VarBind (TyArr TyBool TyBool))
    getBinding ctx "y" `shouldBe` Right (VarBind TyBool)
    getBinding ctx "z" `shouldBe` Left "'z' not found"

  it "getTypeFromContext" $ do
    getTypeFromContext ctx "x" `shouldBe` Right (TyArr TyBool TyBool)
    getTypeFromContext ctx "y" `shouldBe` Right TyBool
    getTypeFromContext ctx "z" `shouldBe` Left "'z' not found"
    getTypeFromContext (addBinding ctx "z" NameBind) "z" `shouldBe` Left "'z' wrong binding NameBind"

typeOfTest :: Spec
typeOfTest  = describe "typeOfTest" $
  it "typeOf" $
    typeOf [] (TmApp (TmAbs "x" TyBool (TmIf (TmVar "x") TmFalse TmTrue)) -- i.e., negation
                     TmTrue)
    `shouldBe`
    Right TyBool

