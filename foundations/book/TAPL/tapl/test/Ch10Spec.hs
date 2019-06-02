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
  it "get/addBinding 1" $ getBinding ctx 0 `shouldBe` Right (VarBind (TyArr TyBool TyBool))
  it "get/addBinding 2" $ getBinding ctx 1 `shouldBe` Right (VarBind TyBool)
  it "get/addBinding 3" $ getBinding ctx 2 `shouldBe` Left "2 not found"

  it "getTypeFromContext 1" $ getTypeFromContext ctx 0 `shouldBe` Right (TyArr TyBool TyBool)
  it "getTypeFromContext 2" $ getTypeFromContext ctx 1 `shouldBe` Right TyBool
  it "getTypeFromContext 3" $ getTypeFromContext ctx 2 `shouldBe` Left "2 not found"
  it "getTypeFromContext 4" $ getTypeFromContext (addBinding ctx "z" NameBind) 0
    `shouldBe` Left "0 wrong binding NameBind"

ignb :: Term
ignb  = TmIf (TmAbs "x" TyBool (TmVar "x" 0)) TmTrue TmTrue

ifmm :: Term
ifmm  = TmIf TmTrue TmTrue (TmAbs "x" TyBool (TmVar "x" 0))

ptmm :: Term
ptmm  = TmApp (TmAbs "x" TyBool (TmIf (TmVar "x" 0) TmFalse TmTrue))
              (TmAbs "z" TyBool (TmVar "z" 0))

typeOfTest :: Spec
typeOfTest  = describe "typeOfTest" $ do
  it "typeOf 1" $ typeOf [] ignb `shouldBe` (Left $ "if guard not given TyBool "    <> show ignb)

  it "typeOf 2" $ typeOf [] ifmm `shouldBe` (Left $ "if arms have different types " <> show ifmm)

  it "typeOf 3" $ typeOf [] ptmm `shouldBe` (Left $ "param type mismatch "          <> show ptmm)

  it "typeOf 4" $ typeOf [] (TmAbs "x" TyBool (TmApp (TmVar "x" 0) TmTrue))
    `shouldBe` Left "arrow type expected TmApp (TmVar \"x\" 0) TmTrue"

  it "typeOf 5" $ typeOf [] (TmApp -- i.e., negation function
                                   (TmAbs "x" TyBool (TmIf (TmVar "x" 0) TmFalse TmTrue))
                                   TmTrue)
    `shouldBe` Right TyBool


