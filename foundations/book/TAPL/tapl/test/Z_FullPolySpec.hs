{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Z_FullPolySpec where

import           Z_FullPoly.Core
import           Z_FullPoly.Syntax
------------------------------------------------------------------------------
import           Protolude
import           Test.Hspec

spec :: Spec
spec  = describe "Z_FullPoly"
  eval1Test

eval1Test :: Spec
eval1Test  = describe "eval1" $
  it "1" $
    eval1 [] (TmApp (TmAbs "x" TyBool (TmVar "x" 0 0))  -- same a termSubstTop 1 above
                    (TmAbs "z" TyBool (TmVar "z" 0 0)))
    `shouldBe`
                     TmAbs "z" TyBool (TmVar "z" 0 0)

