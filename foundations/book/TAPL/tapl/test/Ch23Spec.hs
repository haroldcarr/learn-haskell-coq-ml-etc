{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch23Spec where

import           Ch23_System_F_Universals
import           Protolude
import           Test.Hspec

spec :: Spec
spec  = describe "ch23" $ do
  idT
  doubleT
  selfAppT
  quadrupleT
