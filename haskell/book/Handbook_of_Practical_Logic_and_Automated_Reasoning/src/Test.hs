{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-do-bind  #-}

module Test where

import           X_1_6_to_1_8_Expr
import           X_2_1_Prop_Syntax
import           X_2_2_Prop_Semantics

test = do
    X_1_6_to_1_8_Expr.test
    X_2_1_Prop_Syntax.test
    X_2_2_Prop_Semantics.test
