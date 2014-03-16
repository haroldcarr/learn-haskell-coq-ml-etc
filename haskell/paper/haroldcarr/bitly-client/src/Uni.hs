{-# LANGUAGE DeriveDataTypeable #-}

module Expr where

import           Data.Data
import           Data.Generics.Uniplate.Data
import           Data.List                   (nub)

data Expr = Val Int
          | Var String
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr
          | Neg Expr
          deriving (Show, Eq, Data, Typeable)

constants :: Expr -> [Int]
constants x = nub [y | Val y <- universe x]

-- universe (Mul (Val 3) (Val 2))

variables :: Expr -> [String]
variables x = [y | Var y <- universe x]

