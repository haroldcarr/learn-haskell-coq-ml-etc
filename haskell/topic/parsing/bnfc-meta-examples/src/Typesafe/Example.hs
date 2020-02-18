{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE QuasiQuotes #-}

module Typesafe.Example where

import           Language.LBNF.Runtime (printTree)
import           Prelude               hiding (exp)
import           Typesafe.Typesafe

-- All variables are meta-variables.
-- Variables must be declared in the function parameters
-- Boolean and numeric variables are type checked by Haskell.
prg n tmp x = [prog|
f() {
  n = 0;
  x := True ;
  tmp = 0;
  while ( tmp < 10 ) {
    tmp++;
    n = n * n;
  }
}
|]

main = putStr $ printTree $ fresh vars prg where
  vars = map return ['a'..'z'] ++ ['v' : show n  |n <- [0..]]

class Variable a where
  fromString :: String -> a

instance Variable NVar where
  fromString = NVar . Ident

instance Variable BVar where
  fromString = BVar . Ident

class Fresh a where
  fresh :: [String] -> a -> Prog

instance Fresh Prog where
  fresh _ e = e

instance (Variable a, Fresh b) => Fresh  (a -> b) where
  fresh (s:ss) g = fresh ss $ g (fromString s)


