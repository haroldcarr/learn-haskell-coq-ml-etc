{-# LANGUAGE LambdaCase        #-}

module Lib where

-- import           Test.Hspec

{-# ANN module ("HLint: ignore Eta reduce" :: Prelude.String) #-}

{-

https://ecommons.cornell.edu/bitstream/handle/1813/5614/TR2003-1901.pdf

Classical phantom types
- datatypes where type constraints are expressed using type variables
  that do not appear in the datatype cases
Uses
embed typed languages

PRO: encodings guarantee only well formed data can be constructed
CON: do not permit type-safe deconstruction without additional tagging and run-time checks

first-class phantom types
- typed type representations
- typed higher-order abstract syntax trees
- typed generic functions
- dynamic typing
- staged compilation

1 Introduction

Leijen and Meijer [18]) introduced and used phantom types to embed domain specific languages.
Operations on phantom types restricted to a module which hides internal (untyped) implementation.

USE: enforce type constraints on constructed values
     such that only well-typed λ-terms can be formed using fun and app.
-}
data Lam0
  = Fun (Lam0 -> Lam0 )
  | App Lam0 Lam0

type Lam a = Lam0 -- 'a' is phantom type (i.e., not used on left)

fun    :: (Lam a -> Lam b) -> Lam (a -> b)
fun f   = Fun f
app    :: Lam (a -> b) -> Lam a -> Lam b
app m n = App m n
{-
CON: cannot use those constraints when decomposing a value
e.g., functions like 'reduce' typecheck in this module, but not outside
-}

reduce :: Lam b -> Lam b
reduce  = \case
  App (Fun f) t -> f t
  _ -> error "x"

id' :: Lam (a -> b)
id'  = fun id

x :: Lam b
x  = reduce (app (fun id) (fun id))

-- ... TODO
