{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Lib where

import qualified Prelude
import           Protolude
import           Test.Hspec

{-# ANN module ("HLint: ignore Eta reduce" :: Prelude.String) #-}

{-
https://www.cs.ox.ac.uk/ralf.hinze/publications/With.pdf

------------------------------------------------------------------------------
1
Introducing phantom types (HC: via GADTs)

example: embed a statically typed expression language in Haskell

rules out simple Term data type : allows mixing terms of different types

idea : parameterize : Term t : terms of type t
- restricts where they can be used

Zero       :: Term Int
Succ, Pred :: Term Int → Term Int
IsZero     :: Term Int → Term Bool
If         :: ∀a . Term Bool → Term a → Term a → Term a.


The above signatures cannot be be represented by Haskell ADT (but can with GADTs)
because all ADT constructors the same result type.

constrain the type argument of Term to a certain type

Using GADTs (HC: not exactly the paper presentation):
-}
data Term t where
  Zero   ::                                  Term Int
  Succ   :: Term Int                      -> Term Int
  Pred   :: Term Int                      -> Term Int
  IsZero :: Term Int                      -> Term Bool
  If     :: Term Bool -> Term a -> Term a -> Term a

-- interpreter

eval               :: Term t -> t
eval  Zero          = 0
eval (Succ   e)     = eval e + 1
eval (Pred   e)     = eval e - 1
eval (IsZero e)     = eval e == 0
eval (If e1 e2 e3 ) = if eval e1 then eval e2 else eval e3

{-
The interpreter is tag free:
- If it receives a Boolean expression, then it returns a Boolean.
- conventional interpreter of type Term → Val has to inject the Boolean into the Val data type
  and, when evaluating conditional, it has to untag the evaluated condition
  and has to check whether the value is actually a Boolean

The GADT version uses static typing to avoid tags.

------------------------------------------------------------------------------
2 Generic functions

example : compress different types of data to strings of bits

(could use type classes)

idea : define type whose elements represent types

the compiler will complain if you try to compress an incorrect type

compress RInt "60"

<interactive>:2:15: error:
    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
-}

data RType t where
  RInt  ::                       RType Int
  RChar ::                       RType Char
  RList :: RType a            -> RType [a]
  RPair :: RType a -> RType b -> RType (a, b)

rString :: RType Prelude.String
rString  = RList RChar

type Bit = Int

compress                       :: forall t . RType t -> t -> [Bit]
compress  RInt               i  = compressInt i
compress  RChar              c  = compressChar c
--  0 indicates empty list
compress (RList  _)         []  = [0]
-- 1 indicates non-empty list
compress (RList ra)    (a : as) = 1 : compress ra a ++ compress (RList ra) as
compress (RPair ra rb) (a ,  b) =     compress ra a ++ compress rb b

-- to extend compress to data types with more than two constructors
-- ensure codes for the constructors have a unique prefix property

-- https://gist.github.com/linusyang/4057470cf96b88d13bd8

toBinary :: Int -> [Bit]
toBinary  = \case
  0 -> []
  x -> x `mod` 2 : toBinary (x `div` 2)

toNumber :: [Bit] -> Int
toNumber  = toN 1
 where
  toN t = \case
    []       -> 0
    (b : bs) -> b * t + toN (t * 2) bs

padding :: Int -> Int -> [Bit]
padding p x =
  if p <= len then b else b ++ take (p - len) (repeat 0)
 where
  b   = toBinary x
  len = length b

compressInt :: Int -> [Bit]
compressInt = padding 32

compressChar :: Char -> [Bit]
compressChar x = padding 7 $ ord x

testCompress :: Spec
testCompress  = describe "testCompress" $ do
  it "int" $ compress RInt 60 `shouldBe`
    [0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
  it "string" $ compress rString "Richard" `shouldBe`
    [1,0,1,0,0,1,0,1,1,1,0,0,1,0,1,1,1,1,1,0,0,0,1,1,1,0,0,0,1,0,1,1,1,1,0,0,0,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,0,1,1,0]

{-
TODO ...
-}




