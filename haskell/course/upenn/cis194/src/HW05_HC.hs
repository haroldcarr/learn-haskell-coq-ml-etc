{-
Created       : 2014 Jun 08 (Sun) 13:47:39 by Harold Carr.
Last Modified : 2014 Jun 10 (Tue) 22:09:41 by Harold Carr.
-}

-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE InstanceSigs #-}

module HW05_HC where

import           HW05_ExprT
import           HW05_Parser

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul e1 e2) = eval e1 * eval e2
eval (Add e1 e2) = eval e1 + eval e2

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "eval0" (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))) 20
    ]

------------------------------------------------------------------------------
-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = do
    e <- parseExp Lit Add Mul s
    return $ eval e

ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "evs0" (evalStr "(2+3)*4") (Just (eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))))
    , U.teq "evs1" (evalStr "2+3*4")   (Just (eval (Add (Lit 2) (Mul (Lit 3) (Lit 4)))))
    , U.teq "evs2" (evalStr "2+3*")    Nothing
    ]

------------------------------------------------------------------------------
-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

reify :: ExprT -> ExprT
reify = id

ex3 :: T.Test
ex3 = T.TestList
    [
      U.teq "tc0"       ((mul (add (lit 2) (lit 3)) (lit 4)) :: ExprT) (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    , U.teq "tc0" (reify (mul (add (lit 2) (lit 3)) (lit 4)))          (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
    ]

------------------------------------------------------------------------------
-- Exercise 4

instance Expr Integer where
    lit x     = x
    add e1 e2 = e1 + e2
    mul e1 e2 = e1 * e2

instance Expr Bool where
    lit x     = x > 0
    add e1 e2 = e1 || e2
    mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)

instance Expr MinMax where
    lit = MinMax
    add = max
    mul = min

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

{-
instance Num Mod7 where
    (Mod7 l) + (Mod7 r) = Mod7 (l + r)
    (Mod7 l) - (Mod7 r) = Mod7 (l - r)
    (Mod7 l) * (Mod7 r) = Mod7 (l * r)
    abs (Mod7 i)        = Mod7 (if i < 0 then negate i else i)
    signum (Mod7 i)     = Mod7 (if i < 0 then 1        else 0)
    fromInteger i       = Mod7 i
-}

addMod7 :: Mod7 -> Mod7 -> Mod7
addMod7 (Mod7 l) (Mod7 r) = Mod7 ((l + r) `div` 7)

mulMod7 :: Mod7 -> Mod7 -> Mod7
mulMod7 (Mod7 0) (Mod7 _) = Mod7 0
mulMod7 (Mod7 _) (Mod7 0) = Mod7 0
mulMod7 (Mod7 l0) (Mod7 r0) = Mod7 (mod7 l0 r0)
  where mod7 l r = if l == 1
                   then r
                   else 1 + mod7 (l - 1) r

instance Expr Mod7 where
    lit x = if 0 <= x && x <= 6 then Mod7 x else error ("bad: " ++ show x)
    add   = addMod7
    mul   = mulMod7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger :: Maybe Integer
testInteger = testExp
testBool :: Maybe Bool
testBool = testExp
testMM :: Maybe MinMax
testMM = testExp
-- testExp contains a (-4) which is not legal for Mod7
testSat :: Maybe Mod7
testSat = testExp

testM7Add :: Maybe Mod7
testM7Add = parseExp lit add mul "(3 + 5)"
testM7Mul :: Maybe Mod7
testM7Mul = parseExp lit add mul "(3 * 5)"

ex4 :: T.Test
ex4 = T.TestList
    [
      U.teq "ti"  testInteger (Just (-7))
    , U.teq "tb"  testBool    (Just True)
    , U.teq "tm"  testMM      (Just (MinMax 5))
    -- TODO : testSat gives error
    , U.teq "m7a" testM7Add   (Just (Mod7 1))
    , U.teq "m7m" testM7Mul   (Just (Mod7 7)) -- is this the right result?
    ]

------------------------------------------------------------------------------
-- Exercise 5

-- TODO

------------------------------------------------------------------------------
-- Exercise 6

-- TODO

------------------------------------------------------------------------------

hw05 :: IO T.Counts
hw05 = do
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3
    T.runTestTT ex4

-- End of file.
