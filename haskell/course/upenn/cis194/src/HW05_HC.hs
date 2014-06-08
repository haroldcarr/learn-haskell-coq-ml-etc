{-
Created       : 2014 Jun 08 (Sun) 13:47:39 by Harold Carr.
Last Modified : 2014 Jun 08 (Sun) 14:06:47 by Harold Carr.
-}

module HW05_HC where

import           HW05_ExprT
import           HW05_Parser

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul e1 e2) = eval e1 * (eval e2)
eval (Add e1 e2) = eval e1 + (eval e2)

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

hw05 :: IO T.Counts
hw05 = do
    _ <- T.runTestTT ex1
    T.runTestTT ex2

-- End of file.
