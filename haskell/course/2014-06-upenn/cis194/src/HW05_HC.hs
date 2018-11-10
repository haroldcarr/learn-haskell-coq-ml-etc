{-
Created       : 2014 Jun 08 (Sun) 13:47:39 by Harold Carr.
Last Modified : 2014 Jun 18 (Wed) 06:34:33 by Harold Carr.
-}

-- these two for Exercise 5
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HW05_HC where

import           HW05_ExprT      as E
import           HW05_Parser
import           HW05_StackVM    as S

import qualified Data.Map        as M

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

------------------------------------------------------------------------------
-- Exercise 1

eval :: ExprT -> Integer
eval (Lit i) = i
eval (E.Mul e1 e2) = eval e1 * eval e2
eval (E.Add e1 e2) = eval e1 + eval e2

ex1 :: T.Test
ex1 = T.TestList
    [
      U.teq "eval0" (eval (E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4))) 20
    ]

------------------------------------------------------------------------------
-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr s = do
    e <- parseExp E.Lit E.Add E.Mul s
    return $ eval e

ex2 :: T.Test
ex2 = T.TestList
    [
      U.teq "evs0" (evalStr "(2+3)*4") (Just (eval (E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4))))
    , U.teq "evs1" (evalStr "2+3*4")   (Just (eval (E.Add (E.Lit 2) (E.Mul (Lit 3) (Lit 4)))))
    , U.teq "evs2" (evalStr "2+3*")    Nothing
    ]

------------------------------------------------------------------------------
-- Exercise 3

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = E.Lit
    add = E.Add
    mul = E.Mul

reify :: ExprT -> ExprT
reify = id

ex3 :: T.Test
ex3 = T.TestList
    [
      U.teq "tc0"       ((mul (add (lit 2) (lit 3)) (lit 4)) :: ExprT) (E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4))
    , U.teq "tc0" (reify (mul (add (lit 2) (lit 3)) (lit 4)))          (E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4))
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

{- I could not get this to typecheck, so ended up defining addMod7 and mulMod7 and using them in instance.
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
mulMod7 (Mod7  0) (Mod7  _) = Mod7 0
mulMod7 (Mod7  _) (Mod7  0) = Mod7 0
mulMod7 (Mod7 l0) (Mod7 r0) = Mod7 (mod7 l0 r0)
  where mod7 l r = if l == 1
                   then r
                   else 1 + mod7 (l - 1) r

instance Expr Mod7 where
    lit x = if 0 <= x && x <= 6 then Mod7 x else error ("bad: " ++ show x)
    add   = addMod7
    mul   = mulMod7

testExp     :: Expr a => Maybe a
testExp      = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger  = testExp
testBool    :: Maybe Bool
testBool     = testExp
testMM      :: Maybe MinMax
testMM       = testExp
-- testExp contains a (-4) which is not legal for Mod7
testSat     :: Maybe Mod7
testSat      = testExp

testM7Add   :: Maybe Mod7
testM7Add    = parseExp lit add mul "(3 + 5)"
testM7Mul   :: Maybe Mod7
testM7Mul    = parseExp lit add mul "(3 * 5)"

ex4 :: T.Test
ex4 = T.TestList
    [
      U.teq "ti"  testInteger (Just (-7))
    , U.teq "tb"  testBool    (Just True)
    , U.teq "tm"  testMM      (Just (MinMax 5))
    -- SKIP : testSat gives error because of (-4) not legal
    , U.teq "m7a" testM7Add   (Just (Mod7 1))
    , U.teq "m7m" testM7Mul   (Just (Mod7 7)) -- is this the right result?
    ]

------------------------------------------------------------------------------
-- Exercise 5

-- TODO: extend to handle Bool/And/Or

instance Expr Program where
    lit x   =            [PushI x]
    add x y =  x ++ y ++ [S.Add]
    mul x y =  x ++ y ++ [S.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

exec :: Maybe Program -> Either String StackVal
exec p =
    case p of
        Nothing -> Left "bad"
        Just x  -> stackVM x

ex5 :: T.Test
ex5 = T.TestList
    [
      U.teq "p0" ((mul (add (lit 2) (lit 3)) (lit 4))::Program)    [PushI 2,PushI 3,S.Add,PushI 4,S.Mul]
    , U.teq "p1" ((parseExp lit add mul "(3 * 5)")::Maybe Program) (Just [PushI 3,PushI 5,S.Mul])
    , U.teq "p2" (compile               "(3 * 5)")                 (Just [PushI 3,PushI 5,S.Mul])
    , U.teq "p3" (exec (compile         "(3 * 5)"))                (Right (IVal 15))
      -- parser is not like Haskell - haskell returns 121 (first does mul then final add)
    , U.teq "p4" (exec (compile "(2*3)*(4*5)+1" ))                 (Right (IVal 126))
      -- haskell returns 126 on this one - so parser is adding before doing the middle mul
    , U.teq "p5" (exec (compile "(2*3)*((4*5)+1)"))                (Right (IVal 126))
    ]

------------------------------------------------------------------------------
-- Exercise 6

-- This data and the following two instances are part of the exercise, but is not needed for `withVars`
data VarExprT = VLit Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
              | VVar String
              deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    add = VAdd
    mul = VMul

instance HasVars VarExprT where
    var = VVar

ex6 :: T.Test
ex6 = T.TestList
    [
      U.teq "v0" ((add (lit 3) (var "x")) :: VarExprT)                   (VAdd (VLit 3) (VVar "x"))
    , U.teq "v1" ((add (lit 3) (var "x")) :: (HasVars a, Expr a) => a)   (VAdd (VLit 3) (VVar "x"))
    ]

-- for this exercise, var could just be a regular top-level function
-- but making it a type class makes it extensible (see first part of exercise 6 above)
class HasVars a where
    var :: String -> a

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

-- last arg is Map
instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x0      = \_ -> Just x0
    add x0 y0   = \m ->   x0 m >>= \x ->      y0 m >>= \y -> return (x + y)
    -- mul is equivalent to add except using currying of arg and do (and * instead of + of course)
    mul x0 y0 m = do x <- x0 m;          y <- y0 m;          return (x * y)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp0 = exp0 $ M.fromList vs

-- just to make below shorter
type FM = M.Map String Integer -> Maybe Integer
type MI = Maybe Integer
mm :: M.Map String Integer
mm  = M.fromList [("x", 6), ("y", 10)]

ex6' :: T.Test
ex6' = T.TestList
    [
      U.teq "v2"  (withVars [("x", 6)] $                              var "x")              (Just  6)
    , U.teq "v3"  (withVars [("x", 6)] $                 add (lit 3) (var "x"))             (Just  9)

      -- abandoned equational reasoning - too many steps
    , U.teq "v40" (withVars [("x", 6), ("y", 10)] $ mul (add (lit 3) (var "x")) (var "y"))        (Just 90)
    , U.teq "v41" ((mul (add (lit 3) (var "x")) (var "y")::FM) $ mm)                              (Just 90)
    , U.teq "v42" ((do x <- ((add (lit 3) (var "x")) mm); y <- ((var "y") mm); return (x*y))::MI) (Just 90)

      -- shorter example instead
    , U.teq "v50" (withVars [("x", 6)] $ add (lit 3) (var "x"))  (Just 9)
    , U.teq "v51"      ((add (lit 3)            (var "x")::FM) $ mm)                  (Just 9)
    , U.teq "v52" ((do x <- ((lit 3) mm); y <- ((var "x") mm) ; return (x+y))::MI)    (Just 9)
    , U.teq "v53" ((do x <-  Just 3     ; y <- ((var "x") mm) ; return (x+y))::MI)    (Just 9)
    , U.teq "v54" ((do x <-  Just 3     ; y <- M.lookup "x" mm; return (x+y))::MI)    (Just 9)
    , U.teq "v55" ((do x <-  Just 3     ; y <- Just 6         ; return (x+y))::MI)    (Just 9)
    , U.teq "v56"                                              (return (3+6) ::MI)    (Just 9)
    , U.teq "v57"                                              (Just   (3+6::Int))    (Just 9)
    ]

------------------------------------------------------------------------------

hw05 :: IO T.Counts
hw05 = do
    T.runTestTT ex1
    T.runTestTT ex2
    T.runTestTT ex3
    T.runTestTT ex4
    T.runTestTT ex5
    T.runTestTT ex6
    T.runTestTT ex6'

-- End of file.
