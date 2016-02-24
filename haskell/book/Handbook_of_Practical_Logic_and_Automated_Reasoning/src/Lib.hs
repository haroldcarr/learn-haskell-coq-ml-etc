{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecursiveDo #-}

module Lib where

import           Control.Applicative ((<$>), (<|>))
import           Data.Char           (isAlpha, isDigit)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util     as U (t, tt)
import qualified Text.Earley         as TE (Grammar, Prod, fullParses,
                                            namedSymbol, parser, rule, satisfy,
                                            (<?>))

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- 1.6

data Expr =
    Var   String
  | Const Int
  | Add   Expr Expr
  | Mul   Expr Expr
  deriving (Eq, Read, Show)

eExpr = U.t "eExpr"
    (Add (Mul (Const 2) (Var "x")) (Var "y"))
    (Add (Mul (Const 2) (Var "x")) (Var "y"))

simplify1 expr = case expr of
    (Add (Const m) (Const n)) -> Const (m + n)
    (Mul (Const m) (Const n)) -> Const (m * n)
    (Add (Const 0)         x) -> x
    (Add x         (Const 0)) -> x
    (Mul (Const 0)         _) -> Const 0
    (Mul _         (Const 0)) -> Const 0
    (Mul (Const 1)         x) -> x
    (Mul x         (Const 1)) -> x
    _                         -> expr

s1 = simplify1

eSimplify1 = U.tt "eSimplify1"
    [ s1 $ Add (Const 3) (Const 4)
    , s1 $ Add (Const 4) (Const 3)
    , s1 $ Add (Const 0) (Const 7)
    , s1 $ Add (Const 7) (Const 0)
    , s1 $ Mul (Const 1) (Const 7)
    , s1 $ Mul (Const 7) (Const 1)
    , s1 $      Const 7
    ]
    (Const 7)

simplify expr = case expr of
    (Add e1 e2) -> simplify1 (Add (simplify e1) (simplify e2))
    (Mul e1 e2) -> simplify1 (Mul (simplify e1) (simplify e2))
    _           -> simplify1 expr

eSimplify = U.t "eSimplify"
    (simplify (Add (Mul (Add (Mul (Const 0)
                                  (Var "x"))
                             (Const 1))
                        (Const 3))
                   (Const 12)))
     (Const 15)

expr :: TE.Grammar r (TE.Prod r String String Expr)
expr = mdo
    x1 <- TE.rule $ Add   <$> x1 <* TE.namedSymbol "+" <*> x2
                 <|> x2
                 TE.<?> "sum"
    x2 <- TE.rule $ Mul   <$> x2 <* TE.namedSymbol "*" <*> x3
                 <|> x3
                 TE.<?> "product"
    x3 <- TE.rule $ Var   <$> (TE.satisfy ident TE.<?> "identifier")
                 <|> TE.namedSymbol "(" *> x1 <* TE.namedSymbol ")"
    return x1
  where
     ident (x:_) = isAlpha x
     ident _     = False
     numbP (x:_) = isDigit x
     numbP _     = False

eParseExpr = U.t "eParseExpr"
    (let (p,_) = TE.fullParses (TE.parser expr) $ words "a + b * ( c + d )"
     in p)
    [Add (Var "a") (Mul (Var "b") (Add (Var "c") (Var "d")))]

test :: IO Counts
test =
    runTestTT $ TestList $ eExpr ++ eSimplify1 ++ eSimplify ++ eParseExpr
