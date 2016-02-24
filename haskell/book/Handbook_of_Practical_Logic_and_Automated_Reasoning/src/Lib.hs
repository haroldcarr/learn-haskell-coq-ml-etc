{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Applicative (many, some, (<|>))
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace)
import           Test.HUnit          (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util     as U (t, tt)
import qualified Text.Earley         as TE (Grammar, Prod, Report, fullParses,
                                            parser, rule, satisfy, symbol,
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

exExpr = Add (Mul (Add (Mul (Const 0)
                            (Var "x"))
                       (Const 1))
                  (Const 3))
             (Const 12)

eSimplify = U.t "eSimplify"
    (simplify exExpr)
    (Const 15)

-- 1.7

grammar :: forall r. TE.Grammar r (TE.Prod r String Char Expr)
grammar = mdo
    whitespace <- TE.rule $ many $ TE.satisfy isSpace
    let token :: TE.Prod r String Char a -> TE.Prod r String Char a
        token p = whitespace *> p
        sym x   = token $ TE.symbol x TE.<?> [x]
        ident   = token $ (:) <$> TE.satisfy isAlpha <*> many (TE.satisfy isAlphaNum) TE.<?> "identifier"
        num     = token $ some (TE.satisfy isDigit) TE.<?> "number"
    expr0 <- TE.rule
        $ (Const . read)  <$> num
        <|> Var  <$> ident
        <|> sym '(' *> expr2 <* sym ')'
    expr1 <- TE.rule
        $ Mul <$> expr1 <* sym '*' <*> expr0
        <|> expr0
    expr2 <- TE.rule
        $ Add <$> expr2 <* sym '+' <*> expr1
        <|> expr1
    return $ expr2 <* whitespace

parseExpr :: String -> ([Expr], TE.Report String String)
parseExpr = TE.fullParses (TE.parser grammar)

parseExpr' :: String -> Expr
parseExpr' e =  let (p:_,_) = parseExpr e in p

eParseExpr1 = U.t "eParseExpr1"
    (parseExpr' "a + b * ( c + d )")
    (Add (Var "a") (Mul (Var "b") (Add (Var "c") (Var "d"))))

eParseExpr2 = U.t "eParseExpr3"
    (parseExpr' "(0 * x + 1) * 3 + 12")
    exExpr

-- TODO : parseResult is different from result shown in book - are they equivalent?
parseResult =
     Mul (Add (Add (Var "x1")
                   (Var "x2"))
              (Var "x3"))
         (Add (Add (Add (Const 1)
                        (Const 2))
                   (Mul (Const 3)
                        (Var "x")))
              (Var "y"))

bookResult =
     Mul (Add (Var "x1")
              (Add (Var "x2")
                   (Var "x3")))
         (Add (Const 1)
              (Add (Const 2)
                   (Add (Mul (Const 3)
                             (Var "x"))
                        (Var "y"))))

eParseExpr3 = U.t "eParseExpr3"
    (parseExpr' "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)")
    parseResult

test :: IO Counts
test =
    runTestTT $ TestList $ eExpr ++ eSimplify1 ++ eSimplify ++ eParseExpr1 ++ eParseExpr2 ++ eParseExpr3
