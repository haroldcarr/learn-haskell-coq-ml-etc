> {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
> {-# OPTIONS_GHC -fno-warn-type-defaults      #-}

> -- {-# LANGUAGE DataKinds #-}
> -- {-# LANGUAGE ExistentialQuantification #-}
> -- {-# LANGUAGE FlexibleInstances #-}
> -- {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE GADTs #-}
> -- {-# LANGUAGE TypeFamilies #-}
> -- {-# LANGUAGE TypeOperators #-}
>
> module P08_expr where

> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t, e)

------------------------------------------------------------------------------
* Motivating example: Expression Evaluator (p. 8)

> data IntExpr1
>   = IntVal1 Int
>   | AddInt1 IntExpr1 IntExpr1

:t AddInt1 (IntVal1 5) (IntVal1 7)
-- AddInt1 (IntVal1 5) (IntVal1 7) :: IntExpr1

> evaluate1 :: IntExpr1 -> Int
> evaluate1 e = case e of
>   IntVal1 i     -> i
>   AddInt1 e1 e2 -> evaluate1 e1 + evaluate1 e2

> te1 = U.t "te1"
>   (evaluate1 $ AddInt1 (IntVal1 5) (IntVal1 7))
>   12

Extend with booleans:

> data ExtExpr2
>   = IntVal2  Int
>   | BoolVal2 Bool
>   | AddInt2  ExtExpr2 ExtExpr2
>   | IsZero2  ExtExpr2
>   deriving (Eq, Show)

:t IsZero2 (BoolVal2 True)
-- IsZero2 (BoolVal2 True) :: ExtExpr2

PROBLEM: type checker accepts:

:t AddInt2 (IntVal2 5) (BoolVal2 True)
-- AddInt2 (IntVal2 5) (BoolVal2 True) :: ExtExpr2

Since =ExtExpr2= is NOT parameterized by return value type, evaluation function is complicated:

> evaluate2 :: ExtExpr2 -> Maybe (Either Int Bool)
> evaluate2 e = case e of
>   AddInt2 e1 e2 -> case (evaluate2 e1, evaluate2 e2) of
>                       (Just (Left i1), Just (Left  i2)) -> Just $ Left $ i1 + i2
>                       (Just (Left i1), Just (Right b2)) -> error "wrong type given to AddInt2" -- dynamic type-checking
>                       _                                 -> error "not implemented"
>   IntVal2  i    -> Just (Left  i)
>   BoolVal2 b    -> Just (Right b)
>   _             -> error "not implemented"

> te21 = U.t "te21"
>   (evaluate2 $ AddInt2 (IntVal2 5) (IntVal2 7))
>   (Just (Left 12))

> {- TODO : error detection in test framework not working correctly
> te22 = U.e "te22"
>   (evaluate2 $ AddInt2 (IntVal2 5) (BoolVal2 True))
>   "wrong type given to AddInt2"
> -}

FIX: represent expressions with values of types parameterized by return type (p. 9):

> data PhantomExpr3 t
>   = IntVal3  Int
>   | BoolVal3 Bool
>   | AddInt3  (PhantomExpr3 Int) (PhantomExpr3 Int)
>   | IsZero3  (PhantomExpr3 Int)

=t= above is expr return value type.  Want =IntVal3 5= to be typed =PhantomExpr3 Int=, but:

:t IntVal3 5
-- IntVal3 5 :: PhantomExpr3 t

:t BoolVal3 True
-- BoolVal3 True :: PhantomExpr3 t

PROBLEM: incorrect exprs still accepted by type checker:

:t IsZero3 (BoolVal3 True)
-- IsZero3 (BoolVal3 True) :: PhantomExpr3 t

FIX (trick): wrap value constructors with functions:

> intVal3  :: Int              -> PhantomExpr3 Int
> intVal3   = IntVal3
> boolVal3 :: Bool             -> PhantomExpr3 Bool
> boolVal3  = BoolVal3
> isZero2  :: PhantomExpr3 Int -> PhantomExpr3 Bool
> isZero2   = IsZero3

Now bad exprs rejected by type checker (p. 10):

:t isZero2 (boolVal3 True)
--    Couldn't match type `Bool' with `Int'
--    Expected type: PhantomExpr3 Int
--      Actual type: PhantomExpr3 Bool
:t isZero2 (intVal3 5)
-- isZero2 (intVal3 5) :: PhantomExpr3 Bool

PROBLEM: Want evaluate type signature to be (p. 10):

evaluate3 :: PhantomExpr3 t -> t
evaluate3 (IntVal3 i) = i
evaluate3 _           = error "not implemented"

but get:

    Couldn't match expected type `t' with actual type `Int'
      `t' is a rigid type variable bound by
          the type signature for evaluate2 :: PhantomExpr3 t -> t
          at r22.hs:150:15
    In the expression: i
    In an equation for evaluate2: evaluate2 (IntVal3 i) = i

because return type of value constructor =IntVal3= is =Phantom t=
but =t= can be refined to any type:

:t IntVal3 5 :: PhantomExpr3 Bool
-- IntVal3 5 :: PhantomExpr3 Bool :: PhantomExpr3 Bool

Need to specify type signature of value constructors exactly
so pattern matching will cause type refinement for =IntVal3= here.

Exactly what GADTs do.

(Useless) GADT version (all return general type, so no type refinement):

> data PhantomExpr4 t where
>   IntVal4  :: Int                                  -> PhantomExpr4 t
>   BoolVal4 :: Bool                                 -> PhantomExpr4 t
>   AddInt4  :: PhantomExpr4 Int -> PhantomExpr4 Int -> PhantomExpr4 t
>   IsZero4  :: PhantomExpr4 Int                     -> PhantomExpr4 t

FIX: Final (useful) GADT version (value constructors return specific types) (p. 11):

> data Expr t where
>   IntVal  :: Int                             -> Expr Int
>   BoolVal :: Bool                            -> Expr Bool
>   AddInt  :: Expr Int  -> Expr Int           -> Expr Int
>   IsZero  :: Expr Int                        -> Expr Bool
>   If      :: Expr Bool -> Expr t   -> Expr t -> Expr t

Bad exprs rejected:

:t IsZero (BoolVal True)
--    Couldn't match type `Bool' with `Int'
--    Expected type: Expr Int
--      Actual type: Expr Bool

Specific type returned by =IsZero=:

:t IsZero (IntVal 5)
-- IsZero (IntVal 5) :: Expr Bool

Well-defined evaluator / pattern matching causes type refinement:

> evaluate :: Expr t -> t
> evaluate (IntVal i)     = i                           -- right hand side has type Int
> evaluate (BoolVal b)    = b                           -- right hand side has type Bool
> evaluate (AddInt e1 e2) = evaluate e1 + evaluate e2   -- right hand side has type Expr Int
>                                                       --       and types of e1 e2 must be Expr Int
> evaluate (IsZero e)     = evaluate e == 0
> evaluate (If e1 e2 e3)  = if evaluate e1 then evaluate e2 else evaluate e3

AddInt (IntVal 5) (BoolVal True)
--    Couldn't match type `Bool' with `Int'
--    Expected type: Expr Int
--      Actual type: Expr Bool

:t evaluate $ AddInt (IntVal 5) (IntVal 7)
-- evaluate $ AddInt (IntVal 5) (IntVal 7) :: Int

> te = U.t "te"
>   (evaluate $ AddInt (IntVal 5) (IntVal 7))
>   12

------------------------------------------------------------------------------

> testP08 :: IO Counts
> testP08  =
>     runTestTT $ TestList $ te1 ++ te21 {- ++ te22 -} ++ te
