> -- {-# LANGUAGE DataKinds #-}
> -- {-# LANGUAGE ExistentialQuantification #-}
> -- {-# LANGUAGE FlexibleInstances #-}
> -- {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE GADTs #-}
> -- {-# LANGUAGE TypeFamilies #-}
> -- {-# LANGUAGE TypeOperators #-}
>
> module P08_expr where

import Data.Char
import Debug.Trace

debug = flip trace

------------------------------------------------------------------------------
* Motivating example: Expression Evaluator (p. 8)

> data IntExpr'
>   = IntVal' Int
>   | AddInt' IntExpr' IntExpr'

:t AddInt' (IntVal' 5) (IntVal' 7)
-- AddInt' (IntVal' 5) (IntVal' 7) :: IntExpr'

> evaluate' :: IntExpr' -> Int
> evaluate' e = case e of
>   IntVal' i     -> i
>   AddInt' e1 e2 -> evaluate' e1 + evaluate' e2

evaluate' $ AddInt' (IntVal' 5) (IntVal' 7)
-- 12

Extend with booleans:

> data ExtExpr''
>   = IntVal''  Int
>   | BoolVal'' Bool
>   | AddInt''  ExtExpr'' ExtExpr''
>   | IsZero''  ExtExpr''

:t IsZero'' (BoolVal'' True)
-- IsZero'' (BoolVal'' True) :: ExtExpr''

PROBLEM: type checker accepts:

:t AddInt'' (IntVal'' 5) (BoolVal'' True)
-- AddInt'' (IntVal'' 5) (BoolVal'' True) :: ExtExpr''

Since =ExtExpr='' is NOT parameterized by return value type, evaluation function is complicated:

> evaluate'' :: ExtExpr'' -> Maybe (Either Int Bool)
> evaluate'' e = case e of
>   AddInt'' e1 e2 -> case (evaluate'' e1, evaluate'' e2) of
>                       (Just (Left i1), Just (Left  i2)) -> Just $ Left $ i1 + i2
>                       (Just (Left i1), Just (Right b2)) -> error "wrong type given to AddInt''" -- dynamic type-checking
>                       _                                 -> error "not implemented"
>   IntVal''  i    -> Just (Left  i)
>   BoolVal'' b    -> Just (Right b)
>   _              -> error "not implemented"

evaluate'' $ AddInt'' (IntVal'' 5) (IntVal'' 7)
-- Just (Left 12)

evaluate'' $ AddInt'' (IntVal'' 5) (BoolVal'' True)
-- *** Exception: wrong type given to AddInt''

FIX: represent expressions with values of types parameterized by return type (p. 9):

> data PhantomExpr''' t
>   = IntVal'''  Int
>   | BoolVal''' Bool
>   | AddInt'''  (PhantomExpr''' Int) (PhantomExpr''' Int)
>   | IsZero'''  (PhantomExpr''' Int)

=t= above is expr return value type.  Want =IntVal''' 5= to be typed =PhantomExpr''' Int=, but:

:t IntVal''' 5
-- IntVal''' 5 :: PhantomExpr''' t

:t BoolVal''' True
-- BoolVal''' True :: PhantomExpr''' t

PROBLEM: incorrect exprs still accepted by type checker:

:t IsZero''' (BoolVal''' True)
-- IsZero''' (BoolVal''' True) :: PhantomExpr''' t

FIX (trick): wrap value constructors with functions:

> intVal'''  :: Int                -> PhantomExpr''' Int
> intVal'''   = IntVal'''
> boolVal''' :: Bool               -> PhantomExpr''' Bool
> boolVal'''  = BoolVal'''
> isZero''   :: PhantomExpr''' Int -> PhantomExpr''' Bool
> isZero''    = IsZero'''

Now bad exprs rejected by type checker (p. 10):

:t isZero'' (boolVal''' True)
--    Couldn't match type `Bool' with `Int'
--    Expected type: PhantomExpr''' Int
--      Actual type: PhantomExpr''' Bool
:t isZero'' (intVal''' 5)
-- isZero'' (intVal''' 5) :: PhantomExpr''' Bool

PROBLEM: Want evaluate type signature to be (p. 10):

evaluate''' :: PhantomExpr''' t -> t
evaluate''' (IntVal''' i) = i
evaluate''' _             = error "not implemented"

but get:

    Couldn't match expected type `t' with actual type `Int'
      `t' is a rigid type variable bound by
          the type signature for evaluate'' :: PhantomExpr''' t -> t
          at r22.hs:150:15
    In the expression: i
    In an equation for evaluate'': evaluate'' (IntVal''' i) = i

because return type of value constructor =IntVal=''' is =Phantom t=
but =t= can be refined to any type:

:t IntVal''' 5 :: PhantomExpr''' Bool
-- IntVal''' 5 :: PhantomExpr''' Bool :: PhantomExpr''' Bool

Need to specify type signature of value constructors exactly
so pattern matching will cause type refinement for =IntVal=''' here.

Exactly what GADTs do.

(Useless) GADT version (all return general type, so no type refinement):

> data PhantomExpr'''' t where
>   IntVal''''  :: Int                                        -> PhantomExpr'''' t
>   BoolVal'''' :: Bool                                       -> PhantomExpr'''' t
>   AddInt''''  :: PhantomExpr'''' Int -> PhantomExpr'''' Int -> PhantomExpr'''' t
>   IsZero''''  :: PhantomExpr'''' Int                        -> PhantomExpr'''' t

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

evaluate $ AddInt (IntVal 5) (IntVal 7)
-- 12
