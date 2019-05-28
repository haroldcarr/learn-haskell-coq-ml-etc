{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- {-# LANGUAGE OverloadedStrings #-}

module Ch25_System where

import           Protolude

type Var = Text

------------------------------------------------------------------------------
-- types

data Ty
  = TyVar  Int Int
  | TyArr  Ty  Ty
  | TyAll  Var Ty
  | TySome Var Ty
  deriving (Eq, Show)

data Binding
  = NameBind
  | VarBind Ty
  | TyVarBind
  deriving (Eq, Show)

tyMap :: (Int -> Int -> Int -> Ty) -> Int -> Ty -> Ty
tyMap onVar = walk
 where
  walk c = \case
    TyArr  tyT1 tyT2 -> TyArr (walk c tyT1) (walk c tyT2)
    TyVar  x    n    -> onVar c x n
    TyAll  tyX  tyT2 -> TyAll  tyX (walk (c+1) tyT2)
    TySome tyX  tyT2 -> TySome tyX (walk (c+1) tyT2)

typeShiftAbove :: Int -> Int -> Ty -> Ty
typeShiftAbove d = tyMap (\c x n -> if x >= c then TyVar (x+d) (n+d) else TyVar x (n+d))

typeShift :: Int -> Ty -> Ty
typeShift d = typeShiftAbove d 0

typeSubst :: Ty -> Int -> Ty -> Ty
typeSubst tyS = tyMap (\j x n -> if x == j then typeShift j tyS else TyVar x n)

typeSubstTop :: Ty -> Ty -> Ty
typeSubstTop tyS tyT = typeShift (-1) (typeSubst (typeShift 1 tyS) 0 tyT)

------------------------------------------------------------------------------
-- terms

data Term
  = TmVar    Int  Int
  | TmAbs    Var  Ty   Term
  | TmApp    Term Term
  | TmTAbs   Var  Term
  | TmTApp   Term Ty
  | TmPack   Ty   Term Ty
  | TmUnpack Var  Var  Term Term
  deriving (Eq, Show)

tmMap
  :: (Int -> Int -> Int -> Term)
  -> (Int -> Ty -> Ty)
  -> Int
  -> Term
  -> Term
tmMap onVar onType = walk
 where
  walk c = \case
    TmVar    x    n            -> onVar    c x n
    TmAbs    x    tyT1 t2      -> TmAbs    x (onType c tyT1) (walk (c+1) t2)
    TmApp    t1   t2           -> TmApp    (walk c t1) (walk c t2)
    TmTAbs   tyX  t2           -> TmTAbs   tyX (walk (c+1) t2)
    TmTApp   t1   tyT2         -> TmTApp   (walk c t1) (onType c tyT2)
    TmPack   tyT1 t2   tyT3    -> TmPack   (onType c tyT1) (walk c t2) (onType c tyT3)
    TmUnpack tyX  x    t1   t2 -> TmUnpack tyX x (walk c t1) (walk (c+2) t2)


termShiftAbove :: Int -> Int -> Term -> Term
termShiftAbove d =
  tmMap (\c x n -> if x >= c then TmVar (x+d) (n+d) else TmVar x (n+d))
        (typeShiftAbove d)

termShift :: Int -> Term -> Term
termShift d = termShiftAbove d 0

termSubst :: Int -> Term -> Term -> Term
termSubst j0 s =
  tmMap (\j x n -> if x == j then termShift j s else TmVar x n)
        (\_j tyT -> tyT)
        j0

tyTermSubst :: Ty -> Int -> Term -> Term
tyTermSubst tyS =
  tmMap (\_c x n -> TmVar x n)
        (typeSubst tyS)

termSubstTop   :: Term -> Term -> Term
termSubstTop   s   t = termShift (-1) (termSubst   0 (termShift 1 s)     t)

tyTermSubstTop :: Ty   -> Term -> Term
tyTermSubstTop tyS t = termShift (-1) (tyTermSubst   (typeShift 1 tyS) 0 t)

------------------------------------------------------------------------------
-- evaluation

