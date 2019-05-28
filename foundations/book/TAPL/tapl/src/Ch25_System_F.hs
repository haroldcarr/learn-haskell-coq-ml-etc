{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch25_System where

import           Protolude

type Var = Text

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

type Context = [(Var, Binding)]

addBinding :: Context -> Var -> Binding -> Context
addBinding ctx x bind = (x,bind) : ctx

getBinding :: Context -> Var -> Either Text Binding
getBinding ctx var = foldr go (Left $ show var <> " not found") ctx
 where go (v,b) r = if v == var then Right b else r

getTypeFromContext :: Context -> Var -> Either Text Ty
getTypeFromContext ctx var = getBinding ctx var >>= go
 where go = \case VarBind ty -> Right ty; x -> Left $ show var <> " wrong binding " <> show x

data Term
  = TmVar    Var
  | TmTAbs   Var  Term
  | TmTApp   Term Ty
  | TmPack   Ty   Term Ty
  | TmUnpack Text Text Term Term
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
