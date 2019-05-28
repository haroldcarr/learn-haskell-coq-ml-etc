{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch10_Simply_Typed_Lambda_Calculus where

import           Protolude

type Var = Text

data Binding
  = NameBind
  | VarBind Ty
  deriving (Eq, Show)

type Context = [(Var, Binding)]

addBinding :: Context -> Var -> Binding -> Context
addBinding ctx x bind = (x,bind) : ctx

getBinding :: Context -> Var -> Binding
getBinding ctx var = foldr go (panic $ show var <> " not found") ctx
 where go (v,b) r = if v == var then b else r

getTypeFromContext :: Context -> Var -> Ty
getTypeFromContext ctx var = go $ getBinding ctx var
 where go = \case VarBind ty -> ty; x -> panic $ show var <> " wrong binding " <> show x

data Ty
  = TyBool
  | TyArr Ty Ty -- Arrow
  deriving (Eq, Show)

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmVar Var
  | TmAbs Var Ty Term
  | TmApp Term Term
  deriving (Eq, Show)

------------------------------------------------------------------------------

typeOf :: Context -> Term -> Ty
typeOf ctx = \case
  TmTrue             -> TyBool
  TmFalse            -> TyBool
  TmVar var          -> getTypeFromContext ctx var
  TmAbs var typ body -> TyArr typ (typeOf (addBinding ctx var (VarBind typ)) body)
  tif@(TmIf c t f)   ->
    if typeOf ctx c /= TyBool then panic $ "if guard not given TyBool " <> show tif
    else
      let tyT = typeOf ctx t
       in if typeOf ctx f /= tyT then panic $ "if arms have different types " <> show tif
          else tyT
  t@(TmApp t1 t2)    -> case typeOf ctx t1 of
    TyArr tyT11 tyT12 ->
      if typeOf ctx t2 /= tyT11 then panic $ "param type mismatch " <> show t
      else tyT12
    _ ->
      panic $ "arrow type expected " <> show t
