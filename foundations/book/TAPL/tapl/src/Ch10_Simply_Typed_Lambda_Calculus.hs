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

getBinding :: Context -> Int -> Either Text Binding
getBinding ctx idx = foldr go (Left $ show idx <> " not found") (zip [0 ..] ctx)
 where go (i,(_,b)) r = if i == idx then Right b else r

getTypeFromContext :: Context -> Int -> Either Text Ty
getTypeFromContext ctx idx = getBinding ctx idx >>= go
 where go = \case VarBind ty -> Right ty; x -> Left $ show idx <> " wrong binding " <> show x

data Ty
  = TyBool
  | TyArr Ty Ty -- Arrow
  deriving (Eq, Show)

data Term
  = TmTrue
  | TmFalse
  | TmIf  Term Term Term
  | TmVar Var  Int
  | TmAbs Var  Ty   Term
  | TmApp Term Term
  deriving (Eq, Show)

------------------------------------------------------------------------------

typeOf :: Context -> Term -> Either Text Ty
typeOf ctx = \case
  TmTrue             -> pure TyBool
  TmFalse            -> pure TyBool
  TmVar _v  idx      -> getTypeFromContext ctx idx
  -- Note: Abs are input with ONLY their input type.  This branch fills in the output type.
  TmAbs var typ body -> TyArr typ <$> typeOf (addBinding ctx var (VarBind typ)) body
  tif@(TmIf c t f)   -> do
    tc <- typeOf ctx c
    if tc /= TyBool then Left $ "if guard not given TyBool " <> show tif
    else do
      tyT <- typeOf ctx t
      tyF <- typeOf ctx f
      if tyT /= tyF then Left $ "if arms have different types " <> show tif
      else pure tyT
  t@(TmApp t1 t2)    ->
    typeOf ctx t1 >>= \case
      TyArr tyT11 tyT12 -> do
        tyT2 <- typeOf ctx t2
        if tyT2 /= tyT11
          then Left $ "param type mismatch " <> show t
          else pure tyT12
      _ ->
        Left $ "arrow type expected " <> show t
