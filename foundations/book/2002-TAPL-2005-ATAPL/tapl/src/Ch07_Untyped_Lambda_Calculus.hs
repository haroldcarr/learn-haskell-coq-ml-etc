{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch07_Untyped_Lambda_Calculus where

import           Protolude

type Var = Text

data Term
  = TmVar Var  Int   -- HC : no "total context length"
  | TmAbs Var  Term
  | TmApp Term Term
  deriving (Eq, Show)

printTm :: Context -> Term -> Text
printTm ctx = \case
  TmAbs var term -> "(lambda " <> var <> " . " <> printTm (extendCtx ctx var) term <> ")"
  TmApp t1  t2   -> "(" <> printTm ctx t1 <> " " <> printTm ctx t2 <> ")"
  TmVar var _    -> var

type Context = [(Var, Binding)]
data Binding = NameBind

extendCtx :: Context -> Text -> Context
extendCtx ctx x = (x, NameBind) : ctx

------------------------------------------------------------------------------

doNames
  :: (Int -> Var -> Int -> Context -> Either Text (Var, Int))
  -> Context -> Term -> Either Text Term
doNames onVar c = \case
  TmVar v n     -> onVar 0 v n c >>= \(v',n') -> pure $ TmVar v' n'
  TmAbs v t     -> TmAbs v <$> doNames onVar ((v,NameBind):c) t
  TmApp f v     -> TmApp   <$> doNames onVar c f <*> doNames onVar c v

-- This does NOT remove existing names from Var nor Abs.
-- The Var name is left for debugging.
-- The Abs name is only used for restoring names to "nameless" Var.
rmNames :: Context -> Term -> Either Text Term
rmNames  = doNames lkup
 where
  lkup n v j ((v',_):vs) = if v == v' then Right (v',n) else lkup (n+1) v j vs
  lkup _ v _     []      = Left $ show v <> " not present"

------------------------------------------------------------------------------
-- shifting and substitution

termShift :: Int -> Term -> Term
termShift d = walk 0
 where
  walk c = \case
    TmVar v  x  -> if x >= c then TmVar v (x+d) else TmVar v x
    TmAbs v  t1 -> TmAbs v           (walk (c+1) t1)
    TmApp t1 t2 -> TmApp (walk c t1) (walk  c    t2)

termSubst :: Int -> Term -> Term -> Term
termSubst j s = walk 0
 where
  walk c = \case
    TmVar v  x  -> if x == j + c then termShift c s else TmVar v x
    TmAbs v  t1 -> TmAbs v           (walk (c+1) t1)
    TmApp t1 t2 -> TmApp (walk c t1) (walk  c    t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

------------------------------------------------------------------------------
-- eval

isVal :: Term -> Bool
isVal = \case TmAbs {} -> True; _ -> False

data Eval1NoMatch = Eval1NoMatch deriving (Eq, Show)

eval1
  :: Term
  -> Either Eval1NoMatch Term
eval1 = \case
  TmApp (TmAbs _ t12) v2 | isVal v2 -> pure $ termSubstTop v2 t12
  TmApp t1 t2 | isVal t1            ->      TmApp t1 <$> eval1 t2
  TmApp t1 t2                       -> flip TmApp t2 <$> eval1 t1
  _                                 -> Left Eval1NoMatch

eval
  :: Term
  -> Term
eval t = case eval1 t of
  Left Eval1NoMatch -> t
  Right t'          -> eval t'

