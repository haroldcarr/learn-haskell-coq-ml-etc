{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch07_Untyped_Lambda_Calculus where

import           Protolude

type Var = Text

data Term
  = TmVar Var        -- HC : no de Bruijn index - just its name : user keeps them distinct
  | TmAbs Var  Term
  | TmApp Term Term
  deriving (Eq, Show)

printTm :: Context -> Term -> Text
printTm ctx = \case
  TmAbs var term  -> "(lambda " <> var <> " . " <> printTm (extendCtx ctx var) term <> ")"
  TmApp t1 t2     -> "(" <> printTm ctx t1 <> " " <> printTm ctx t2 <> ")"
  TmVar x         -> x

type Context = [(Var, Binding)]
data Binding = NameBind

extendCtx :: Context -> Text -> Context
extendCtx ctx x = (x, NameBind) : ctx

------------------------------------------------------------------------------

isVal :: Term -> Bool
isVal = \case TmAbs {} -> True; _ -> False

data Eval1NoMatch = Eval1NoMatch deriving (Eq, Show)

eval1
  :: Term
  -> Either Eval1NoMatch Term
eval1 = \case
  TmApp (TmAbs var body) val | isVal val -> pure $ applyOneLevel var val body
  TmApp t1 t2 | isVal t1                 ->      TmApp t1 <$> eval1 t2
  TmApp t1 t2                            -> flip TmApp t2 <$> eval1 t1
  _                                      -> Left Eval1NoMatch

-- Used in place of de Bruijn indices.  But only looks in immediate body, no deeper.
applyOneLevel :: Var -> Term -> Term -> Term
applyOneLevel var val = \case
  TmVar v | v == var -> val
  t@TmVar {}         -> t
  TmAbs v t          -> TmAbs v (applyOneLevel var val t)
  TmApp t1 t2        -> TmApp (applyOneLevel var val t1) (applyOneLevel var val t2)

eval
  :: Term
  -> Term
eval t = case eval1 t of
  Left Eval1NoMatch -> t
  Right t'          -> eval t'

