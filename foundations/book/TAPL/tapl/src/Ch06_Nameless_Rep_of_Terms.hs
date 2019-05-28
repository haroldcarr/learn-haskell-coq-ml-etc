{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch06_Nameless_Rep_of_Terms where

import Protolude

type Var = Text

data Term
  = TmTrue
  | TmFalse
  | TmIf  Term Term Term
  | TmVar Var  Int
  | TmAbs Var  Term
  | TmApp Term Term
  deriving (Eq, Show)

type Context = [Var]

doNames
  :: (Int -> Var -> Int -> Context -> Either Text (Var, Int))
  -> Context -> Term -> Either Text Term
doNames onVar c = \case
  TmTrue        -> pure TmTrue
  TmFalse       -> pure TmFalse
  TmIf cond t f -> TmIf <$> doNames onVar c cond <*> doNames onVar c t <*> doNames onVar c f
  TmVar v n     -> onVar 0 v n c >>= \(v',n') -> pure $ TmVar v' n'
  TmAbs v t     -> TmAbs v <$> doNames onVar (v:c) t
  TmApp f v     -> TmApp <$> doNames onVar c f <*> doNames onVar c v

-- This does NOT remove existing names from Var nor Abs.
-- The Var name is left for debugging.
-- The Abs name is only used for restoring names to "nameless" Var.
rmNames :: Context -> Term -> Either Text Term
rmNames  = doNames lkup
 where
  lkup n v j (v':vs) = if v == v' then Right (v',n) else lkup (n+1) v j vs
  lkup _ v _     []  = Left $ show v <> " not present"

-- This expects Abs to have bound names.
mkNames :: Context -> Term -> Either Text Term
mkNames  = doNames lkup
 where
  lkup _ _ j vs = case atMay vs j of
    Just v  -> Right (v, j)
    Nothing -> Left $ show j <> " not present"

