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

------------------------------------------------------------------------------
-- p 78 shifting : renumbering indices of Vars in Term

{-
When a substitution goes under an Abs (i.e., [x ->s](λy.x),
assuming 1 is the index of x in outer context),
the context in which the substitution is taking place becomes one var longer than the original,
so increment the indices of free Vars in s so they keep referring to
the same names in new context as they did before.

But do NOT shift BOUND Vars in s.

e.g., s = 2 (λ.0) (i.e., s = z (λw.w), assume 2 is index of z)
- shift the 2 but not the 0

shifting function takes a "cutoff" parameter 'c' that controls which Vars should be shifted
- 'c' starts at 0 : meaning all variables should be shifted
- gets (+1) each time the shifting function goes through a binder

when calculating shift t
- know that 't' comes from inside c-many binders in the original argument to shift
- so all Vars k < c in 't' are bound in the original argument and should not be shifted
-        Vars k ≥ c in 't' are free and should be shifted
-}
