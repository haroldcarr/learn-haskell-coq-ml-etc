{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module X_3 where

import Data.Map.Strict as Map
-- Works with either import (one at a time, of course)
-- import X_1
import X_2

-- | preserves names
--   rewrites let bindings
desugar :: Map.Map String Int -> ExpLet a -> ExpAnn a
desugar env expr = case expr of
  LitLet a          -> LitAnn a
  VarLet name       -> VarAnn name (env Map.! name)
  AbsLet name expr' -> let env'  = Map.map succ env
                           env'' = Map.insert name 0 env'
                       in AbsAnn name (desugar env'' expr')
  AppLet f x        -> AppAnn (desugar env f) (desugar env x)
  LetLet n v expr'  -> desugar env (AppLet (AbsLet n expr') v)
  _                 -> error "impossible?"

-- | removes names
anonymise :: ExpAnn a -> ExpUD a
anonymise expr = case expr of
  LitAnn a   -> LitUD a
  VarAnn _ i -> VarUD i
  AbsAnn _ e -> AbsUD (anonymise e)
  AppAnn f x -> AppUD (anonymise f) (anonymise x)
  _          -> error "impossible?"

-- | operates on undecorated expressions
eval :: [a] -> ExpUD a -> a
eval env expr = case expr of
  LitUD a   -> a
  VarUD i   -> env !! i
  AbsUD f   -> eval env f
  AppUD f x -> let x' = eval env x
               in eval (x':env) f
  _         -> error "impossible?"

identity,konst :: ExpLet a
identity  = AbsLet "i" (VarLet "i")
konst     = AbsLet "x" (AbsLet "y" (VarLet "x"))

ev :: Int
ev = eval [] . anonymise . desugar Map.empty
   $ AppLet (AppLet konst (LitLet 1)) (LitLet 2)

-- summary
-- - composable compiler passes : smaller, easier to write and to think about
