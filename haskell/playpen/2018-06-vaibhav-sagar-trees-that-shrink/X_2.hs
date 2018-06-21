{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeFamilies    #-}

module X_2 where

import           Data.Void

{-
Ed Kmett says
- using Void makes approach error prone
- use strict fields and () instead
- additional benefit : can disable constructors with Void so we can actually have trees that shrink
-}

data ExpX i a
  = LitX !(XLit i a) a
  | VarX !(XVar i a)
  | AbsX !(XAbs i a) (ExpX i a)
  | AppX !(XApp i a) (ExpX i a) (ExpX i a)
  | ExpX !(XExp i a) -- this constructor is sometimes disabled below
                     -- via : XExp i a = Void

type family XLit i a
type family XVar i a
type family XAbs i a
type family XApp i a
type family XExp i a

data UD
type ExpUD a            = ExpX UD a
type instance XLit UD a = ()
type instance XVar UD a = Int
type instance XAbs UD a = ()
type instance XApp UD a = ()
type instance XExp UD a = Void

pattern LitUD :: a -> ExpUD a
pattern LitUD a   <- LitX _ a     where LitUD a = LitX () a
pattern VarUD :: Int -> ExpUD a
pattern VarUD i   <- VarX i       where VarUD i = VarX i
pattern AbsUD :: ExpUD a -> ExpUD a
pattern AbsUD a   <- AbsX _ a     where AbsUD a = AbsX () a
pattern AppUD :: ExpUD a -> ExpUD a -> ExpUD a
pattern AppUD f a <- AppX _ f a where AppUD f a = AppX () f a

instance Show a => Show (ExpX UD a) where
  show (LitUD a)   = "(LitUD " ++ show a ++ ")"
  show (VarUD a)   = "(VarUD " ++ show a ++ ")"
  show (AbsUD a)   = "(AbsUD " ++ show a ++ ")"
  show (AppUD f a) = "(AppUD " ++ show f ++ " " ++ show a ++ ")"
  show _           = "Show (ExpX UD a)"

data Ann
type ExpAnn a            = ExpX Ann a
type instance XLit Ann a = ()
type instance XVar Ann a = (String, Int)
type instance XAbs Ann a = String
type instance XApp Ann a = ()
type instance XExp Ann a = Void

pattern LitAnn :: a -> ExpAnn a
pattern LitAnn a   <- LitX _ a       where LitAnn a = LitX () a
pattern VarAnn :: String -> Int -> ExpAnn a
pattern VarAnn s i <- VarX (s,i)   where VarAnn s i = VarX (s,i)
pattern AbsAnn :: String -> ExpAnn a -> ExpAnn a
pattern AbsAnn s a <- AbsX s a     where AbsAnn s a = AbsX s a
pattern AppAnn :: ExpAnn a -> ExpAnn a -> ExpAnn a
pattern AppAnn f a <- AppX _ f a   where AppAnn f a = AppX () f a

data Let
type ExpLet a            = ExpX Let a
type instance XLit Let a = ()
type instance XVar Let a = String
type instance XAbs Let a = String
type instance XApp Let a = ()
type instance XExp Let a = (String, ExpLet a, ExpLet a)

pattern LitLet :: a -> ExpLet a
pattern LitLet a <- LitX _ a      where LitLet a = LitX () a
pattern VarLet :: String -> ExpLet a
pattern VarLet s <- VarX s        where VarLet s = VarX s
pattern AbsLet :: String -> ExpLet a -> ExpLet a
pattern AbsLet s a <- AbsX s a    where AbsLet s a = AbsX s a
pattern AppLet :: ExpLet a -> ExpLet a -> ExpLet a
pattern AppLet f a <- AppX _ f a  where AppLet f a = AppX () f a
pattern LetLet :: forall i a b c d
                . XExp i a ~ (b, c, d)
               => b -> c -> d -> ExpX i a
pattern LetLet n v e <- ExpX (n,v,e)

-- see X_3 for functions (e.g., eval) that operate on this data
