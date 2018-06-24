{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

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
{-
instance (Eq a, Eq i, Eq (XVar i a), Eq (XExp i a)) => Eq (ExpX i a) where
  (LitX _ l)     == (LitX _ r)     = l == r
  (VarX   l)     == (VarX   r)     = l == r
  (AbsX _ l)     == (AbsX _ r)     = l == r
  (AppX _ l1 l2) == (AppX _ r1 r2) = l1 == r1 && l2 == r2
  (ExpX   l)     == (ExpX   r)     = l == r
  _              ==         _      = False

instance ( Show a
         , Show i
         , Show (XLit i a)
         , Show (XVar i a)
         , Show (XAbs i a)
         , Show (XApp i a)
         , Show (XExp i a)
         ) => Show (ExpX i a) where
  show (LitX i l)     = show "(LitX " ++ show i ++ " " ++ show l ++ ")"
  show (VarX   l)     = show "(VarX " ++ show l ++ ")"
  show (AbsX i l)     = show "(AbsX " ++ show i ++ " " ++ show l ++ ")"
  show (AppX i l1 l2) = show "(AppX " ++ show i ++ " " ++ show l1 ++ " " ++ show l2 ++ ")"
  show (ExpX   l)     = show "(ExpX " ++ show l ++ ")"
-}
------------------------------------------------------------------------------
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

------------------------------------------------------------------------------
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
{-
instance ( Eq a
         , Eq i
         , Eq (XLit i a)
         , Eq (XVar i a)
         , Eq (XAbs i a)
         , Eq (XApp i a)
         , Eq (XExp i a)
         ) => Eq (ExpAnn a) where
  (LitAnn   l)   == (LitAnn   r)   = l == r
  (VarAnn _ l)   == (VarAnn _ r)   = l == r
  (AbsAnn _ l)   == (AbsAnn _ r)   = l == r
  (AppAnn l1 l2) == (AppAnn r1 r2) = l1 == r1 && l2 == r2
  _              ==         _      = False
-}
instance Show a => Show (ExpX Ann a) where
  show (LitAnn   a) = "(LitAnn " ++ show a ++ ")"
  show (VarAnn _ a) = "(VarAnn " ++ show a ++ ")"
  show (AbsAnn _ a) = "(AbsAnn " ++ show a ++ ")"
  show (AppAnn f a) = "(AppAnn " ++ show f ++ " " ++ show a ++ ")"
  show _            = "Show (ExpX Ann a)"

------------------------------------------------------------------------------
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
pattern LetLet n v e <- ExpX (n,v,e) where LetLet n v e = ExpX (n,v,e) -- HC : where bi-directional

instance Show a => Show (ExpX Let a) where
  show (LitLet   a)   = "(LitLet " ++ show a ++ ")"
  show (VarLet   a)   = "(VarLet " ++ show a ++ ")"
  show (AbsLet v a)   = "(AbsLet " ++ show v ++ " " ++ show a ++ ")"
  show (AppLet f a)   = "(AppLet " ++ show f ++ " " ++ show a ++ ")"
  show (LetLet n v e) = "(LetLet " ++ show n ++ " " ++ show v ++ " " ++ show e ++ ")"
  show x              = error ("Show (ExpX Let a): " ++ show x)

------------------------------------------------------------------------------

-- see X_3 for functions (e.g., eval) that operate on this data
