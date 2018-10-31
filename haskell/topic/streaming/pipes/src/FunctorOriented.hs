{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module FunctorOriented where

import           Control.Monad.State.Strict
import qualified Data.Bifunctor.Fix         as BF
import           Data.Traversable

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use <$>"    #-}
{-# ANN module "HLint: ignore Use sum"    #-}

------------------------------------------------------------------------------
-- my own blind looking around

data A b c
  = B b
  | C c
  deriving (Functor, Show)

data X y z
  = Y y
  | Z z
  deriving (Functor, Show)

data M n o
  = N (A n o)
  | O (X n o)
  deriving (Functor, Show)

a2b :: forall a b. A a b -> X a b
a2b (B b) = Y b
a2b (C c) = Z c

mno :: M n o -> M o n
mno (N (B n)) = N (C n)
mno (N (C o)) = N (B o)
mno (O (Y n)) = O (Z n)
mno (O (Z o)) = O (Y o)

mis
  :: (Functor f, Functor g, Show a)
  => f (g a)
  -> f (g String)
mis mis0 = fmap (fmap show) mis0

xx :: Either a (Maybe String)
xx  = mis (Right (Just 1))
yy :: Maybe (Maybe String)
yy  = mis (Just  (Just 1))
xz :: [[String]]
xz  = mis [      [     1]]

------------------------------------------------------------------------------
-- retrofitting an existing 'Tree a' (see below) to "two levels"

data T a r
  = Tip'
  | Leaf' a
  | Fork' r r

unwrap :: Tree a -> T a (Tree a)
unwrap Tip        = Tip'
unwrap (Leaf a)   = Leaf' a
unwrap (Fork l r) = Fork' l r

wrap :: T a (Tree a) -> Tree a
wrap Tip'        = Tip
wrap (Leaf' a)   = Leaf a
wrap (Fork' l r) = Fork l r

class SClass s where
  mapS   :: (x -> y)      -> s x -> s y
  accS   :: (x -> y -> y) -> s x ->   y -> y
  seqS   :: Monad m => s (m x) -> m (s x)
--  matchS :: s x -> s x -> Maybe [(x,x)]

instance SClass (T a) where
  mapS _   Tip'          = Tip'
  mapS _   (Leaf' a)     = Leaf' a
  mapS f   (Fork' l r)   = Fork' (f l) (f r)
  accS   _ Tip'        y = y
  accS   _ (Leaf' _)   y = y
  accS acc (Fork' l r) y = acc l (acc r y)
  seqS     Tip'          = return Tip'
  seqS     (Leaf' a)     = return (Leaf'  a)
  seqS     (Fork' l r)   = do { l' <- l; r' <- r; return (Fork' l' r') }

-- mt :: Num a => T a (Tree a)
-- mt  = mapS (+1) (unwrap (Fork (Leaf 1) (Leaf 2)))
-- at :: Num a => T a (T a (Tree a))
-- at  = accS (+) (Fork' (Leaf' 1) (Leaf' 2))

------------------------------------------------------------------------------
-- but above SClass is just Functor, Foldable, Traversable, etc

data Tree a
  = Tip
  | Leaf a
  | Fork (Tree a) (Tree a)
  deriving (Foldable, Functor, Show, Traversable)

instance Applicative Tree where
  pure                          = Leaf
  Tip          <*>   _          = Tip
  _            <*> Tip          = Tip
  Leaf l       <*> Leaf r       = Leaf (l r)
  l@(Leaf _)   <*> Fork  fl  fr = Fork (l <*> fl) (l <*> fr)
  Fork  fl  fr <*> l@(Leaf _)   = Fork (fl <*> l) (fr <*> l)
  Fork lfl lfr <*> Fork rfl rfr = Fork (Fork (lfl <*> rfl) (lfl <*> rfr))
                                       (Fork (lfr <*> rfl) (lfr <*> rfr))

instance Monad Tree where
  return         = pure
  Tip      >>= _ = Tip
  Leaf l   >>= f = f l
  Fork l r >>= f = Fork (l >>= f) (r >>= f)

ll, ff :: Tree Int
ll  = Leaf (+1) <*> Leaf 1
ff  = Fork (Leaf (+1)) (Leaf (*2)) <*> Fork (Leaf 2) (Leaf 4)
fmff :: Tree String
fmff = fmap show ff
flfll :: Tree Int
flfll = Fork (Leaf 1) (Fork (Leaf 2) (Leaf 4))
fld :: Int
fld  = foldr (+) 0 flfll

trv :: Maybe (Tree Int)
trv  = traverse (\n -> if n == 1 then Just n else Nothing) flfll

sqa :: Maybe (Tree Int)
sqa  = sequenceA (Fork (Leaf (Just 1)) (Fork (Leaf (Just 2)) (Leaf (Just 4))))

mm  :: IO (Tree ())
mm   = mapM print flfll

incr :: Enum s => State s s
incr = do
  v <- get
  put (succ v)
  return v

tag :: (Enum s, Traversable t)
    => s
    -> t a
    -> t (a, s)
tag s t = evalState (mapM step t) s -- traverse works (i.e., replace mapM)
 where
  step a = do
    i <- incr
    return (a, i)

tag' :: (Enum a, Traversable t)
     => a
     -> t b
     -> (a, t (b, a))
tag' a0 tb = mapAccumL step a0 tb
 where
  step a b = (succ a, (b, a))

tmm :: Tree (Int, Int)
tmm  = tag  0 flfll
tmm' :: (Int, Tree (Int, Int))
tmm' = tag' 0 flfll

------------------------------------------------------------------------------
-- "two level" via Fix/Free

data TreeF a r
  = TipF
  | LeafF a
  | ForkF r r
  deriving Functor

------------------------------------------------------------------------------
-- map NOT WORKING

type BFTre = BF.Fix TreeF

bftip     :: BFTre a
bftip      = BF.In TipF
bfleaf    :: BFTre a -> BFTre a
bfleaf a   = BF.In (LeafF a)
bffork    :: a -> a -> BFTre a
bffork l r = BF.In (ForkF l r)

bftflfll
  :: ( Num (BF.Fix TreeF a)
     , Num (BF.Fix TreeF (BF.Fix TreeF a))
     )
  => BF.Fix TreeF (BF.Fix TreeF (BF.Fix TreeF a))
bftflfll = bffork (bfleaf 1) (bffork (bfleaf 2) (bfleaf 4))

-- bfmap :: (a -> b) -> BFTre a -> BFTre b
-- bfmap = fmap

--------------------------------------------------
-- fold
newtype Fix f = Fix { unFix :: f (Fix f) }

type Tre a = Fix (TreeF a)

tip     :: Tre a
tip      = Fix TipF
leaf    :: a -> Tre a
leaf     = Fix . LeafF
fork    :: Tre a -> Tre a -> Tre a
fork l r = Fix (ForkF l r)

cata
  :: Functor f
  => (f a -> a)
  -> Fix f
  -> a
cata f = f . fmap (cata f) . unFix

tflfll :: Tre Int
tflfll = fork (leaf 1) (fork (leaf 2) (leaf 4))
tfld :: Int
tfld  = cata phi tflfll
  where
    phi = \case
      LeafF a   -> a
      TipF      -> 0
      ForkF l r -> l + r

