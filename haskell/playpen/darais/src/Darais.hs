{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Darais where

import Debug.Trace
-- import Control.Monad.State
import Control.Monad.Writer

newtype Fix f = Fix { unFix :: f (Fix f) }

data ExprF r = Val Int | Add r r
    deriving (Functor, Foldable, Traversable)

type Expr = Fix ExprF

cata' :: Monoid b
      => (ExprF a -> a)
      -> ((Expr -> (b, Maybe a)) -> (Expr -> (b, Maybe a)))
      -> (Expr -> (b, Maybe a))
cata' f g = g (fmap (fmap f . sequenceA) . traverse (cata' f g) . unFix)

cata :: Functor f
     => (f a -> a)
     -> (Fix f -> a)
cata f = f . fmap (cata f) . unFix

cataM' :: (Applicative m, Monad m, Traversable t)
       => (t a -> m a)
       -> ((Fix t -> m a) -> (Fix t -> m a))
       -> (Fix t -> m a)
cataM' f g = g ((f =<<) . traverse (cataM' f g) . unFix)

cataM2 :: (Monoid b, Applicative m, Monad m, Traversable t)
       => (t a -> m a)
       -> ((Fix t -> m (b, Maybe a)) -> (Fix t -> m (b, Maybe a)))
       -> (Fix t -> m (b, Maybe a))
cataM2 f g (Fix t) = do
  let _tcm = traverse (cataM2 f g) t
  undefined

cataM :: (Applicative m, Monad m, Traversable t)
      => (t a -> m a)
      -> Fix t
      -> m a
cataM f = (f =<<) . traverse (cataM f) . unFix

eval' :: Expr -> ((), Maybe Int)
eval' = cata' phi psi
  where
    phi          (Val x)    = x
    phi          (Add x y)  = x + y
    psi k v@(Fix (Val _))   = trace "Evaluating Val" $ k v
    psi k v@(Fix (Add _ _)) = trace "Evaluating Add" $ k v

e1, e2 :: ((), Maybe Int)
e1 = eval' (Fix (Val 3))
e2 = eval' (Fix (Add (Fix (Val 2)) (Fix (Val 3))))

-- evalM :: (Applicative m, Monad m) => Expr -> m Int
evalM' :: Expr -> WriterT (Sum Int) [] (Sum Int)
evalM' = cataM' phi psi
  where
    phi          (Val x)    = do { tell 3; return (Sum x) }
    phi          (Add x y)  = do { tell 4; return (x + y) }
    psi k v@(Fix (Val _))   = trace "Evaluating Val" $ k v
    psi k v@(Fix (Add _ _)) = trace "Evaluating Add" $ k v

evalM'2 :: Expr -> WriterT (Sum Int) [] (Sum Int)
evalM'2 = cataM' phi psi
  where
    phi          (Val x)    = return (Sum x)
    phi          (Add x y)  = return (x + y)
    psi k v@(Fix (Val _))   = do { tell 3; k v }
    psi k v@(Fix (Add _ _)) = do { tell 4; k v }

em2 :: WriterT (Sum Int) [] (Sum Int)
em2 = evalM'2 (Fix (Add (Fix (Val 1)) (Fix (Val 2))))
