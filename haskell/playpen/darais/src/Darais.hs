{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeApplications #-}

module Darais where

-- import Control.Monad
import           Control.Monad.Writer
-- import Data.Functor.Identity
import           Debug.Trace
import           Test.HUnit
import           Test.HUnit.Util as T

newtype Fix f = Fix { unFix :: f (Fix f) }

data ExprF r = Val Int | Add r r deriving (Foldable, Functor, Show, Traversable)
type Expr = Fix ExprF

adi :: (Monoid b, Applicative s, Traversable t)
    => (t a -> a)
    -> ((Fix t -> (b, s a)) -> Fix t -> (b, s a))
    -> Fix t -> (b, s a)
adi f g = g (go . traverse (adi f g) . unFix)
  where
    go = fmap (fmap f . sequenceA)

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
cataM2 _f _g _t = undefined
  {-
  let tcm = traverse (cataM2 f g) (unFix t)
  aa <- (tcm >>= (f . fmap))
  g aa
  -}

cataM :: (Applicative m, Monad m, Traversable t)
      => (t a -> m a)
      -> Fix t
      -> m a
cataM f = (f =<<) . traverse (cataM f) . unFix

eval :: Expr -> ((), Maybe Int)
eval  = adi phi psi
  where
    phi          (Val x)    = x
    phi          (Add x y)  = x + y
    psi k v@(Fix (Val _))   = trace "Evaluating Val" $ k v
    psi k v@(Fix (Add _ _)) = trace "Evaluating Add" $ k v

ph :: ExprF Int -> Int
ph (Val x)   = x
ph (Add x y) = x + y
ps :: (Fix ExprF -> t) -> Fix ExprF -> t
ps k v@(Fix (Val _))   = k v
ps k v@(Fix (Add _ _)) = k v

e1, e2 :: [Test]
e1 = T.tt "e1"
     [ eval                                                           (Fix (Val 3))
     , adi ph ps                                                      (Fix (Val 3))
     , ps (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix) (Fix (Val 3))
     ,    (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix) (Fix (Val 3))
     ,    (fmap (fmap ph . sequenceA) . traverse (adi ph ps))              (Val 3)
     ,     fmap (fmap ph . sequenceA)  (traverse (adi ph ps)               (Val 3))
     ,     fmap (fmap ph . sequenceA)  (traverse (ps (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix))               (Val 3))
     ,     fmap (fmap ph . sequenceA)           ((),                        Val 3)
     ,                                          ((), (fmap ph . sequenceA) (Val 3))
     ,                                          ((), (fmap ph . sequenceA) (Val 3))
     ,                                          ((),  fmap ph   (Just      (Val 3)))
     ,                                          ((),             Just (ph  (Val 3)))
     ,                                          ((),             Just (ph  (Val 3)))
     ,                                          ((),             Just           3)
     ]
     ((), Just 3)
{-
traverse :: (Applicative f, Traversable t)
         => (a -> f b)
         -> t a
         -> f (t b)

       _ :: (Fix ExprF -> (b0, s0 Int))
         -> ExprF r0
         -> ((), ExprF (Maybe Int))
-}
e2 = T.t "e2"
     (eval (Fix (Add (Fix (Val 2)) (Fix (Val 3)))))
     ((), Just 5)

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

emt2 :: [Test]
emt2 = T.t "emt2"
       (runWriterT em2)
       [(Sum {getSum = 3},Sum {getSum = 10})]

------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $ e1 ++ e2 ++ emt2
