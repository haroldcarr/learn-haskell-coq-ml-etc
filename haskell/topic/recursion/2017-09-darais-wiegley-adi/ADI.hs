{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}

module ADI where

-- import Control.Monad
import           Control.Monad.Writer
-- import Data.Functor.Identity
import           Debug.Trace
import           Test.HUnit
import           Test.HUnit.Util as T

newtype Fix f = Fix { unFix :: f (Fix f) }

data ExprF r = Val Int | Add r r deriving (Eq, Foldable, Functor, Show, Traversable)
{-
instance Functor ExprF where
  fmap _ (Val a)     = Val a :: ExprF b
  fmap f (Add a1 a2) = Add (f a1) (f a2)
-}
type Expr = Fix ExprF

adi :: (Monoid b, Applicative s, Traversable t)
    => (t a -> a)
    -> ((Fix t -> (b, s a)) -> Fix t -> (b, s a))
    -> Fix t
    -> (b, s a)
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
    phi          (Val x)    = trace "\neval phi     Val"   x
    phi          (Add x y)  = trace "\neval phi     Add" $ x + y
    psi k v@(Fix (Val _))   = trace "\neval psi Fix Val" $ k v
    psi k v@(Fix (Add _ _)) = trace "\neval psi Fix Add" $ k v

ph :: ExprF Int -> Int
ph (Val x)   = x
ph (Add x y) = x + y
ps :: (Fix ExprF -> t) -> Fix ExprF -> t
ps k v@(Fix (Val _))   = trace "VAL" $ k v
ps k v@(Fix (Add _ _)) = trace "ADD" $ k v

e1, e2 :: [Test]
e1 = T.tt "e1"
     [ eval                                                           (Fix (Val 3))
     , adi ph ps                                                      (Fix (Val 3))
     , ps (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix) (Fix (Val 3))
     ,    (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix) (Fix (Val 3))
     ,    (fmap (fmap ph . sequenceA) . traverse (adi ph ps))              (Val 3)
     ,     fmap (fmap ph . sequenceA)  (traverse (adi ph ps)               (Val 3))
     ,     fmap (fmap ph . sequenceA)  (traverse (ps (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix))               (Val 3))
     ,     fmap (fmap ph . sequenceA)  (traverse (ps (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix))               (Val 3 :: ExprF (Fix ExprF)))
     --                                          _ Fix ExprF -> ((), Maybe Int)
     ,     fmap (fmap ph . sequenceA)  (traverse (ps (fmap (fmap ph . sequenceA) . traverse (adi ph ps) . unFix))               (Val 3 :: ExprF (Fix ExprF)))
     ,     fmap (fmap ph . sequenceA)           ((),                        Val 3)
     ,                                          ((), (fmap ph . sequenceA) (Val 3))
     ,                                          ((),  fmap ph   (Just      (Val 3)))
     ,                                          ((),             Just (ph  (Val 3)))
     ,                                          ((),             Just           3)
     ]
     ((), Just 3)

xx,xxx :: ((), ExprF Int)
xx  = traverse ( \(Fix (Val x)) -> ((), x))          (Val 3 :: ExprF (Fix ExprF))
xxx = traverse ((\     (Val x)  -> ((), x)) . unFix) (Val 3 :: ExprF (Fix ExprF))
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

evalM'2 :: (MonadWriter (Sum Int) m) => Expr -> m (Sum Int)
evalM'2 = cataM' phi psi
  where
    phi          (Val x)    = return (Sum x)
    phi          (Add x y)  = return (x + y)
    psi k v@(Fix (Val _))   = do { tell 3; k v }
    psi k v@(Fix (Add _ _)) = do { tell 4; k v }

em2 :: (MonadWriter (Sum Int) m) => m (Sum Int)
em2  = evalM'2 (Fix (Add (Fix (Val 1)) (Fix (Val 2))))

emt2 :: [Test]
emt2  = T.t "emt2"
        (runWriterT em2)
        [(Sum {getSum = 3},Sum {getSum = 10})]

------------------------------------------------------------------------------
{-
λ: fmap length (Val 3)
Val 3
λ: :t fmap length (Val 3)
fmap length (Val 3) :: ExprF Int
λ: :t fmap show (Val 3)
fmap show (Val 3) :: ExprF String
λ: :t fmap show (Add (Val 2) (Val 3))
fmap show (Add (Val 2) (Val 3)) :: ExprF String
λ: :t fmap id (Add (Val 2) (Val 3))
fmap id (Add (Val 2) (Val 3)) :: ExprF (ExprF r)
λ: :t fmap (+) (Add (Val 2) (Val 3))
fmap (+) (Add (Val 2) (Val 3))  :: Num (ExprF r) => ExprF (ExprF r -> ExprF r)
λ: fmap (Just) (Add (Val 2) (Val 3))
Add (Just (Val 2)) (Just (Val 3))
λ: fmap Just (Add (Val 2) (Add (Val 3) (Val 4)))
Add (Just (Val 2)) (Just (Add (Val 3) (Val 4)))
λ: fmap (show) (Val 3)
Val 3
λ: :t fmap (show) (Val 3)
fmap (show) (Val 3) :: ExprF String
λ: Add (Just (Val 2)) (Just (Add (Val 3) (Val 4)))   :: ExprF (Maybe (ExprF (ExprF Int)))
Add (Just (Val 2)) (Just (Add (Val 3) (Val 4)))
λ: Add (Just (Val 2)) (Just (Add (Val 3) (Val 4)))   :: ExprF (Maybe (ExprF (ExprF Char)))
Add (Just (Val 2)) (Just (Add (Val 3) (Val 4)))
λ: :t traverse
traverse
  :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
λ: traverse Just (Val 3)
Just (Val 3)
λ: traverse Just (Add (Val 2) (Add (Val 3) (Val 4)))
Just (Add (Val 2) (Add (Val 3) (Val 4)))
λ: :t traverse (+1) (Val 3)
traverse (+1) (Val 3) :: (Num (f b), Applicative f) => f (ExprF b)
λ: :t traverse (show) (Val 3)
traverse (show) (Val 3) :: [ExprF Char]
λ: traverse (show) (Val 3)
[Val 3]
λ: traverse (show) (Add (Val 2) (Val 3))
[Add 'V' 'V',Add 'V' 'a',Add 'V' 'l',Add 'V' ' ',Add 'V' '3',Add 'a' 'V',Add 'a' 'a',Add 'a' 'l',Add 'a' ' ',Add 'a' '3',Add 'l' 'V',Add 'l' 'a',Add 'l' 'l',Add 'l' ' ',Add 'l' '3',Add ' ' 'V',Add ' ' 'a',Add ' ' 'l',Add ' ' ' ',Add ' ' '3',Add '2' 'V',Add '2' 'a',Add '2' 'l',Add '2' ' ',Add '2' '3']
λ: :t traverse (show) (Add (Val 2) (Val 3))
traverse (show) (Add (Val 2) (Val 3)) :: [ExprF Char]
λ: :t [Val 3]
[Val 3] :: [ExprF r]
λ: [Val 3] :: [ExprF Char]
[Val 3]
λ: traverse ((:) 'x' . show) (Val 3)
[Val 3]
λ: :t traverse ((:) 'x' . show) (Val 34)
traverse ((:) 'x' . show) (Val 34) :: [ExprF Char]
λ: traverse ((:) 'x' . show) (Val 34)
[Val 34]
λ: head (traverse ((:) 'x' . show) (Val 34))
Val 34
λ: :t head (traverse ((:) 'x' . show) (Val 34))
head (traverse ((:) 'x' . show) (Val 34)) :: ExprF Char
λ: let (Val x) = head (traverse ((:) 'x' . show) (Val 34))
λ: x
34
λ: :t x
x :: Int
getVal (Val x) = x
λ: traverse (Just . Val . (+1) . getVal) (Val 3)
Just (Val 3)
λ: traverse (Just .   getVal) (Val 3)
Just (Val 3)
λ: traverse (Just . Val) (Val 3)
Just (Val 3)
λ: traverse (Just . (+1)) (Val 3)
Just (Val 3)
λ: traverse (Just . ('x':) . show) (Val 3)
Just (Val 3)
λ: traverse (Just . ('x':) . show) (Add (Val 3) (Val 4))
Just (Add "xVal 3" "xVal 4")
λ: traverse (('x':) . show) (Val 3)
[Val 3]
λ: traverse (('x':) . show) (Add (Val 3) (Val 4))
[Add 'x' 'x',Add 'x' 'V',Add 'x' 'a',Add 'x' 'l',Add 'x' ' ',Add 'x' '4',Add 'V' 'x',Add 'V' 'V',Add 'V' 'a',Add 'V' 'l',Add 'V' ' ',Add 'V' '4',Add 'a' 'x',Add 'a' 'V',Add 'a' 'a',Add 'a' 'l',Add 'a' ' ',Add 'a' '4',Add 'l' 'x',Add 'l' 'V',Add 'l' 'a',Add 'l' 'l',Add 'l' ' ',Add 'l' '4',Add ' ' 'x',Add ' ' 'V',Add ' ' 'a',Add ' ' 'l',Add ' ' ' ',Add ' ' '4',Add '3' 'x',Add '3' 'V',Add '3' 'a',Add '3' 'l',Add '3' ' ',Add '3' '4']
-}

------------------------------------------------------------------------------
xf1,xf2,xf3,xt1,xt2,xt3,xm1,xm2,xm3 :: [Test]

-- fmap :: Functor f => (a -> b) -> f a -> f b
xf1 = T.t "xf1"
      -- (a   -> b)        f a        f b
      -- (Int -> Float) -> [ Int ] -> [ Float ]
      (fmap      ((1.1 +) . fromInteger) [1,2,3])
      [2.1,3.1,4.1::Float]
xf2 = T.t "xf2"
      -- (a   -> b)        f a        f b
      --  f            b
      -- (ExprF Int -> Maybe (ExprF Int)
      --                   f      a
      --                   ExprF (ExprF Int)
      --                              f      b
      --                              ExprF (Maybe (ExprF Int))
      (fmap       (Just :: ExprF Int -> Maybe (ExprF Int))                  (Add (Val 3) (Val (4::Int))))
      (Add (Just (Val 3)) (Just (Val 4)) :: ExprF (Maybe (ExprF Int)))
xf3 = T.t "xf3"
      -- (a   -> b)        f a        f b
      -- a                   b
      -- ExprF (ExprF r0) -> Maybe (ExprF (ExprF Int))
      --                   f      a
      --                   ExprF (ExprF (ExprF Int))
      --                              f      b
      --                              ExprF (Maybe (ExprF (ExprF Int))))
      (fmap       Just                  (Add (Val 3) (Add (Val (4::Int)) (Val 5))))
      (Add (Just (Val 3)) (Just (Add (Val 4) (Val 5)))
                                         :: ExprF (Maybe (ExprF (ExprF Int))))

-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
xt1 = T.t "xt1"
      (traverse  (+1)                  [Sum 1, Sum 2, Sum (3::Int)])
      Sum {getSum = [2,3,4]}
xt2 = T.t "xt2"
      --  a      f   b       t a        f   t b
      -- (Int -> Sum Int) -> [ Int ] -> Sum [ Int ] ]
      (traverse ((+1) . Sum)           [1,2,3::Int])
      Sum {getSum = [2,3,4]}
xt3 = T.t "xt3"
      --  a      f b          t a        f t b
      -- (Int -> [ Char ]) -> [ Int ] -> [ [ Char ] ]
      (traverse  show                  [1,2,3::Int])
      ["123"]

-- (=<<) :: Monad m => (a -> m b) -> m a -> m b
xm1 = T.t "xm1"
      ((=<<)    (\(Sum x) -> [x + 1])  [1,2,3::Sum Int])
      [2,3,4]
xm2 = T.t "xm2"
      ((=<<)    (\(Sum x) -> [x + 1])  [Sum 1, Sum 2, Sum (3::Int)])
      [2,3,4]
xm3 = T.t "xm3"
      ((=<<)    (     \x  -> [x + 1])  [1,2,3::Int])
      [2,3,4]

------------------------------------------------------------------------------

seqq :: Monad m => [m a] -> m [a]
seqq = foldr mcons (return [])

mcons :: Monad m => m t -> m [t] -> m [t]
mcons x y = do
  x' <- x
  y' <- y
  return (x': y')

------------------------------------------------------------------------------

runTests :: IO Counts
runTests = runTestTT $ TestList $
  e1  ++ e2  ++ emt2 ++
  xf1 ++ xf2 ++ xf3 ++
  xt1 ++ xt2 ++ xt3 ++
  xm1 ++ xm2
