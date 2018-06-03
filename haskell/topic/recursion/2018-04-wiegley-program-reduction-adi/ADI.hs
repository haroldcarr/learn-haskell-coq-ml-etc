{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module ADI where

import           Debug.Trace

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}
{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

newtype Fix f = Fix { unFix :: f (Fix f) }

cata,cata'
  :: Functor f
  =>       (f a ->   a)                  -- f
  ->                       Fix f -> a
adi,adi',adi''
  :: Functor f
  =>       (f a ->   a)                  -- f
  -> ((Fix f    ->   a) -> Fix f -> a)   -- g
  ->                       Fix f -> a
cata  f     =            f . fmap (cata f)   . unFix
cata' f x   =            f  (fmap (cata f)    (unFix       x))
adi   f g   = g         (f . fmap (adi  f g) . unFix)
adi'  f g x = g ( \z -> (f . fmap (adi  f g) . unFix) z)   x
adi'' f g x = g ( \z ->  f  (fmap (adi  f g)  (unFix  z))) x

data Atom = AInt Int | ABool Bool deriving Show

data ExprF r
  = Const Atom
  | Add r r
  | If r r r
  deriving (Show, Functor)

type Expr = Fix ExprF

evaluate :: Expr -> Int
evaluate = cata $ \case
  Const (AInt  n) -> n
  Const (ABool b) -> if b then 1 else 0
  Add   l r       -> l + r
  If    c t e     -> if c /= 0 then t else e

iff :: Expr
iff = Fix (If (Fix (Const (AInt 0)))
              (Fix (Const (AInt 1)))
              (Fix (Const (AInt 2))))

eiff :: Int
eiff = evaluate iff

-- | psi runs first.  It is given a continuation `k`.
--   Whatever it returns to `k` is the given to phi.
eval :: Expr -> Int
eval  = adi phi psi
  where
    phi          (Const (AInt  n))  = trace "\nNInt" $ 100*n
    phi          (Const (ABool b))  = if b then 1 else 0
    phi          (Add l r)          = trace "\nNAdd" $ l + r
    phi          (If  c t e)        = if c /= 0 then t else e
    psi k   (Fix (Const (AInt  n))) = trace "\nFIX NInt" $ k (Fix (Const (AInt (n-3))))
    psi k v@(Fix (Const (ABool _))) = k v
    psi k   (Fix (Add l _))         = trace "\nFIX NAdd" $ k (Fix (Const (AInt (psi k l))))
    psi k v@(Fix (If  _ _ _))       = k v

eadi1,eadi2 :: Int
eadi1 = eval (Fix (Const (AInt 0)))
eadi2 = eval (Fix (Add (Fix (Const (AInt 1)))
                       (Fix (Const (AInt 3)))))

