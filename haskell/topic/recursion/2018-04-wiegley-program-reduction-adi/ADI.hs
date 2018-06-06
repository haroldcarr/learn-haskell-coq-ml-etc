{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module ADI where

import Control.Monad.Reader
import           Debug.Trace

{-# ANN module ("HLint: ignore Avoid lambda"        :: String) #-}
{-# ANN module ("HLint: ignore Eta reduce"          :: String) #-}
{-# ANN module ("HLint: ignore Redundant $"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

newtype Fix f = Fix { unFix :: f (Fix f) }

cata,cata'
  :: Functor f
  =>        (f a ->   a)                  -- f
  ->                        Fix f -> a
adi,adi',adi''
  :: Functor f
  =>        (f a ->   a)                  -- f
  ->   ((Fix f   ->   a) -> Fix f -> a)   -- g
  ->                        Fix f -> a
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

evalCata :: Expr -> Int
evalCata = cata $ \case
  Const (AInt  n) -> n
  Const (ABool b) -> if b then 1 else 0
  Add   l r       -> l + r
  If    c t e     -> if c /= 0 then t else e

-- | psi
--   - Runs first.
--   - It is given a continuation `k` and values before recursion.
--   - Whatever it returns to `k` is the given to phi.
--   psi
--   - Runs second.
--   - It is given fully evaluated arguments.
evalAdi :: Expr -> Int
evalAdi  = adi phi psi
  where
    phi          (Const (AInt  n))  = trace "\nNInt"     $ n
    phi          (Const (ABool b))  = trace "\nBool"     $ if b then 1 else 0
    phi          (Add l r)          = trace "\nNAdd"     $ l + r
    phi          (If  c t e)        = trace "\nIf"       $ if c /= 0 then t else e
    psi k   (Fix (Const (AInt  n))) = trace "\nFix Int"  $ k (Fix (Const (AInt (n))))
    psi k v@(Fix (Const (ABool _))) = trace "\nFix Bool" $ k v
    psi k   (Fix (Add l _))         = trace "\nFix Add"  $ k (Fix (Const (AInt (psi k l))))
    psi k v@(Fix (If  _ _ _))       = trace "\nFix If"   $ k v

iff :: Expr
iff = Fix (If (Fix (Const (AInt 0)))
              (Fix (Const (AInt 1)))
              (Fix (Const (AInt 2))))

add :: Expr
add = Fix (Add (Fix (Const (AInt 10)))
               (Fix (Const (AInt 30))))

eciff,eaiff,eci,eai,ecadd,eaadd :: Int
eciff = evalCata iff
eaiff = evalAdi  iff
eci   = evalCata (Fix (Const (AInt 0)))
eai   = evalAdi  (Fix (Const (AInt 0)))
ecadd = evalCata add
eaadd = evalAdi  add

evalAdiR :: Expr -> Reader Int Int
evalAdiR  = adi phi psi
  where
    phi          (Const (AInt  n))  = trace "\nNInt"     $ n
    phi          (Const (ABool b))  = trace "\nBool"     $ if b then 1 else 0
    phi          (Add l r)          = trace "\nNAdd"     $ l + r
    phi          (If  c t e)        = trace "\nIf"       $ if c /= 0 then t else e
    psi k   (Fix (Const (AInt  n))) = trace "\nFix Int"  $ k (Fix (Const (AInt (n))))
    psi k v@(Fix (Const (ABool _))) = trace "\nFix Bool" $ k v
    psi k   (Fix (Add l _))         = trace "\nFix Add"  $ k (Fix (Const (AInt (psi k l))))
    psi k v@(Fix (If  _ _ _))       = trace "\nFix If"   $ k v
