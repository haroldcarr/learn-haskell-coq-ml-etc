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

data NAtom = NInt Int | NBool Bool deriving Show

data NExprF r
  = NConstant NAtom
  | NAdd r r
  | NIf r r r
  deriving (Show, Functor)

type NExpr = Fix NExprF

evaluate :: NExpr -> Int
evaluate = cata $ \case
  NConstant (NInt  n) -> n
  NConstant (NBool b) -> if b then 1 else 0
  NAdd l r            -> l + r
  NIf  c t e          -> if c /= 0 then t else e

iff :: NExpr
iff = Fix (NIf (Fix (NConstant (NInt 0)))
               (Fix (NConstant (NInt 1)))
               (Fix (NConstant (NInt 2))))

eiff :: Int
eiff = evaluate iff

eval :: NExpr -> Int
eval  = adi phi psi
  where
    phi          (NConstant (NInt  n))  = trace "\nNInt" $ 100*n
    phi          (NConstant (NBool b))  = if b then 1 else 0
    phi          (NAdd l r)             = trace "\nNAdd" $ l + r
    phi          (NIf  c t e)           = if c /= 0 then t else e
    psi k   (Fix (NConstant (NInt  n))) = trace "\nFIX NInt" $ k (Fix (NConstant (NInt (n-3))))
    psi k v@(Fix (NConstant (NBool _))) = k v
    psi k   (Fix (NAdd l _))            = trace "\nFIX NAdd" $ k (Fix (NConstant (NInt (psi k l))))
    psi k v@(Fix (NIf  _ _ _))          = k v

eadi1,eadi2 :: Int
eadi1 = eval (Fix (NConstant (NInt 0)))
eadi2 = eval (Fix (NAdd (Fix (NConstant (NInt 1)))
                        (Fix (NConstant (NInt 3)))))

