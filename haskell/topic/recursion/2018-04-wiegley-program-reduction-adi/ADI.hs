{-# LANGUAGE LambdaCase #-}

module ADI where

import           Debug.Trace
import           ExprF
import           Fix
import           RS

{-# ANN module ("HLint: ignore Avoid lambda"        :: String) #-}
{-# ANN module ("HLint: ignore Eta reduce"          :: String) #-}
{-# ANN module ("HLint: ignore Redundant $"         :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}
{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

{-
cata,cata'
  :: Functor f
  =>        (f a ->   a)                  -- f
  ->                        Fix f -> a
-}
adi,adi',adi''
  :: Functor f
  =>        (f a ->   a)                  -- f
  ->   ((Fix f   ->   a) -> Fix f -> a)   -- g
  ->                        Fix f -> a
{-
cata  f     =            f . fmap (cata f)   . unFix
cata' f x   =            f  (fmap (cata f)    (unFix       x))
-}
adi   f g   = g         (f . fmap (adi  f g) . unFix)
adi'  f g x = g ( \z -> (f . fmap (adi  f g) . unFix) z)   x
adi'' f g x = g ( \z ->  f  (fmap (adi  f g)  (unFix  z))) x

evalCata :: Expr -> Int
evalCata = cata phi
  where
    phi = \case
      Const (AInt  n) -> n
      Const (ABool b) -> if b then 1 else 0
      Add   l r       -> l + r
      If    c t e     -> if c /= 0 then t else e

-- | psi
--   - Given continuation `k` and values before recursion.
--   - Whatever it returns to `k` is the given to phi.
--   phi
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
iff = Fix (If (Fix (Const (ABool False)))
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

evalAdiBeforeAfter :: Expr -> Int
evalAdiBeforeAfter  = adi phi psi
  where
    phi          (Const (AInt  n))  = trace  "\nNInt"            $ n
    phi          (Const (ABool b))  = trace  "\nBool"            $ if b then 1 else 0
    phi          (Add l r)          = trace  "\nNAdd"            $ l + r
    phi          (If  c t e)        = trace  "\nIf"              $ if c /= 0 then t else e
    psi k   (Fix (Const (AInt  n))) = trace  "\nFix Int"         $ k (Fix (Const (AInt (n))))
    psi k v@(Fix (Const (ABool _))) = trace  "\nFix Bool Before" $ case k v of
                                x ->  trace ("\nFix Bool After" ++ show x)
                                                                 $ x
    psi k   (Fix (Add l _))         = trace "\nFix Add"          $ k (Fix (Const (AInt (psi k l))))
    psi k v@(Fix (If  _ _ _))       = trace "\nFix If"           $ k v

