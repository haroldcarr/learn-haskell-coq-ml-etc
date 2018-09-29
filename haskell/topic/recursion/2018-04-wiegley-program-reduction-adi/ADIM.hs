{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module ADIM where

import           Control.Monad
import           Control.Monad.State
import           Debug.Trace
import           Lens.Micro
import           Lens.Micro.TH
------------------------------------------------------------------------------
import           ADI   (iff, add)
import           ExprF
import           Fix

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

adiM
  :: (Traversable t, Monad m)
  =>        (t a -> m a)
  ->   ((Fix t   -> m a) -> Fix t -> m a)
  ->                        Fix t -> m a
adiM f g = g ((f <=< traverse (adiM f g)) . unFix)

data Count = Count
  { _numConst :: Int
  , _numAdd   :: Int
  , _numIf    :: Int
  } deriving Show
makeLenses ''Count

evalAdiM :: Expr -> State Count Int
evalAdiM  = adiM phi psi
  where
    phi          (Const (AInt  n))  = do modify (over numConst (+1)); trace "\nInt"  $ return   n
    phi          (Const (ABool b))  = do modify (over numConst (+1)); trace "\nBool" $ return $ if b then 1 else 0
    phi          (Add   l r)        = do modify (over numAdd   (+1)); trace "\nAdd"  $ return $ l + r
    phi          (If    c t e)      = do modify (over numIf    (+1)); trace "\nIf"   $ return $ if c /= 0 then t else e
    psi k v@(Fix (Const (AInt  _))) =    trace "\nFix Int"  $ k v
    psi k v@(Fix (Const (ABool _))) =    trace "\nFix Bool" $ k v
    psi k   (Fix (Add   l _))       = do n <- psi k l;
                                         trace "\nFix Add"  $ k (Fix (Const (AInt n)))
    psi k v@(Fix (If    _ _ _))     =    trace "\nFix If"   $ k v

eif :: (Int, Count)
eif = runState (evalAdiM iff) (Count 0 0 0)
ead :: (Int, Count)
ead = runState (evalAdiM add) (Count 0 0 0)
