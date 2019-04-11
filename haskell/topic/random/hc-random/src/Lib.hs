{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib where

import System.Random
import Control.Monad.State


g1      = mkStdGen 1
(r1,g2) = randomR (1::Int, 6::Int) g1
(r2,g3) = randomR (1::Int, 6::Int) g2
(r3,g4) = randomR (1::Int, 6::Int) g3

newtype MyState = MyState
  { stdgen :: StdGen }

class RRandom s where
  rRandom' :: s -> StdGen
instance RRandom MyState where
  rRandom' = stdgen
class WRandom s where
  wRandom' :: StdGen -> s -> s
instance WRandom MyState where
  wRandom' x s = s { stdgen = x }

type RWRandom s = (RRandom s, WRandom s)

wRandom :: (RWRandom s, MonadState s m) => forall b . Random b => (b, b) -> m b
wRandom range = do
  g <- gets rRandom'
  let (r,g') = randomR range g
  s <- get
  put (wRandom' g' s)
  return r

tryit :: (RWRandom s, MonadState s m) => m (Int, Int, Int)
tryit  = do
  r'   <- wRandom (1,6)
  r''  <- wRandom (1,6)
  r''' <- wRandom (1,6)
  return (r',r'',r''')

tt :: (Int, Int, Int)
tt  = evalState tryit (MyState (mkStdGen 1))
