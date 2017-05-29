{-# LANGUAGE FlexibleContexts #-}

module Writer where

import Control.Monad.Writer
import Prelude hiding (log)
import Test.HUnit as T
import Test.HUnit.Util as U

------------------------------------------------------------------------------

log :: (MonadWriter [String] m, Show a) => a -> m a
log x = writer (x, ["number: " ++ show x])

useLog :: Writer [String] Int
useLog = do
  _ <- log 1
  a <- log 3
  b <- log 5
  _ <- log 9
  return (a*b)

tul = U.t "tul"
      (runWriter useLog)
      (15, ["number: 1","number: 3","number: 5","number: 9"])

------------------------------------------------------------------------------

lSum :: (MonadWriter (Sum Int) m, Num a) => a -> m a
lSum x = writer (x, 40)

useLSum :: Writer (Sum Int) Int
useLSum = do
  a <- lSum 1
  b <- lSum 2
  c <- lSum 3
  _ <- lSum 0
  return (a+b+c)

tus = U.t "tus"
      (runWriter useLSum)
      (6,Sum {getSum = 160})

------------------------------------------------------------------------------

lInt :: (MonadWriter [a] m, Num a) => a -> m a
lInt x = writer (x, [x])

useLInt :: Writer [Int] Int
useLInt = do
  a <- lInt 1
  b <- lInt 2
  c <- lInt 3
  _ <- lInt 0
  return (a+b+c)

tui = U.t "tui"
      (runWriter useLInt)
      (6,[1,2,3,0])

------------------------------------------------------------------------------

-- https://stackoverflow.com/questions/29096003/how-do-pass-and-listen-work-in-writert

-- https://stackoverflow.com/questions/34832072/what-is-the-point-of-pass-and-listen-in-writer-monad

------------------------------------------------------------------------------

testWriter =
  runTestTT $ TestList $ tul ++ tus ++ tui
