{-# LANGUAGE FlexibleContexts #-}

module UseWriter where

import Control.Monad.Writer
import Prelude hiding (log)
import Test.HUnit as T
import Test.HUnit.Util as U

------------------------------------------------------------------------------

log     :: (MonadWriter [String]  m, Show a) => a -> m a
lSum    :: (MonadWriter (Sum Int) m, Num  a) => a -> m a
lInt    :: (MonadWriter [a]       m, Num  a) => a -> m a
log  x   = writer (x, ["number: " ++ show x])
lSum x   = writer (x, 40) -- writer :: (a, Sum Int) -> m a
lInt x   = writer (x, [x])

useLog  :: Writer [String]  Int
useLSum :: Writer (Sum Int) Int
useLInt :: Writer [Int]     Int
useLog   = do _ <- log  1; a <- log  3; b <- log  5; _ <- log  9; tell ["1"]; return (a*b)
useLSum  = do a <- lSum 1; b <- lSum 2; c <- lSum 3; _ <- lSum 0; tell   1  ; return (a+b+c)
useLInt  = do a <- lInt 1; b <- lInt 2; c <- lInt 3; _ <- lInt 0; tell  [1] ; return (a+b+c)

tul = U.t "tul" (runWriter useLog)  (15, ["number: 1","number: 3","number: 5","number: 9","1"])
tus = U.t "tus" (runWriter useLSum) ( 6, Sum {getSum = 161})
tui = U.t "tui" (runWriter useLInt) ( 6, [1,2,3,0,1])

testWriter = runTestTT $ TestList $ tul ++ tus ++ tui
tw = testWriter

------------------------------------------------------------------------------

-- https://stackoverflow.com/questions/29096003/how-do-pass-and-listen-work-in-writert

-- https://stackoverflow.com/questions/34832072/what-is-the-point-of-pass-and-listen-in-writer-monad

------------------------------------------------------------------------------

