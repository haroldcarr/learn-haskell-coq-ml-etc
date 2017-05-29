{-# LANGUAGE FlexibleContexts #-}

module Writer where

import Control.Monad.Writer
import Prelude hiding (log)

log :: (MonadWriter [String] m, Show a) => a -> m a
log x = writer (x, ["number: " ++ show x])

useLog :: Writer [String] Int
useLog = do
  _ <- log 1
  a <- log 3
  b <- log 5
  _ <- log 9
  return (a*b)

-- runWriter useLog
