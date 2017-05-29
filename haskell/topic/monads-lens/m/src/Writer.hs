{-# LANGUAGE FlexibleContexts #-}

module Writer where

import Control.Monad.Writer

logNumber :: (MonadWriter [String] m, Show a) => a -> m a
logNumber x = writer (x, ["number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  _ <- logNumber 1
  a <- logNumber 3
  b <- logNumber 5
  _ <- logNumber 9
  return (a*b)

-- runWriter multWithLog
