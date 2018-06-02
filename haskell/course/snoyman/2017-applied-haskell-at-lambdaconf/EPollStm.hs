#!/usr/bin/env stack
-- stack --resolver lts-8.12 script

import           Control.Applicative

-- ONE MUST BE PRIMITIVE

pollSTM :: Async a -> STM (Maybe (Either SomeException a))
pollSTM a = do
  (Just <$> waitCatchSTM a) <|> retry

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM a = do
    res <- pollSTM a
    case res of
      Nothing -> retry
      Just x  -> return x

waitSTM :: Async a -> STM a
waitSTM a = do
    res <- pollSTM a
    case res of
      Nothing -> retry
