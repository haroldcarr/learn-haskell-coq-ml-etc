module MyTMVar where

import Control.Concurrent.STM hiding (TMVar)

newtype TMVar a = TMVar (TVar (Maybe a))

newTMVar :: a -> STM (TMVar a)
newTMVar a = do
  t <- newTVar (Just a)
  return $ TMVar t

newTMVarIO :: a -> IO (TMVar a)
newTMVarIO = fmap TMVar . newTVarIO . Just

takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar tv) = do
  x <- readTVar tv
  case x of
    Nothing -> retry
    Just a  -> do
      writeTVar tv Nothing
      return a

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar tv) = do
  x <- readTVar tv
  case x of
    Nothing -> return Nothing
    Just a  -> do
      writeTVar tv Nothing
      return (Just a)

