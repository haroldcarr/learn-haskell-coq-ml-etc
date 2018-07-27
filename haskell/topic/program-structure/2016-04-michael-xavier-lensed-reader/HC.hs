{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module HC where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader

data    Config   = Config   { _username :: String
                            , _password :: String
                            }
newtype Other    = Other    { _dbname :: String }
data    AppState = AppState { _asConfig :: Config
                            , _asOther  :: Other
                            , _asLogger :: String -> IO ()
                            }
makeClassy ''Config
makeClassy ''Other
makeLenses ''AppState

instance HasConfig AppState where
  config = asConfig

instance HasOther AppState where
  other = asOther

-- lenses compose in opposite direction than functions
getUsername :: (MonadReader r m, HasConfig r) => m String
getUsername  = view (config . username)
getPassword :: (MonadReader r m, HasConfig r) => m String
getPassword  = view (config . password)
getDBName   :: (MonadReader r m, HasOther  r) => m String
getDBName    = view (other  . dbname)
getLogger   :: (MonadReader AppState m) => m (String -> IO ())
getLogger    = ask >>= \(AppState _ _ l) -> return l

newtype AppT m a = AppT { unAppT :: ReaderT AppState m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppState)

runAppT :: AppState -> AppT m a -> m a
runAppT s m = runReaderT (unAppT m) s

main :: IO ()
main = do
  r <- runAppT (AppState (Config "HC" "FCW")
                         (Other "other")
                         putStrLn)
               top
  putStrLn r

top :: AppT IO String
top = do
  lr <- configOnly
  liftIO (putStrLn lr)
  hr <- fullAccessButCannotDoIO
  liftIO (putStrLn hr)
  rhr <- launchMissles
  liftIO (putStrLn rhr)
  AppState x _y _z <- ask
  return (_password x)

configOnly :: (MonadReader r m, HasConfig r) => m String
configOnly = do
  u <- getUsername
  p <- getPassword
  -- d <- getDBName -- Could not deduce (HasOther r) arising from a use of ‘getDBName’
  return ("configOnly: " ++ u ++ " " ++ p)

fullAccessButCannotDoIO :: (MonadReader AppState m) => m String
fullAccessButCannotDoIO = do
  pw <- getPassword
  db <- getDBName
  _l <- getLogger -- can access, but can't use
  -- _l "X" --     Couldn't match type ‘m’ with ‘IO’
  return ("fullAccessButCannotDoIO: " ++ pw ++ " " ++ db)

launchMissles :: (MonadIO m, MonadReader AppState m) => m String
launchMissles = do
  pw <- getPassword
  db <- getDBName
  l  <- getLogger          -- can access, even without MonadIO
  let msg = "launchMissles: " ++ pw ++ " " ++ db
  liftIO (l msg)          -- can NOT call unless MonadIO included
  return msg

