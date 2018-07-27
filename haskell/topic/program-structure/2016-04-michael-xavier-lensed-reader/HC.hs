{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module HC where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Prelude hiding (log)

{-# ANN module "HLint: ignore Reduce duplication" #-}

data    Config   = Config   { _username :: String
                            , _password :: String
                            }
newtype Other    = Other    { _dbname :: String }
data    Logger   = Logger   { _level  :: MVar Int
                            , _log    :: Int -> String -> IO ()
                            }
data    App      = App      { _asConfig :: Config
                            , _asOther  :: Other
                            , _asLogger :: Logger
                            }
makeClassy ''Config
makeClassy ''Other
makeClassy ''Logger
makeLenses ''App
instance HasConfig App where config = asConfig
instance HasOther  App where other  = asOther
instance HasLogger App where logger = asLogger
-- lenses compose in opposite direction than functions
getUsername :: (MonadReader r m, HasConfig r) => m String
getUsername  = view (config . username)
getPassword :: (MonadReader r m, HasConfig r) => m String
getPassword  = view (config . password)
getDBName   :: (MonadReader r m, HasOther  r) => m String
getDBName    = view (other  . dbname)
getLogLevel :: (MonadReader r m, HasLogger r) => m (MVar Int)
getLogLevel  = view (logger . level)
getLogger   :: (MonadReader r m, HasLogger r) => m (Int -> String -> IO ())
getLogger    = view (logger . log)

newtype AppT m a = AppT { unAppT :: ReaderT App m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader App)

runAppT :: App -> AppT m a -> m a
runAppT app m = runReaderT (unAppT m) app

newtype AppState = AppState { num :: Int }
newtype AppTWithState m a = AppTWithState { unAppTWithState :: RWST App () AppState m a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader App)

runAppTWithState :: App -> AppState -> AppTWithState m a -> m (a, AppState, ())
runAppTWithState app appState m = runRWST (unAppTWithState m) app appState

main :: IO ()
main = do
  mv <- newMVar 1
  r <- runAppT (App (Config "HC" "FCW")
                    (Other "other")
                    (Logger mv (const putStrLn)))
               top
  putStrLn r

main2 :: IO ()
main2 = do
  mv <- newMVar 1
  (r, AppState n, _)
           <- runAppTWithState
                  (App (Config "HC" "FCW")
                       (Other "other")
                       (Logger mv (const putStrLn)))
                  (AppState 0)
                  top2
  putStrLn r
  print n

top :: AppT IO String
top = do
  lr <- configOnly
  liftIO (putStrLn lr)
  hr <- fullAccessButCannotDoIO
  liftIO (putStrLn hr)
  rhr <- launchMissles
  liftIO (putStrLn rhr)
  App x _y _z <- ask
  return (_password x)

top2 :: AppTWithState IO String
top2 = do
  -- update          --   *************
  lr <- configOnly
  liftIO (putStrLn lr)
  hr <- fullAccessButCannotDoIO
  liftIO (putStrLn hr)
  rhr <- launchMissles
  liftIO (putStrLn rhr)
  App x _y _z <- ask
  return (_password x)

update :: (MonadState AppState m) => m ()
update = do
  (AppState n) <- get
  put (AppState (n + 1))
  n' <- gets num
  put (AppState (n' + 1))

configOnly :: (MonadReader r m, HasConfig r) => m String
configOnly = do
  u <- getUsername
  p <- getPassword
  -- d <- getDBName -- Could not deduce (HasOther r) arising from a use of ‘getDBName’
  return ("configOnly: " ++ u ++ " " ++ p)

fullAccessButCannotDoIO :: (MonadReader App m) => m String
fullAccessButCannotDoIO = do
  pw <- getPassword
  db <- getDBName
  _l <- getLogger -- can access, but can't use
  -- _l 0 "X" --     Couldn't match type ‘m’ with ‘IO’
  return ("fullAccessButCannotDoIO: " ++ pw ++ " " ++ db)

launchMissles :: (MonadIO m, MonadReader App m) => m String
launchMissles = do
  pw <- getPassword
  db <- getDBName
  let msg = "launchMissles: " ++ pw ++ " " ++ db
  loggit msg
  return msg

loggit :: (MonadIO m, MonadReader r m, HasLogger r) => String -> m ()
loggit msg = do
  l  <- getLogger          -- can access, even without MonadIO
  ll <- getLogLevel
  liftIO (readMVar ll >>= \lev -> l lev msg)
