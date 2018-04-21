{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module P4 where

import           Data.Monoid          ((<>))
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           P0

data DbConfig = DbConfig
    { _dbConn   :: DbConnection
    , _dbSchema :: Schema
    }

data DbError
    = QueryError T.Text
    | InvalidConnection
    deriving Show

data NetworkConfig = NetConfig
    { _port :: Port
    , _ssl  :: SSL
    }

data NetworkError
    = Timeout T.Text -- Int
    | ServerOnFire
    deriving Show

data AppConfig = AppConfig
    { _appDbConfig  :: DbConfig
    , _appNetConfig :: NetworkConfig
    }

data AppError
    = AppDbError  { _dbError  :: DbError }
    | AppNetError { _netError :: NetworkError }
    deriving Show

-- 34:50

makeClassy       ''DbConfig
makeClassy       ''NetworkConfig
makeClassy       ''AppConfig
makeClassyPrisms ''DbError
makeClassyPrisms ''NetworkError
makeClassyPrisms ''AppError

instance HasDbConfig AppConfig where
  dbConfig = appDbConfig
instance HasNetworkConfig AppConfig where
  networkConfig = appNetConfig
instance AsDbError AppError where
  _DbError = _AppError . _AppDbError
instance AsNetworkError AppError where
  _NetworkError = _AppError . _AppNetError

-- 35:38

type MyData = T.Text

loadFromDb :: (MonadError e m, MonadReader r m,
               AsDbError  e,   HasDbConfig r,
               MonadIO m)
            => m MyData
loadFromDb = do
  dbc <- ask
  let c = view dbConn   dbc
      s = view dbSchema dbc
  if c == "BAD" then
    throwError $ review _QueryError s
  else
    return ("MyData returned from 'loadFromDb' " <> c <> " " <> s)

sendOverNet :: (MonadError     e m, MonadReader      r m,
                AsNetworkError e,   HasNetworkConfig r,
                MonadIO m)
             => MyData -> m ()
sendOverNet x = do
  nc <- ask
  let p = view port nc
      s = view ssl  nc
  if p == (-1) then
    throwError $ review _Timeout s
  else
    liftIO $ T.putStrLn ("sendOverNet: " <> x <> " " <> T.pack (show p) <> " " <> s)

-- this would not compile at the end of P1
loadAndSend :: (MonadError     e m, MonadReader      r m,
                AsNetworkError e,   HasNetworkConfig r,
                AsDbError      e,   HasDbConfig      r,
                MonadIO m)
             => m ()
loadAndSend = loadFromDb >>= sendOverNet

-- 39:00

newtype App a =
    App { unApp :: ReaderT AppConfig (ExceptT AppError IO) a }
    deriving (Applicative, Functor, Monad, MonadIO,
              MonadReader AppConfig,
              MonadError  AppError)

-- HC

mainApp :: DbConfig -> NetworkConfig -> IO ()
mainApp dbc nc = do
  r <- runExceptT $
         runReaderT (P4.unApp loadAndSend)
                    (AppConfig dbc nc)
  T.putStrLn (T.pack $ show r)

m1 :: IO ()
m1 = mainApp (DbConfig "DbConfig conn" "DbConfig sche")
             (NetConfig 45 "NetConfig ssl")

m2 :: IO ()
m2 = mainApp (DbConfig "BAD" "DbConfig Schema for BAD connection")
             (NetConfig 45 "NetConfig ssl")

m3 :: IO ()
m3 = mainApp (DbConfig "DbConfig conn" "DbConfig sche")
             (NetConfig (-1) "NetConfig ssl for -1")

-- 39:45

-- Abstractions > Concretions
-- Typeclass constraints stack up better than monolithic transformers
-- Lens gives compositional vocabulary for talking about data

-- talked about
-- - http://hackage.haskell.org/package/mtl
-- - http://lens.github.io

-- encourage looking at
-- - http://github.com/benkolera/talk-stacking-your-monads/
-- - http://hackage.haskell.org/package/hoist-error

-- makeClassy / makeClassyPrisms
-- - https://hackage.haskell.org/package/lens-4.13.2/docs/Control-Lens-TH.html
