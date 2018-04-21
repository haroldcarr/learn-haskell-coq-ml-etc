{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

module P3 where

import           Control.Lens         hiding (Lens, Prism, prism)
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           P0

type Lens  a b = Lens'  a b
type Prism a b = Prism' a b

prism = prism'

-- 25:00

-- for each type

data DbConfig = DbConfig
    { _dbConn   :: DbConnection
    , _dbSchema :: Schema
    }

-- associate a typeclass of optics for that type

class HasDbConfig t where
    dbConfig :: Lens t DbConfig
    dbConn   :: Lens t DbConnection
    dbSchema :: Lens t Schema
    dbConn   =  dbConfig . dbConn -- note: lens compose "in reverse"
    dbSchema =  dbConfig . dbSchema

instance HasDbConfig DbConfig where
    dbConfig = id
    dbConn   = lens _dbConn   (\d c -> d { _dbConn   = c})
    dbSchema = lens _dbSchema (\d s -> d { _dbSchema = s})

data NetworkConfig = NetConfig
    { _port :: Port
    , _ssl  :: SSL
    }

class HasNetworkConfig t where
    netConfig :: Lens t NetworkConfig
    netPort   :: Lens t Port
    netSSL    :: Lens t SSL
    netPort   =  netConfig . netPort
    netSSL    =  netConfig . netSSL

instance HasNetworkConfig NetworkConfig where
    netConfig = id
    netPort   = lens _port (\n p -> n { _port = p})
    netSSL    = lens _ssl  (\n s -> n { _ssl  = s})

data AppConfig = AppConfig
    { appDbConfig  :: DbConfig
    , appNetConfig :: NetworkConfig
    }

instance HasDbConfig AppConfig where
    dbConfig = lens appDbConfig (\app db -> app { appDbConfig = db})

instance HasNetworkConfig AppConfig where
    netConfig = lens appNetConfig (\app net -> app { appNetConfig = net})

-- 32:04

data DbError
    = QueryError T.Text
    | InvalidConnection
    deriving Show

class AsDbError t where
    _DbError     :: Prism t DbError
    _QueryError  :: Prism t T.Text
    _InvalidConn :: Prism t ()
    _QueryError  =  _DbError . _QueryError
    _InvalidConn =  _DbError . _InvalidConn

instance AsDbError DbError where
    _DbError     = id
    _QueryError  = prism QueryError
        $ \case QueryError t -> Just t
                _            -> Nothing
    _InvalidConn = prism (const InvalidConnection)
        $ \case InvalidConnection -> Just ()
                _                 -> Nothing

-- 32:04

data NetworkError
    = Timeout Int
    | ServerOnFire
    deriving Show

class AsNetworkError t where
    _NetworkError :: Prism t NetworkError
    _Timeout      :: Prism t Int
    _ServerOnFire :: Prism t ()
    _Timeout      =  _NetworkError . _Timeout
    _ServerOnFire =  _NetworkError . _ServerOnFire

instance AsNetworkError NetworkError where
    _NetworkError = id
    _Timeout      = prism Timeout
        $ \case Timeout t -> Just t
                _         -> Nothing
    _ServerOnFire = prism (const ServerOnFire)
        $ \case ServerOnFire -> Just ()
                _            -> Nothing

-- 34:00

data AppError
    = AppDbError  { dbError  :: DbError }
    | AppNetError { netError :: NetworkError }
    deriving Show

instance AsDbError AppError where
    _DbError     = prism AppDbError
        $ \case AppDbError dbe -> Just dbe
                _              -> Nothing

instance AsNetworkError AppError where
    _NetworkError = prism AppNetError
        $ \case AppNetError ne -> Just ne
                _              -> Nothing

-- 34:50

-- above boilerplate can be generated automatically:

-- makeClassyPrisms ''NetworkError  -- prism
-- makeClassy       ''DbConig       -- lens

-- 35:38

type MyData = T.Text

loadFromDb :: (MonadError e m, MonadReader r m,
               AsDbError  e,   HasDbConfig r,
               MonadIO m)
            => m MyData
loadFromDb = undefined

sendOverNet :: (MonadError     e m, MonadReader      r m,
                AsNetworkError e,   HasNetworkConfig r,
                MonadIO m)
             => MyData -> m ()
sendOverNet = undefined

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

mainApp :: App ()
mainApp = loadAndSend

-- HC

main :: IO ()
main = do
  r <- runExceptT $
         runReaderT (P3.unApp mainApp)
                    (AppConfig (DbConfig "conn" "sche") (NetConfig 1 "ssl"))
  T.putStrLn (T.pack $ show r)

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
