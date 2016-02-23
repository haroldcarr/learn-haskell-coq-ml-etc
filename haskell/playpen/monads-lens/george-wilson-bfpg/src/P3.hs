{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module P3 where

import           Control.Applicative
import           Control.Lens         hiding (Lens, Prism, prism)
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Monad.Writer
import qualified Data.Text            as T
import           P0

type Lens  a b = Lens'  a b
type Prism a b = Prism' a b
prism = prism'

-- 25:00

data DbConfig =
     DbConfig {
         _dbConn   :: DbConnection
       , _dbSchema :: Schema
      }

class HasDbConfig t where
    dbConfig :: Lens t DbConfig
    dbConn   :: Lens t DbConnection
    dbSchema :: Lens t Schema
    dbConn   =  dbConfig . dbConn
    dbSchema =  dbConfig . dbSchema

instance HasDbConfig DbConfig where
    dbConfig = id
    dbConn   = lens _dbConn   (\d c -> d { _dbConn   = c})
    dbSchema = lens _dbSchema (\d s -> d { _dbSchema = s})

data NetworkConfig =
     NetConfig {
         _port :: Port
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

data AppConfig =
     AppConfig {
         appDbConfig  :: DbConfig
       , appNetConfig :: NetworkConfig
     }

instance HasDbConfig AppConfig where
    dbConfig = lens appDbConfig (\app db -> app { appDbConfig = db})

instance HasNetworkConfig AppConfig where
    netConfig = lens appNetConfig (\app net -> app { appNetConfig = net})

data DbError =
     QueryError T.Text
   | InvalidConnection

class AsDbError t where
    _DbError     :: Prism t DbError
    _QueryError  :: Prism t T.Text
    _InvalidConn :: Prism t ()
    _QueryError  =  _DbError . _QueryError
    _InvalidConn =  _DbError . _InvalidConn

instance AsDbError DbError where
    _DbError    = id
    _QueryError = prism QueryError
        $ \case QueryError t -> Just t
                _            -> Nothing
    _InvalidConn = prism (const InvalidConnection)
        $ \case InvalidConnection -> Just ()
                _                 -> Nothing
