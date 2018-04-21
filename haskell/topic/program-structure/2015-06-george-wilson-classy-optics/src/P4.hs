{-# LANGUAGE TemplateHaskell            #-}

module P4 where

import           Control.Lens
import qualified Data.Text            as T
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

