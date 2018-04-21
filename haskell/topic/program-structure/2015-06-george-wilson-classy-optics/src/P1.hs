{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module P1 where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text            as T
import           Data.Text.IO         as T
import           P0

data DbConfig = DbConfig
  { dbConn :: DbConnection
  , schema :: Schema
  }

data NetworkConfig = NetConfig
  { port :: Port
  , ssl  :: SSL
  }

data AppConfig = AppConfig
  { appDbConfig  :: DbConfig
  , appNetConfig :: NetworkConfig
  }

data DbError
  = QueryError T.Text
  | InvalidConnection

data NetworkError
  = Timeout Int
  | ServerOnFire

data AppError
  = AbbDbError  DbError
  | AppNetError NetworkError

-- 3:44

newtype App a =
    App { unApp :: ReaderT AppConfig (ExceptT AppError IO) a }
      deriving (Applicative, Functor, Monad, MonadIO,
                MonadReader AppConfig,
                MonadError AppError)
-- 6:09

getPort :: MonadReader NetworkConfig m => m Port
getPort = reader port -- 'reader' is synonym for 'asks'

getPort' :: MonadReader NetworkConfig m => m Port
getPort' = do
    cfg <- ask
    return (port cfg)

printM :: MonadIO m => Text -> m ()
printM = liftIO . T.putStrLn

-- 9:53

type Err = Text

mightFail :: MonadError Err m => m Int
mightFail = undefined

couldFail :: MonadError Err m => m Text
couldFail = undefined

maybeFail :: MonadError Err m => m (Maybe (Int, Text))
maybeFail = ( do a <- mightFail
                 b <- couldFail
                 pure (Just (a, b))
            ) `catchError` (\_ -> pure Nothing)

-- 10:06

{-
-- instance MonadReader AppConfig App
ask :: App AppConfig

-- instance MonadError AppError App
throwError :: AppError -> App a
catchError :: App a -> (AppError -> App a) -> App q

-- instance MonadIO App
liftIO :: IO a -> App a
-}

-- 11:00

-- No type-safety in following:

loadFromDb :: App MyData
loadFromDb = undefined

sendOverNet :: MyData -> App ()
sendOverNet = undefined

loadAndSend :: App ()
loadAndSend = loadFromDb >>= sendOverNet

-- 12:50

-- this version provide more type-safety : can't touch NetworkConfig or throw Network Errors
loadFromDb' :: (MonadReader DbConfig m,
                MonadError  DbError  m,
                MonadIO              m)
               => m MyData
loadFromDb' = undefined

-- 13:29

sendOverNet' :: (MonadReader NetworkConfig m,
                 MonadError  NetworkError  m,
                 MonadIO                   m)
                => MyData -> m ()
sendOverNet' = undefined

-- but this causes a type error:

-- loadAndSend' = loadFromDb' >>= sendOverNet'
