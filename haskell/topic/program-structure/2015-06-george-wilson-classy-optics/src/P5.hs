{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module P5 where

import           Data.Monoid          ((<>))
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           P0
import           P4

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- 35:38

loadFromDbIO
  :: (MonadError e m, MonadReader r m,
      AsDbError  e,   HasDbConfig r,
      MonadIO m)
  => m MyData
loadFromDbIO = do
  dbc <- ask
  let c = view dbConn   dbc
      s = view dbSchema dbc
  if c == "BAD" then
    throwError $ review _QueryError s
  else do
    liftIO $ T.putStr ("loadFromDb " <> c <> " " <> s <> "> ")
    liftIO   T.getLine

loadFromDb
  :: (MonadError e m, MonadReader r m,
      AsDbError  e,   HasDbConfig r,
      Monad m)
  => m MyData
loadFromDb = do
  dbc <- ask
  let c = view dbConn   dbc
      s = view dbSchema dbc
  if c == "BAD" then
    throwError $ review _QueryError s
  else
    return ("loadFromDb " <> c <> " " <> s <> " : 'HCData'")

sendOverNetIO
  :: (MonadError     e m, MonadReader      r m,
      AsNetworkError e,   HasNetworkConfig r,
      MonadIO m)
  => MyData
  -> m ()
sendOverNetIO x = do
  nc <- ask
  let p = view port nc
      s = view ssl  nc
  if p == (-1) then
    throwError $ review _Timeout s
  else
    liftIO $ T.putStrLn ("sendOverNet: " <> x <> " " <> T.pack (show p) <> " " <> s)

sendOverNet
  :: (MonadError     e m, MonadReader      r m,
      AsNetworkError e,   HasNetworkConfig r,
      Monad m)
  => MyData
  -> m T.Text
sendOverNet x = do
  nc <- ask
  let p = view port nc
      s = view ssl  nc
  if p == (-1) then
    throwError $ review _Timeout s
  else
    return ("sendOverNet " <> T.pack (show p) <> " " <> s <> " : '" <> x <> "'")

-- this would not compile at the end of P1
loadAndSendIO
  :: (MonadError     e m, MonadReader      r m,
      AsNetworkError e,   HasNetworkConfig r,
      AsDbError      e,   HasDbConfig      r,
      MonadIO m)
  => m ()
loadAndSendIO = loadFromDbIO >>= sendOverNetIO

-- this would not compile at the end of P1
loadAndSend
  :: (MonadError     e m, MonadReader      r m,
      AsDbError      e,   HasDbConfig      r,
      AsNetworkError e,   HasNetworkConfig r,
      Monad m)
  => m T.Text
loadAndSend = loadFromDb >>= sendOverNet

-- 39:00

newtype AppIO a =
    AppIO { unAppIO :: ReaderT AppConfig (ExceptT AppError IO) a }
    deriving (Applicative, Functor, Monad, MonadIO,
              MonadReader AppConfig,
              MonadError  AppError)
-- 39:00

newtype App m a =
    App   { unApp   :: ReaderT AppConfig (ExceptT AppError m)  a }
    deriving (Applicative, Functor, Monad, MonadIO, -- remove MonadIO?
              MonadReader AppConfig,
              MonadError  AppError)

------------------------------------------------------------------------------
-- HC

appIO :: AppIO ()
appIO = do
  loadAndSendIO
  loadAndSendIO

app :: App IO ()
app = do
  loadAndSendIO
  loadAndSendIO

appW :: App (Writer [T.Text]) T.Text
appW = do
  r1 <- loadAndSend
  r2 <- loadAndSend
  return $ r1 <> " |||| " <> r2

runAppIO :: DbConfig -> NetworkConfig -> IO ()
runAppIO dbc nc = do
  r <- runExceptT $
         runReaderT (unAppIO appIO)
                    (AppConfig dbc nc)
  T.putStrLn (T.pack $ show r)

runApp   :: DbConfig -> NetworkConfig -> IO ()
runApp dbc nc = do
  r <- runExceptT $
         runReaderT (unApp   app)  -- only difference
                    (AppConfig dbc nc)
  T.putStrLn (T.pack $ show r)

runAppW  :: DbConfig -> NetworkConfig -> (Either AppError T.Text, [T.Text])
runAppW dbc nc =
  runIdentity $
    runWriterT $
      runExceptT $
        runReaderT (unApp appW)
                   (AppConfig dbc nc)

m1io,m2io,m3io :: IO ()
m1io = runAppIO dbcGood ncGood
m2io = runAppIO dbcBad  ncGood
m3io = runAppIO dbcGood ncBad

m1,m2,m3 :: IO ()
m1 = runApp dbcGood ncGood
m2 = runApp dbcBad  ncGood
m3 = runApp dbcGood ncBad

-- m1w,m2w,m3w :: IO ()
m1w = runAppW dbcGood ncGood
m2w = runAppW dbcBad  ncGood
m3w = runAppW dbcGood ncBad

dbcGood = DbConfig "conn" "sche"
dbcBad  = DbConfig "BAD"  "sche for BAD"
ncGood  = NetConfig 45    "xssl"
ncBad   = NetConfig (-1)  "xssl for -1"

------------------------------------------------------------------------------
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
