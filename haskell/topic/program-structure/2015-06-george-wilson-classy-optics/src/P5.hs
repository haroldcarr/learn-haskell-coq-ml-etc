{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module P5 where

import           Data.Monoid          ((<>))
import           Control.Lens
-- import           Control.Monad.IO.Class
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           P4

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- 35:38

type MyData = T.Text

loadFromDb :: (MonadError e m, MonadReader r m,
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
    return ("MyData returned from 'loadFromDb' " <> c <> " " <> s)

sendOverNet :: (MonadError     e m, MonadReader      r m,
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
    return $ "sendOverNet: " <> x <> " " <> T.pack (show p) <> " " <> s

-- this would not compile at the end of P1
loadAndSend :: (MonadError     e m, MonadReader      r m,
                AsDbError      e,   HasDbConfig      r,
                AsNetworkError e,   HasNetworkConfig r,
                Monad m)
             => m T.Text
loadAndSend = loadFromDb >>= sendOverNet

-- 39:00

newtype App m a =
    App { unApp :: ReaderT AppConfig (ExceptT AppError m) a }
    deriving (Applicative, Functor, Monad,
              MonadReader AppConfig,
              MonadError  AppError)

-- HC

app :: App IO T.Text
app = do
  r1 <- loadAndSend
  r2 <- loadAndSend
  return $ r1 <> " |||| " <> r2

runApp :: DbConfig -> NetworkConfig -> IO ()
runApp dbc nc = do
  r <- runExceptT $
         runReaderT (unApp app)
                    (AppConfig dbc nc)
  T.putStrLn (T.pack $ show r)

dbcGood = DbConfig "DbConfig conn" "DbConfig sche"
dbcBad  = DbConfig "BAD"           "DbConfig Schema for BAD connection"
ncGood  = NetConfig 45             "NetConfig ssl"
ncBad   = NetConfig (-1)           "NetConfig ssl for -1"

m1,m2,m3 :: IO ()
m1 = runApp dbcGood ncGood
m2 = runApp dbcBad  ncGood
m3 = runApp dbcGood ncBad

------------------------------------------------------------------------------

appW :: App (Writer [T.Text]) T.Text
appW = do
  r1 <- loadAndSend
  r2 <- loadAndSend
  return $ r1 <> " |||| " <> r2

-- runApp :: DbConfig -> NetworkConfig -> IO ()
runAppW dbc nc =
  runIdentity $
    runWriterT $
      runExceptT $
        runReaderT (unApp appW)
                   (AppConfig dbc nc)


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
