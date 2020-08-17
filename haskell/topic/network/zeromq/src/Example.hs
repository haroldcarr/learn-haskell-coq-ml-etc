{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

------------------------------------------------------------------------------
import           Init
import           Lib
import           NoBlockChan
import           Types
------------------------------------------------------------------------------
import qualified Control.Concurrent.Async      as Async
import qualified Control.Concurrent.Chan.Unagi as U
import qualified Data.Serialize                as S
import           Protolude                     hiding (to)
------------------------------------------------------------------------------

limit :: Int
limit  = 10000

newtype SignedRPC = SignedRPC Int deriving (Eq, Generic, Show)
instance S.Serialize SignedRPC

main :: IO ()
main  = do
  let a      = "tcp://127.0.0.1:10000"
      b      = "tcp://127.0.0.1:10001"
      le _ _ = pure ()
      li _ _ = pure ()
  (at, ainr, aobw) <- initialize a le li
  (bt, binr, bobw) <- initialize b le li
  runMsgServer (at :: TransportEnv Address)
  runMsgServer (bt :: TransportEnv Address)
  Async.concurrently_
    (sendMsgs b aobw)
    (Async.concurrently_
      (sendMsgs a bobw)
      (Async.concurrently_
        (recvMsgs ainr)
        (recvMsgs binr)))
 where
  sendMsgs to c =
    forM_ [1::Int .. limit] $ \i ->
      U.writeChan c (OutBoundMsg [to] (S.encode (SignedRPC i)))
  recvMsgs mvc = do
    ms <- tryGetMsgs mvc 2000
    for_ ms $ \m -> case S.decode m of
      Left err ->
        print ["failed S.decode"::Text, show m, show err]
      Right rpc@(SignedRPC i) ->
        when (i == limit) $ do
          print (["receive", "decoded", show rpc]::[Text])
          exitSuccess
    threadDelay 10000
    recvMsgs mvc
