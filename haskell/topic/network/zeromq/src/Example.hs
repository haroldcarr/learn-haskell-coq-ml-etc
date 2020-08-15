{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Example where

------------------------------------------------------------------------------
import           Lib
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
  let a = Addr "tcp://127.0.0.1:10000"
      b = Addr "tcp://127.0.0.1:10001"
  (at, ainr, aobw) <- setup a
  (bt, binr, bobw) <- setup b
  runMsgServer (at :: TransportEnv SignedRPC)
  runMsgServer (bt :: TransportEnv SignedRPC)
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
      U.writeChan c (OutBoundMsg (ROne to) (S.encode (SignedRPC i)))
  recvMsgs mvc = do
    {-
    m <- getMsgSync mvc
    print (["receive", "decoded", show m]::[Text])
    -}
    ms <- tryGetMsgs mvc 2000
    for_ ms $ \m@(SignedRPC i) ->
      -- print (["receive", "decoded", show m]::[Text])
      when (i == limit) $ do
        print (["receive", "decoded", show m]::[Text])
        exitSuccess
    threadDelay 10000
    recvMsgs mvc
