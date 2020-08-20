{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Example where

------------------------------------------------------------------------------
import           Init
import           NoBlockChan
import           Types
import           ZMQ
------------------------------------------------------------------------------
import qualified Control.Concurrent.Chan.Unagi as U
import qualified Data.Serialize                as S
import           Protolude                     hiding (to)
------------------------------------------------------------------------------

newtype RPC = RPC Int deriving (Eq, Generic, Show)
instance S.Serialize RPC

main :: forall chanType. Channel chanType => Proxy chanType -> Int -> IO ()
main chanType limit = do
  let a          = "tcp://127.0.0.1:10000"
      b          = "tcp://127.0.0.1:10001"
      le _ _     = pure ()
      li _ _     = pure ()
      {-
      le adr m     = print (T.pack adr:m)
      li adr m     = print (T.pack adr:m)
      -}
  (at, ainr, aobw) <- initialize chanType a le li
  (bt, binr, bobw) <- initialize chanType b le li
  runMsgServer chanType at
  runMsgServer chanType bt
  void $ runConcurrently $ (,,,)
    <$> Concurrently (sendMsgs b aobw)
    <*> Concurrently (sendMsgs a bobw)
    <*> Concurrently (recvMsgs ainr)
    <*> Concurrently (recvMsgs binr)
 where
  sendMsgs to c =
    forM_ [1::Int .. limit] $ \i ->
      U.writeChan c (OutBoundMsg [to] (S.encode (RPC i)))

  recvMsgs inr = do
    ms <- readC chanType inr 2000
    for_ ms doDecode
    recvMsgs inr

  doDecode m = case S.decode m of
    Left err ->
      print ["failed S.decode"::Text, show m, show err]
    Right rpc@(RPC i) ->
      when (i == limit) $ do
        print (["receive", "decoded", show rpc]::[Text])
        exitSuccess
