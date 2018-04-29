{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}

module Crypto where

import qualified "cryptonite" Crypto.Hash as H
import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PK
import qualified Crypto.Random.Types      as RT
import qualified Data.Either.Combinators  as E
import qualified Data.Serialize           as S
import           Universum

------------------------------------------------------------------------------
newtype Msg       = Msg       { getMsg :: ByteString } deriving Show
newtype Signature = Signature { getSig :: ByteString } deriving (Generic, Show)
instance S.Serialize Signature

generatePKSKIO
  :: RT.MonadRandom m
  => m (RSA.PublicKey, RSA.PrivateKey)
generatePKSKIO = RSA.generate 200 0x10001

-- TODO : tests do not work when using this one
generatePKSK
  :: Either RSA.Error (RSA.PublicKey, RSA.PrivateKey)
generatePKSK = E.maybeToRight RSA.InvalidParameters
               (RSA.generateWith (4,5) 200 0x10001) -- TODO Error

signMsgIO
  :: RT.MonadRandom m
  => RSA.PrivateKey
  -> Msg
  -> m (Either RSA.Error Signature)
signMsgIO p m =
  fmap (E.mapRight Signature)
       (PK.signSafer (Just H.SHA512) p (getMsg m))

signMsg
  :: RSA.PrivateKey
  -> Msg
  -> Either RSA.Error Signature
signMsg p m = E.mapRight Signature (PK.sign Nothing (Just H.SHA512) p (getMsg m))

verifyMsgSig
  :: RSA.PublicKey
  -> Msg
  -> Signature
  -> Bool
verifyMsgSig p m s = PK.verify (Just H.SHA512) p (getMsg m) (getSig s)

------------------------------------------------------------------------------

doitIO :: RT.MonadRandom m
     => Msg
     -> m Bool
doitIO msg = do
  (pk,sk) <- generatePKSKIO
  (Right sig) <- signMsgIO sk msg
  return $ verifyMsgSig pk msg sig

doit :: Msg -> Either RSA.Error Bool
doit msg = do
  (pk,sk) <- generatePKSK
  sig <- signMsg sk msg
  return $ verifyMsgSig pk msg sig

top :: RT.MonadRandom m => m Bool
top = doitIO (Msg "this is a message")
