{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module S where

import Crypto.Hash as H
import Crypto.PubKey.RSA as RSA
import Crypto.PubKey.RSA.PKCS15 as PK
import Crypto.Random.Types as RT
import Universum

generatePKSK
  :: RT.MonadRandom m
  => m (RSA.PublicKey, RSA.PrivateKey)
generatePKSK = RSA.generate 200 0x10001

signMsg
  :: RT.MonadRandom m
  => RSA.PrivateKey
  -> ByteString
  -> m (Either Error ByteString)
signMsg = PK.signSafer (Just H.SHA512)

verifyMsg
  :: PublicKey
  -> ByteString -- ^ message
  -> ByteString -- ^ signature
  -> Bool
verifyMsg = PK.verify (Just H.SHA512)

doit :: RT.MonadRandom m
     => ByteString
     -> m Bool
doit msg = do
  (pk,sk) <- generatePKSK
  (Right sig) <- signMsg sk msg
  return $ verifyMsg pk msg sig

top :: RT.MonadRandom m => m Bool
top = doit "this is a message"





