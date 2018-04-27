{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module S where

import Crypto.Hash as H
import Crypto.PubKey.RSA as RSA
import Crypto.PubKey.RSA.PKCS15 as PK
import Crypto.Random.Types as RT
import Universum

top :: RT.MonadRandom m => m Bool
top = doit H.SHA512

doit :: (HashAlgorithmASN1 hashAlg, RT.MonadRandom m)
     => hashAlg
     -> m Bool
doit ha = do
  (pk,sk) <- RSA.generate 200 0x10001
  (Right sig) <- PK.signSafer (Just ha) sk "foo"
  return $ PK.verify (Just ha) pk "foo" sig




