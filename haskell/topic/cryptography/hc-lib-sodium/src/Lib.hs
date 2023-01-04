{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import qualified Crypto.Sodium.Hash
import qualified Crypto.Sodium.Init
import qualified Crypto.Sodium.Key
import qualified Crypto.Sodium.Mac
import qualified Crypto.Sodium.Nonce
import qualified Crypto.Sodium.Encrypt.Public
import qualified Crypto.Sodium.Encrypt.Symmetric
-- import qualified Crypto.Sodium.Encrypt.Symmetric.Stream
import qualified Crypto.Sodium.Random
import qualified Crypto.Sodium.Sign
import qualified Data.ByteArray
import qualified Data.ByteArray.Sized
import qualified Data.ByteString

------------------------------------------------------------------------------

libInit :: IO ()
libInit  = Crypto.Sodium.Init.sodiumInit

dbab :: Data.ByteArray.Bytes
dbab  = Data.ByteArray.pack [0 .. 87]

dbasba :: Data.ByteArray.Sized.SizedByteArray 32 Data.ByteArray.Bytes
dbasba  = Data.ByteArray.Sized.unsafeSizedByteArray (Data.ByteArray.pack [0 .. 31])

--------------------------------------------------
-- Crypto.Sodium.Encrypt.Public

keypairSK :: IO ( Crypto.Sodium.Encrypt.Public.PublicKey Data.ByteString.ByteString
              , Crypto.Sodium.Encrypt.Public.SecretKey Data.ByteArray.ScrubbedBytes )
keypairSK  = Crypto.Sodium.Encrypt.Public.keypair

deEncrypted :: IO ( Crypto.Sodium.Encrypt.Public.Nonce Data.ByteString.ByteString
                  , Data.ByteString.ByteString
                  , Maybe Data.ByteString.ByteString )
deEncrypted  = do
  (pk, sk) <- keypairSK
  case Crypto.Sodium.Encrypt.Public.toNonce ("012345678901234567890123" :: Data.ByteString.ByteString) of
    Nothing -> error "HC: deEncrypted toNonce"
    Just nonce -> do
      let encrypted :: Data.ByteString.ByteString
            = Crypto.Sodium.Encrypt.Public.encrypt pk sk nonce
              ("message to be encrypted for privacy privacy privacy privacy privacy" :: Data.ByteString.ByteString)
      -- NOTE: if sk/pk reversed, compiler does not complain, but decrypt returns Nothing
      let decrypted = Crypto.Sodium.Encrypt.Public.decrypt sk pk nonce encrypted
      pure (nonce, encrypted, decrypted)

--------------------------------------------------
-- Crypto.Sodium.Encrypt.Symmetric

keyS :: Data.ByteString.ByteString -> Maybe (Crypto.Sodium.Encrypt.Symmetric.Key Data.ByteString.ByteString)
keyS  = Crypto.Sodium.Encrypt.Symmetric.toKey

deEncryptedS :: IO ( Crypto.Sodium.Encrypt.Symmetric.Key Data.ByteString.ByteString
                   , Crypto.Sodium.Encrypt.Public.Nonce Data.ByteString.ByteString
                   , Data.ByteString.ByteString
                   , Maybe Data.ByteString.ByteString )
deEncryptedS  = do
  case keyS "01234567890123456789012345678901" of
    Nothing -> error "HC: deEncryptedS keyS"
    Just k  ->
      case Crypto.Sodium.Encrypt.Symmetric.toNonce ("012345678901234567890123" :: Data.ByteString.ByteString) of
        Nothing -> error "HC: deEncryptedS toNonce"
        Just nonce -> do
          let encrypted :: Data.ByteString.ByteString
                = Crypto.Sodium.Encrypt.Symmetric.encrypt k nonce
                  ("message to be encrypted for privacy privacy privacy privacy privacy" :: Data.ByteString.ByteString)
          let decrypted = Crypto.Sodium.Encrypt.Symmetric.decrypt k nonce encrypted
          pure (k, nonce, encrypted, decrypted)

--------------------------------------------------
-- Crypto.Sodium.Encrypt.Symmetric.Stream

-- TODO

------------------------------------------------------------------------------
-- Crypto.Sodium.Hash

hash_blake2b256 :: Data.ByteArray.Bytes -> Crypto.Sodium.Hash.HashBlake2b 32 Data.ByteArray.Bytes
hash_blake2b256  = Crypto.Sodium.Hash.blake2b @32

dbab_blake2b256 :: Crypto.Sodium.Hash.HashBlake2b 32 Data.ByteArray.Bytes
dbab_blake2b256  = hash_blake2b256 dbab

hash_blake2b512 :: Data.ByteArray.Bytes -> Crypto.Sodium.Hash.HashBlake2b 64 Data.ByteArray.Bytes
hash_blake2b512  = Crypto.Sodium.Hash.blake2b @64

dbab_blake2b512 :: Crypto.Sodium.Hash.HashBlake2b 64 Data.ByteArray.Bytes
dbab_blake2b512  = hash_blake2b512 dbab

hash_sha256 :: Data.ByteArray.Bytes -> Crypto.Sodium.Hash.HashSha256 Data.ByteArray.Bytes
hash_sha256  = Crypto.Sodium.Hash.sha256

dbab_sha256 :: Crypto.Sodium.Hash.HashSha256 Data.ByteArray.Bytes
dbab_sha256  = hash_sha256 dbab

hash_sha512 :: Data.ByteArray.Bytes -> Crypto.Sodium.Hash.HashSha512 Data.ByteArray.Bytes
hash_sha512  = Crypto.Sodium.Hash.sha512

dbab_sha512 :: Crypto.Sodium.Hash.HashSha512 Data.ByteArray.Bytes
dbab_sha512  = hash_sha512 dbab

------------------------------------------------------------------------------
-- Crypto.Sodium.Key

password :: Data.ByteString.ByteString
password  = "passwordpasswordpasswordpasswordpasswordpasswordpasswordpasswordpasswordpasswordpasswordpasswordpasswordpassword"

params :: Crypto.Sodium.Key.Params
params  = Crypto.Sodium.Key.Params 1024 1024

dks :: IO ( Data.ByteArray.Sized.SizedByteArray 64 Data.ByteString.ByteString
         , Crypto.Sodium.Key.DerivationSlip )
dks = Crypto.Sodium.Key.derive
          @(Data.ByteArray.Sized.SizedByteArray 64 Data.ByteString.ByteString)
         (Crypto.Sodium.Key.Params 1024 (2 * 1024 * 1024))
         password

 >>= \case
  Nothing -> error "HC:dks:Nothing"
  Just r -> pure r

rd :: IO Bool
rd = do
  (key, slip) <- dks
  case Crypto.Sodium.Key.rederive slip password of
    Nothing -> error "HC:rd:Nothing"
    Just k  -> pure (k == key)

ggg ::IO (Data.ByteArray.Sized.SizedByteArray 64 Data.ByteArray.ScrubbedBytes)
ggg = Crypto.Sodium.Key.generate

------------------------------------------------------------------------------
-- Crypto.Sodium.Mac

authenticator :: Crypto.Sodium.Mac.Authenticator Data.ByteArray.Bytes
--                                       key    msg
authenticator = Crypto.Sodium.Mac.create dbasba dbab

isValid :: Bool
--                                  key    msg
isValid  = Crypto.Sodium.Mac.verify dbasba dbab authenticator

------------------------------------------------------------------------------
-- Crypto.Sodium.Nonce

genN :: IO (Data.ByteArray.Sized.SizedByteArray 32 Data.ByteString.ByteString)
genN  = Crypto.Sodium.Nonce.generate

------------------------------------------------------------------------------
-- Crypto.Sodium.Random

genR :: IO (Data.ByteArray.Sized.SizedByteArray 32 Data.ByteArray.Bytes)
genR  = Crypto.Sodium.Random.generate

------------------------------------------------------------------------------
-- Crypto.Sodium.Sign

keypairSign :: IO ( Crypto.Sodium.Sign.PublicKey Data.ByteString.ByteString
                  , Crypto.Sodium.Sign.SecretKey Data.ByteArray.ScrubbedBytes )
keypairSign  = Crypto.Sodium.Sign.keypair

deEncryptedSign :: IO ( Data.ByteString.ByteString
                      , Maybe Data.ByteString.ByteString )
deEncryptedSign  = do
  (pk, sk) <- keypairSign
  let signed :: Data.ByteString.ByteString
        = Crypto.Sodium.Sign.create sk
            ("message to be signed" :: Data.ByteString.ByteString)
  let verified = Crypto.Sodium.Sign.open pk signed
  pure (signed, verified)
