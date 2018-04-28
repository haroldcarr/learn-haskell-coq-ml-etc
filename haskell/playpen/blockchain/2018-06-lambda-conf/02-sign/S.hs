{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module S where

import qualified Crypto.Hash              as H
import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PK
import qualified Crypto.Random.Types      as RT
import qualified Data.Either.Combinators  as E
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Serialize           as S
import qualified Data.UUID                as U
import qualified Data.UUID.V4             as U
import           Universum

------------------------------------------------------------------------------
newtype Msg       = Msg       { message   :: ByteString } deriving Show
newtype Signature = Signature { signature :: ByteString } deriving (Generic, Show)
instance S.Serialize Signature

generatePKSKIO
  :: RT.MonadRandom m
  => m (RSA.PublicKey, RSA.PrivateKey)
generatePKSKIO = RSA.generate 200 0x10001

generatePKSK
  :: Maybe (RSA.PublicKey, RSA.PrivateKey)
generatePKSK = RSA.generateWith (4,5) 200 0x10001

signMsgIO
  :: RT.MonadRandom m
  => RSA.PrivateKey
  -> Msg
  -> m (Either RSA.Error Signature)
signMsgIO p m =
  fmap (E.mapRight Signature)
       (PK.signSafer (Just H.SHA512) p (message m))

signMsg
  :: RSA.PrivateKey
  -> Msg
  -> Either RSA.Error Signature
signMsg p m =
  E.mapRight Signature (PK.sign Nothing (Just H.SHA512) p (message m))

verifyMsg
  :: RSA.PublicKey
  -> Msg
  -> Signature
  -> Bool
verifyMsg p m s = PK.verify (Just H.SHA512) p (message m) (signature s)

doitIO :: RT.MonadRandom m
     => Msg
     -> m Bool
doitIO msg = do
  (pk,sk) <- generatePKSKIO
  (Right sig) <- signMsgIO sk msg
  return $ verifyMsg pk msg sig

doit :: Msg -> Either RSA.Error Bool
doit msg = do
  (pk,sk) <- E.maybeToRight RSA.InvalidParameters generatePKSK -- TODO Error
  sig <- signMsg sk msg
  return $ verifyMsg pk msg sig

top :: RT.MonadRandom m => m Bool
top = doitIO (Msg "this is a message")

------------------------------------------------------------------------------

newtype UUID    = UUID    { uuid    :: ByteString } deriving (Generic, Show)
newtype PKHash  = PKHash  { pkHash  :: ByteString } deriving (Generic, Show)
newtype HashPtr = HashPtr { hashPtr :: ByteString } deriving (Generic, Show)
instance S.Serialize UUID
instance S.Serialize PKHash
instance S.Serialize HashPtr

data TX
  = CreateCoin UUID
  | TransferCoin PKHash HashPtr
  deriving (Generic, Show)
instance S.Serialize TX

data SignedTX = SignedTX
  { sTX  :: TX
  , sSig :: Signature
  } deriving (Generic, Show)
instance S.Serialize SignedTX

createCoinIO :: RSA.PrivateKey -> IO (Either RSA.Error SignedTX)
createCoinIO sk = do
  u <- U.nextRandom
  let cc = CreateCoin (UUID (BSL.toStrict (U.toByteString u)))
  fmap (E.mapRight (SignedTX cc)) (signMsgIO sk (Msg (S.encode cc)))

createCoin :: UUID -> RSA.PrivateKey -> Either RSA.Error SignedTX
createCoin u sk = do
  let cc = CreateCoin u
  E.mapRight (SignedTX cc) (signMsg sk (Msg (S.encode cc)))

hash :: BS.ByteString -> H.Digest H.SHA256
hash = H.hashWith H.SHA256


