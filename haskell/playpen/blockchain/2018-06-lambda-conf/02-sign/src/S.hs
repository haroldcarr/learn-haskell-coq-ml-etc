{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module S where

import qualified Crypto.Hash              as H
import qualified Crypto.PubKey.RSA        as RSA
import qualified Crypto.PubKey.RSA.PKCS15 as PK
import qualified Crypto.Random.Types      as RT
import qualified Data.Either.Combinators  as E
import qualified Data.ByteArray           as BA
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Map.Strict          as M
import qualified Data.Serialize           as S
import qualified Data.UUID                as U
import qualified Data.UUID.V4             as U
import qualified Prelude
import           Test.Hspec               as HS
import           Universum

{-# ANN module ("HLint: ignore Unused LANGUAGE pragma"::Prelude.String) #-}

------------------------------------------------------------------------------
newtype Msg       = Msg       { getMsg :: ByteString } deriving Show
newtype Signature = Signature { getSig :: ByteString } deriving (Generic, Show)
instance S.Serialize Signature

generatePKSKIO
  :: RT.MonadRandom m
  => m (RSA.PublicKey, RSA.PrivateKey)
generatePKSKIO = RSA.generate 200 0x10001

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
signMsg p m =
  let r = E.mapRight Signature (PK.sign Nothing (Just H.SHA512) p (getMsg m))
  in ("signMsg: msg: " ++ show m ++ "; sig: " ++ show r) `trace` r

verifyMsgSig
  :: RSA.PublicKey
  -> Msg
  -> Signature
  -> Bool
verifyMsgSig p m s =
  PK.verify (Just H.SHA512) p (getMsg m)
    ((\x -> ("verifyMsgSig: msg:" ++ show m ++ "; sig: " ++ show x) `trace` x)
     (getSig s))

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

------------------------------------------------------------------------------

newtype UUID   = UUID   { getUuid   :: ByteString } deriving (Generic, Show)
newtype TXHash = TXHash { getTXHash :: ByteString } deriving (Eq, Generic, Ord, Show)
newtype Hash   = Hash   { getHash   :: ByteString } deriving (Generic, Show)
instance S.Serialize UUID
instance S.Serialize TXHash
instance S.Serialize Hash
deriving instance Generic     RSA.PublicKey
deriving instance S.Serialize RSA.PublicKey

hash :: BS.ByteString -> Hash
hash = Hash . BA.convert . H.hashWith H.SHA256

data TX
  = CreateCoin UUID
  | TransferCoin TXHash RSA.PublicKey
  deriving (Generic, Show)
instance S.Serialize TX

data SignedTX = SignedTX
  { sTX  :: TX
  , sSig :: Signature
  } deriving (Generic, Show)
instance S.Serialize SignedTX

signTX :: RSA.PrivateKey -> TX -> Either RSA.Error SignedTX
signTX sk tx = E.mapRight (SignedTX tx) (signMsg sk (Msg (S.encode tx)))

verifyTXSig :: RSA.PublicKey -> SignedTX -> Bool
verifyTXSig pk stx = case stx of
  (SignedTX tx@(CreateCoin _)     sig) -> v tx sig
  (SignedTX tx@(TransferCoin _ _) sig) -> v tx sig
 where v tx = verifyMsgSig pk (Msg (S.encode tx))

createUUID :: IO UUID
createUUID = do
  u <- U.nextRandom
  return (UUID (BSL.toStrict (U.toByteString u)))

createCoinIO :: RSA.PrivateKey -> IO (Either RSA.Error SignedTX)
createCoinIO sk = do
  u <- createUUID
  return (createCoin u sk)

createCoin :: UUID -> RSA.PrivateKey -> Either RSA.Error SignedTX
createCoin u sk = signTX sk (CreateCoin u)

transferCoin :: SignedTX -> RSA.PrivateKey -> RSA.PublicKey -> Either RSA.Error SignedTX
transferCoin fromCoin ownerSK toPK = do
  let fromHash = hash (S.encode fromCoin)
      toCoin   = TransferCoin (TXHash (getHash fromHash)) toPK
  signTX ownerSK toCoin

testIt = do
  u <- runIO createUUID
  let Right (creatorPK, creatorSK) = generatePKSK
  {-
  let Right (alicePK  , aliceSK)   = generatePKSK
  let Right (bobPK    , _bobSK)    = generatePKSK
  let Right (jimPK    , _jimSK)    = generatePKSK
  -}
  let Right cc   = createCoin   u    creatorSK
  {-
  let Right cToA = transferCoin cc   creatorSK alicePK
  let Right aToB = transferCoin cToA aliceSK   bobPK
  let Right aToJ = transferCoin cToA aliceSK   jimPK
  let chain1 = addToChain M.empty [cc{-, cToA-}]
  let chain2 = addToChain M.empty [cc, cToA]
  -}
  describe "GoofyCoin" $
    it "isValidCoin createCoin" $
      isValidCoin creatorPK M.empty cc `shouldBe` Right True
    {-
    it "isValidCoin chain1 aToJ" $
      isValidCoin creatorPK chain1 aToJ `shouldBe` Right True
    it "isValidCoin chain1 aToJ (double spend)" $
      isValidCoin creatorPK chain2 aToB `shouldBe` Right True
    -}
 where
  addToChain :: M.Map TXHash SignedTX -> [SignedTX] -> M.Map TXHash SignedTX
  addToChain = foldr (\x cc -> M.insert (TXHash (getHash (hash (S.encode x)))) x cc)
  isValidCoin
    :: RSA.PublicKey          -- ^ creator public key
    -> M.Map TXHash SignedTX  -- ^ the "chain"
    -> SignedTX               -- ^ TX to verify
    -> Either Text Bool
  isValidCoin cpk _ stx@(SignedTX (CreateCoin _) _) =
    return $ verifyTXSig cpk stx
  isValidCoin cpk m stx@(SignedTX (TransferCoin txh _) _) =
    case M.lookup txh m of
      Nothing
        -> Left ("No TX for : " <> show txh)
      Just cc@(SignedTX (CreateCoin _) _)
        -> isValidCoin cpk m cc
      Just    (SignedTX (TransferCoin _ opk) _)
        -> if verifyTXSig opk stx
           then return True
           else return False

