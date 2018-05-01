{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans            #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE StandaloneDeriving #-}

module GCoin where

import qualified "cryptonite" Crypto.Hash as H
import qualified Crypto.PubKey.RSA        as RSA
import qualified Data.ByteArray           as BA
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Either.Combinators  as E
import qualified Data.Map.Strict          as M
import qualified Data.Serialize           as S
import qualified Data.UUID                as U
import qualified Data.UUID.V4             as U
import           Formatting               ((%), sformat, stext)
import qualified Prelude
import           Test.Hspec               as HS
import           Universum
------------------------------------------------------------------------------
import           Crypto

type PK = RSA.PublicKey
type SK = RSA.PrivateKey

-- Hlint complain about DeriveAnyClass, but it is needed to S.Serialize RSA.PublicKey
{-# ANN module ("HLint: ignore Unused LANGUAGE pragma"::Prelude.String) #-}

newtype UUID    = UUID    { getUuid    :: ByteString } deriving (Eq, Generic, Show)
newtype STXHash = STXHash { getSTXHash :: ByteString } deriving (Eq, Generic, Ord, Show)
newtype Hash    = Hash    { getHash    :: ByteString } deriving (Generic, Show)
instance S.Serialize UUID
instance S.Serialize STXHash
instance S.Serialize Hash
deriving instance Generic     RSA.PublicKey
deriving instance S.Serialize RSA.PublicKey

data TX
  = CreateCoin UUID
  | TransferCoin STXHash PK
  deriving (Eq, Generic, Show)
instance S.Serialize TX

data SignedTX = SignedTX
  { sTX  :: TX
  , sSig :: Signature
  } deriving (Eq, Generic, Show)
instance S.Serialize SignedTX

createUUID :: IO UUID
createUUID = do
  u <- U.nextRandom
  return (UUID (BSL.toStrict (U.toByteString u)))

signTX :: SK -> TX -> Either RSA.Error SignedTX
signTX sk tx = E.mapRight (SignedTX tx) (signMsg sk (Msg (S.encode tx)))

verifyTXSigErr :: PK -> TX -> Text
verifyTXSigErr pk tx =
  sformat ("verifyTXSig False: pk: " % stext % "; stx: " % stext)
          (show pk) (show tx)

verifyTXSig :: PK -> SignedTX -> Either Text ()
verifyTXSig pk stx = case stx of
  (SignedTX tx@(CreateCoin _)     sig) -> v tx sig
  (SignedTX tx@(TransferCoin _ _) sig) -> v tx sig
 where v tx s = let b = verifyMsgSig pk (Msg (S.encode tx)) s
                in if b then Right () else Left (verifyTXSigErr pk tx)

createCoinIO :: SK -> IO (Either RSA.Error SignedTX)
createCoinIO sk = do
  u <- createUUID
  return (createCoin u sk)

createCoin :: UUID -> SK -> Either RSA.Error SignedTX
createCoin u sk = signTX sk (CreateCoin u)

transferCoin :: SignedTX -> SK -> PK -> Either RSA.Error SignedTX
transferCoin fromCoin ownerSK toPK = do
  let fromHash = hashSignedTX fromCoin
      toCoin   = TransferCoin fromHash toPK
  signTX ownerSK toCoin

hashSignedTX :: SignedTX -> STXHash
hashSignedTX = STXHash . getHash . hash . encodeSTX

hash :: BS.ByteString -> Hash
hash = Hash . BA.convert . H.hashWith H.SHA256

encodeSTX :: SignedTX -> ByteString
encodeSTX = S.encode

decodeSTX :: ByteString -> SignedTX
decodeSTX bs = case S.decode bs of
  Right s -> s
  _       -> error "decodeSignedTX" -- TODO

isValidCoinBase
  :: (STXHash -> Either Text SignedTX) -- ^ lookup a TX "in the chain"
  -> PK                                -- ^ creator public key
  -> SignedTX                          -- ^ TX to verify
  -> Either Text ()
isValidCoinBase lookup cpk stx = case stx of
  (SignedTX (CreateCoin _)       _) ->
    verifyTXSig cpk stx
  (SignedTX (TransferCoin txh _) _) ->
    case lookup txh of
      Right   cc@(SignedTX (CreateCoin _)       _) ->
        isValidCoinBase lookup cpk cc
      Right next@(SignedTX (TransferCoin _ opk) _) ->
        case verifyTXSig opk stx of
          Right _ -> isValidCoinBase lookup cpk next
          l       -> l
      Left l -> Left l

isValidCoinErrMsg = "no SignedTX found with STXHash == "

isValidCoinTest c = isValidCoinBase lookup
  where lookup x = E.maybeToRight (isValidCoinErrMsg <> show x) (M.lookup x c)

addToChainTest :: M.Map STXHash SignedTX -> [SignedTX] -> M.Map STXHash SignedTX
addToChainTest = foldr (\x cc -> M.insert (hashSignedTX x) x cc)

emptyChainTest = M.empty

-- This test is used in GCoinSpec and in BCSpec.
testIsValidCoin isValidCoinX addToChainX emptyChain = do
  u                      <- runIO createUUID
  (creatorPK, creatorSK) <- runIO generatePKSKIO
  (alicePK  , aliceSK)   <- runIO generatePKSKIO
  (bobPK    , bobSK)     <- runIO generatePKSKIO
  (jimPK    , _jimSK)    <- runIO generatePKSKIO
  let Right cc    = createCoin   u     creatorSK
  let Right cToA  = transferCoin cc    creatorSK alicePK
  let Right aToB1 = transferCoin cToA  aliceSK   bobPK
  let Right aToJ  = transferCoin cToA  aliceSK   jimPK
  let Right bToA  = transferCoin aToB1 bobSK     alicePK
  let Right aToB2 = transferCoin bToA  aliceSK   bobPK
  let chain       = addToChainX emptyChain [cc, cToA, aToB1, bToA]
  let chainBad    = addToChainX emptyChain     [cToA, aToB1, bToA]
  describe "isValidCoin" $ do
    it "createCoin" $
      isValidCoinX emptyChain    creatorPK   cc  `shouldBe` Right ()
    it "chain aToJ" $
      isValidCoinX chain         creatorPK aToJ  `shouldBe` Right ()
    it "chain aToJ (double spend)" $
      isValidCoinX chain         creatorPK aToB1 `shouldBe` Right ()
    it "bad sig" $
      let ccBadSTX@(SignedTX ccBadTX _) = cc { sSig = Signature "BAD" }
      in isValidCoinX emptyChain creatorPK ccBadSTX `shouldBe`
      Left (verifyTXSigErr creatorPK ccBadTX)
    it "aToB2" $
      isValidCoinX chain         creatorPK aToB2 `shouldBe` Right ()
    it "chainBad aToB2" $
      isValidCoinX chainBad      creatorPK aToB2 `shouldBe`
      Left (isValidCoinErrMsg <> show ((\(TransferCoin h _) -> h) (sTX cToA)))
