{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AESS where

import           Control.Arrow           ((&&&))
import qualified Crypto.Hash             as CH
import qualified Data.ByteArray          as BA
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as BSC
import qualified Data.Map                as M
import qualified Data.Proxy              as PX
import qualified Data.Serialize          as S
import           Data.Serialize.Text     ()
import qualified Data.Text               as T
import qualified GHC.Generics            as G

import Debug.Trace as DB

-- | polymorphic
data X d
  = XEncrypted EncryptMetadata
  | XDecrypted d
  | XNoAccess  T.Text
  deriving (G.Generic, Show, S.Serialize)

data EncryptMetadata = EncryptMetadata
  { emOwnerPublicKey :: T.Text
  , emObjectName     :: T.Text
  , emData           :: BS.ByteString
  } deriving (G.Generic, Show, S.Serialize)

type Hash = BS.ByteString

-- Secret Key -+                  +-> hash -> HO
--             |                  |
-- O          -+-> encrypt -> EO -+-> XEncrypted (EncryptedMetadata PK, ON, EO) -> serialize -> SO
--

encrypt :: S.Serialize a => BS.ByteString -> a -> BS.ByteString
encrypt _k = S.encode

decrypt :: S.Serialize a => BS.ByteString -> BS.ByteString -> Either String a
decrypt _k = S.decode

serialize :: S.Serialize a => a -> BS.ByteString
serialize = S.encode

deserialize :: S.Serialize a => BS.ByteString -> Either String a
deserialize = S.decode

-- TODO public/secret
encryptHashAndSerialize :: forall a. S.Serialize a => X a -> T.Text -> T.Text -> (Hash, BS.ByteString)
encryptHashAndSerialize (XDecrypted a) pk on = (ho, so)
 where eo = encrypt "KEY" a -- TODO encryption
       ho = BS.pack (BA.unpack (CH.hashWith CH.SHA256 eo))
       eso :: X a -> BS.ByteString
       eso = serialize
       so = eso (XEncrypted (EncryptMetadata pk on eo))

-- TODO public/secret
deserializeX :: forall a b. (S.Serialize a, S.Serialize b) => BS.ByteString -> PX.Proxy b -> Either String (X b)
deserializeX so _ =
  let dso :: BS.ByteString -> Either String (X a)
      dso = deserialize
      eo = case dso so of
             l@Left {} -> error "error decoding" -- TODO
             Right (XEncrypted (EncryptMetadata pk on eo)) -> DB.trace (("X: "::String) ++ T.unpack pk ++ (" "::String) ++ T.unpack on  ++ (" "::String) ++ show eo) eo
   in DB.trace (("YES: "::String) ++ BSC.unpack eo) decrypt "KEY" eo

deserializeX' :: forall a. S.Serialize a => BS.ByteString -> Either String (X a)
deserializeX' so =
  let dso :: BS.ByteString -> Either String (X a)
      dso = deserialize
   in dso so

-- import Data.Text
-- :set -XOverloadedStrings
-- let (ho, eo) = encryptHashAndSerialize (XDecrypted "FOO") "PK" "ON"
-- let (Right (XEncrypted (EncryptMetadata pk on ed))) = deserializeX' eo :: Either String (X Text)
-- decrypt "KEY" ed :: Either String Text

example = do
  let (ho, eo) = encryptHashAndSerialize (XDecrypted ("FOO"::T.Text)) "PK" "ON"
  let (Right (XEncrypted (EncryptMetadata pk on ed))) = deserializeX' eo :: Either String (X T.Text)
  decrypt "KEY" ed :: Either String T.Text

-- example2 (Proxy::Proxy Text)
example2 :: S.Serialize p => PX.Proxy p -> Either String p
example2 p = do
  let (ho, eo) = encryptHashAndSerialize (XDecrypted ("FOO"::T.Text)) "PK" "ON"
  let (Right (XEncrypted (EncryptMetadata pk on ed))) = deserializeX' eo :: Either String (X T.Text)
  ds p ed

-- example3 (Proxy::Proxy Text) "FOO"
-- example3 (Proxy::Proxy Int) (1::Int)
-- example3 (Proxy::Proxy (Int,Float)) (1::Int,3.0::Float)
example3 :: S.Serialize p => PX.Proxy p -> p -> Either String p
example3 p pp = do
  let (ho, eo) = encryptHashAndSerialize (XDecrypted pp) "PK" "ON"
  let (Right (XEncrypted (EncryptMetadata pk on ed))) = deserializeX' eo :: Either String (X Int) -- (X T.Text) -- Any type to X is OK -- TODO
  ds p ed

-- :set -XOverloadedStrings
-- encryptHashAndSerialize  (XDecrypted "FOO")                      "PK" "ON"
-- encryptHashAndSerialize ((XDecrypted "FOO")::(X Data.Text.Text)) "PK" "ON"
-- ed  (XDecrypted "FOO")
-- ed ((XDecrypted "FOO")::(X Data.Text.Text))
-- ed ((XDecrypted "FOO")::(X Data.Text.Text)) :: Either String (X Data.Text.Text)

-- THIS WORKS:
-- let (Right (XEncrypted (EncryptMetadata _pk _on eo))) = ed'  (XDecrypted "FOO")
-- deserialize eo :: Either String Text
-- import Data.Proxy
-- let p = Proxy::Proxy Text
-- ds p eo

ds :: S.Serialize a => PX.Proxy a -> BS.ByteString -> Either String a
ds _ = deserialize

{-
ed :: forall a b. (S.Serialize a, S.Serialize b) => X a -> PX.Proxy b -> (X b)
ed x p =
  let (_ho, so) = encryptHashAndSerialize x "PK01" "fooname"
   in case deserializeX so p of
    Left err -> error err
    Right b -> b
-}

ed' :: forall a. S.Serialize a => X a -> Either String (X a)
ed' x =
  let (_ho, so) = encryptHashAndSerialize x "PK01" "fooname"
   in deserializeX' so

data MPCReturn = MPCReturn
  { mpcObjectHash :: T.Text
  , mpcDecryptKey :: T.Text
  } deriving (Eq, Ord, Show)

data VRecord = VRecord
  { vrName         :: T.Text
  , vrDob          :: X T.Text
  , vrVaccinations :: X [ X Vaccination ] -- TODO : Authenticated List
  } deriving (G.Generic, Show, S.Serialize)

data Vaccination = Vaccination
  { vType :: T.Text
  , vDate :: T.Text
  } deriving (G.Generic, Show, S.Serialize)

{-
store :: ( M.Map T.Text (M.Map T.Text Hash) -- constract state
         , M.Map Hash BS.ByteString         -- storage
         )
store =
  let v1                 = Vaccination "MMR" "2001-01-01"
      v2                 = Vaccination "DIP" "2010-12-31"
      v3                 = Vaccination "why" "2015-07-15"
      (hv1, ev1, xv1)    = encrypt "PK_O1" "V1" v1
      (hv2, ev2, xv2)    = encrypt "PK_O1" "V2" v2
      (hv3, ev3, xv3)    = encrypt "PK_O1" "V2" v3
      vl                 = [xv1, xv2, xv3]
      (hvl, evl, xvl)    = encrypt "PK_O1" "VL" vl
      dob                = "1994-04-13"
      (hdob, edob, xdob) = encrypt "PK_O1" "DOB" dob
      vr                 = VRecord "John Doe" xdob xvl
      (hvr, evr, _)      = encrypt "PK_O1" "VR" vr
      bc                 = M.fromList
        [("PK_01"
         , M.fromList [ ("V1" , hv1)
                      , ("V2" , hv2)
                      , ("V3" , hv3)
                      , ("VL" , hvl)
                      , ("DOB", hdob)
                      , ("VR" , hvr)
                      ])
        ]
      ss                 = M.fromList
        [ (hv1,  ev1)
        , (hv2,  ev2)
        , (hv3,  ev3)
        , (hvl,  evl)
        , (hdob, edob)
        , (hvr,  evr)
        ]
  in (bc, ss)

retrieve :: ( M.Map T.Text (M.Map T.Text Hash) -- constract state
            , M.Map Hash BS.ByteString         -- storage
            )
         -> T.Text
         -> T.Text
         -> Maybe VRecord
retrieve (bc, ss) owner oid = do
  w   <- M.lookup owner bc
  h   <- M.lookup oid   w
  obs <- M.lookup h     ss
  evr <- case S.decode obs :: Either String (X VRecord) of
             Left {}  -> error "decode failed"
             Right r@(XEncrypted (EncryptMetadata pk on x)) -> Just r
  vr  <- case decrypt evr :: Either String (X VRecord) of
             Left {} -> error "decrypt failed"
             Right (XDecrypted vr) -> Just vr
  return undefined
-}
