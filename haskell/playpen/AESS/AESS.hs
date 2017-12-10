{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module AESS where

import           Control.Arrow           ((&&&))
import qualified Crypto.Hash             as CH
import qualified Data.ByteArray.Encoding as BE
import qualified Data.ByteString         as BS
import qualified Data.Map                as M
import qualified Data.Serialize          as S
import           Data.Serialize.Text     ()
import qualified Data.Text               as T
import qualified GHC.Generics            as G

-- | polymorphic
data PX e d r
  = PXEncrypted (EncryptMetadata e)
  | PXDecrypted d
  | PXNoAccess  r
  deriving (G.Generic, Show, S.Serialize)

data EncryptMetadata e = EncryptMetadata
  { emOwnerPublicKey :: T.Text
  , emObjectName     :: T.Text
  , emData           :: e
  } deriving (G.Generic, Show, S.Serialize)

type Hash = BS.ByteString

-- | specific
type X d = PX BS.ByteString d T.Text

-- TODO public/secret
encrypt :: S.Serialize a => T.Text -> T.Text -> a -> (Hash, X a)
encrypt pk on a = (h, PXEncrypted (EncryptMetadata pk on x))
 where x = S.encode a
       h = BE.convertToBase BE.Base64 (CH.hashWith CH.SHA256 x)

-- TODO pub/secret
decrypt :: S.Serialize a => X (EncryptMetadata BS.ByteString) -> Either String (X a)
decrypt (PXEncrypted (EncryptMetadata _pk _on e)) = fmap PXDecrypted (S.decode e)

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

populate =
  let v1   = Vaccination "MMR" "2001-01-01"
      v2   = Vaccination "DIP" "2010-12-31"
      v3   = Vaccination "why" "2015-07-15"
      (hv1, ev1) = encrypt "PK_O1" "V1" v1
      (hv2, ev2) = encrypt "PK_O1" "V2" v2
      (hv3, ev3) = encrypt "PK_O1" "V2" v3
      vl   = [ ev1, ev2, ev3 ]
      (hvl, evl)  = encrypt "PK_O1" "VL" vl
      dob  = "1994-04-13"
      (hdob, edob) = encrypt "PK_O1" "DOB" dob
      vr   = VRecord "John Doe" edob evl
      (hvr, evr)  = encrypt "PK_O1" "VR" vr
      bc   = M.fromList [("PK_01"
                         , M.fromList [ ("V1" , hv1)
                                      , ("V2" , hv2)
                                      , ("V3" , hv3)
                                      , ("VL" , hvl)
                                      , ("DOB", hdob)
                                      , ("VR" , hvr)
                                      ])
                        ]
      ss   = M.fromList [ (hv1,  ev1)
                        , (hv2,  ev2)
                        , (hv3,  ev3)
                        ]

  in (bc, ss)
