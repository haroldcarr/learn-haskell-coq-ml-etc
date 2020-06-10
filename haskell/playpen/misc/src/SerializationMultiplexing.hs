{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module SerializationMultiplexing where

import qualified Data.Serialize  as S
import           Protolude

newtype Epoch = Epoch Int deriving (Eq, Generic, Show)
newtype Round = Round Int deriving (Eq, Generic, Show)
instance S.Serialize Epoch
instance S.Serialize Round

data BlockInfo = BlockInfo
  { _biEpoch :: !Epoch
  , _biRound :: !Round
  } deriving (Eq, Generic, Show)
instance S.Serialize BlockInfo

bi0 :: BlockInfo
bi0  = BlockInfo (Epoch 1) (Round 2)

data Tag = Tag deriving (Eq, Show, Generic)
instance S.Serialize Tag

encodeWithTag :: ByteString
encodeWithTag  = S.encode (Tag, bi0)

encodeWithoutTag :: ByteString
encodeWithoutTag  = S.encode bi0

decodeDemux :: ByteString -> Either BlockInfo BlockInfo
decodeDemux bs =
  case S.decode bs of
    Right (tag, bi) | tag == Tag
      -> Left  bi
    Right (x, bi)
      -> panic ("unexpected tag " <> show x <> " for " <> show bi)
    Left _ -> case S.decode bs of
      Right bi -> Right bi
      Left  e  -> panic (show e)

wt, wot :: Either BlockInfo BlockInfo
wt  = decodeDemux encodeWithTag
wot = decodeDemux encodeWithoutTag
