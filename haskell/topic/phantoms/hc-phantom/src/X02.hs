{-# LANGUAGE NoImplicitPrelude #-}

module X02 where

import           Protolude

-- http://dev.stephendiehl.com/hask/#phantom-types

-- newtypes to distinguish between plaintext and cryptotext

newtype PlaintextNT  = PlaintextNT  Text
newtype CryptotextNT = CryptotextNT Text

data Key = Key

encryptNT :: Key -> PlaintextNT  -> CryptotextNT
decryptNT :: Key -> CryptotextNT -> PlaintextNT
encryptNT _k (PlaintextNT  t) = CryptotextNT t
decryptNT _k (CryptotextNT t) = PlaintextNT  t

-- via phantom types

data Cryptotext
data Plaintext

newtype Msg a = Msg Text

encrypt :: Key -> Msg Plaintext  -> Msg Cryptotext
decrypt :: Key -> Msg Cryptotext -> Msg Plaintext
encrypt _k (Msg t) = Msg t
decrypt _k (Msg t) = Msg t

-- -XEmptyDataDecls with phantom types that contain no value inhabitants : "anonymous types"

-- {-# LANGUAGE EmptyDataDecls #-}

data Token a

-- The tagged library defines a similar Tagged newtype wrapper.
