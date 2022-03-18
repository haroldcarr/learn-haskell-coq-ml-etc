{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Lib where

-- https://twitter.com/chrislpenner/status/1504528022170333198?s=20&t=LNe2QSzs-SfcmcvUPpVTkg

import           Data.Coerce
import           Data.Proxy
import           Data.Text     as Text
import           Data.UUID     (UUID)
import qualified Data.UUID     as UUID
import           GHC.TypeLits
import           Servant.API
import           System.Random

newtype Censored a = Censored a

newtype Code = Code UUID
  deriving newtype  (FromHttpApiData, ToHttpApiData)
  deriving (Show)   via (Censored Code)
  deriving (Random) via UUID
  deriving (IsID)   via (PrefixedUUID "CODE")

class IsID t where
  toString   :: t -> String
  fromString :: String -> Maybe t

newtype PrefixedUUID (prefix :: Symbol) = PrefixedUUID UUID

instance KnownSymbol s => IsID (PrefixedUUID s) where
  toString (PrefixedUUID uuid) =
    let prefix = symbolVal (Proxy @s) <> "-"
     in prefix <> UUID.toString uuid
  fromString str =
    let prefix = symbolVal (Proxy @s) <> "-"
     in do
      uuidText <- Text.stripPrefix (Text.pack prefix) (Text.pack str)
      uuid     <- UUID.fromString (Text.unpack uuidText)
      pure (PrefixedUUID uuid)

toText :: IsID t => t -> Text
toText  = Text.pack . toString

fromUUID :: Coercible UUID t => UUID -> t
fromUUID  = coerce

instance TypeError ('Text "Show disabled for type: "
                    ':<>: 'ShowType a
                    ':<>: 'Text " since it contains sensitive info")
  => Show (Censored a) where
  show _ = "<censored>"

------------------------------------------------------------------------------

puidX :: PrefixedUUID "X"
puidX  = fromUUID UUID.nil -- PrefixedUUID @"X" UUID.nil -- also works

tt :: Text.Text
tt  = toText puidX

cuid :: Code
cuid  = Code UUID.nil

{-
err :: String
err  = show cuid
-}


