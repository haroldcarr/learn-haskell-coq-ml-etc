> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE FlexibleInstances   #-}
> {-# LANGUAGE KindSignatures      #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE QuasiQuotes       #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module T4_proxy_and_payload where
>
> import           Control.Applicative  ((<$>))
> import           Control.Monad        (mzero)
> import           Data.Aeson
> import qualified Data.Aeson           as A
> import           Data.Aeson.Types
> import qualified Data.ByteString.Lazy as BL
> import           Data.Monoid          ((<>))
> import           Data.Proxy           (Proxy (Proxy))
> import           Data.Text            (pack)
> import           GHC.TypeLits         (KnownSymbol, Symbol, symbolVal)
> import           Test.HUnit           (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util      as U (t, tt)
> import           Text.RawString.QQ

to simplify `Payload` de/serialization, change Proxy serialization to just be a A.String
(instead of a full json object as in previous examples)

> instance {-# OVERLAPPING #-} KnownSymbol s => ToJSON   (Proxy s) where
>   toJSON = A.String . pack . symbolVal

> instance {-# OVERLAPPING #-} KnownSymbol s => FromJSON (Proxy s) where
>   parseJSON (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) = return (Proxy :: Proxy s)
>   parseJSON _ = mzero

KindSignatures (https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/other-type-extensions.html)

> -- `s` : type-level string representing value expected as JSON `type` field value
> -- `a` : generic payload
> newtype Payload (s :: Symbol) a = Payload a deriving (Eq)

> instance (KnownSymbol s, ToJSON   a) => ToJSON   (Payload s a) where
>   toJSON (Payload a) = object [ "type" .= (Proxy :: Proxy s)
>                               , "data" .= a
>                               ]

> instance (KnownSymbol s, FromJSON a) => FromJSON (Payload s a) where
>   parseJSON (Object o) = (o .: "type" :: Parser (Proxy s)) -- first deserialize into `Proxy s`
>                          >>                                -- if success, discard result and parse rest
>                          Payload <$> o .: "data"
>   parseJSON _          = mzero

> instance (KnownSymbol s, Show a) => Show (Payload s a) where
>   show (Payload a) = "Payload " <> symbolVal (Proxy :: Proxy s) <> " " <> show a

> t4j  = [r|{"type":"String", "data":"cool"}|]

> t4e1 = U.t "t4e1"
>      (decode t4j :: Maybe (Payload "String" String))
>      (Just (Payload "cool"))

> t4e2 = U.t "t4e2"
>      (decode t4j :: Maybe (Payload "Int" String))
>      Nothing

> t4e3 = U.t "t4e3"
>      (decode t4j :: Maybe (Payload "String" Int))
>      Nothing

> t4p = Payload 10 :: Payload "My Integer" Int
> t4e4 = U.t "t4e4"
>      (encode t4p :: BL.ByteString)
>      "{\"data\":10,\"type\":\"My Integer\"}"

able to specify in `Payload` type the value of the `"type"` key expected

code using `Payload` more readable

assumptions made by analyzing type (e.g. function returning `Payload "invite_request" InviteRequest`).

------------------------------------------------------------------------------

> t4test =
>   runTestTT $ TestList $ t4e1 ++ t4e2 ++ t4e3 ++ t4e4
