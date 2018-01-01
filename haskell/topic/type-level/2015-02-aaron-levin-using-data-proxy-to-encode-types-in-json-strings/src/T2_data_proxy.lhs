> {-# LANGUAGE DataKinds         #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE QuasiQuotes       #-}

> module T2_data_proxy where

> import T1_sum_type_string_compare_no_type_level (Payloads)

> import           Control.Monad (mzero)
> import           Data.Aeson
> import qualified Data.Aeson as A
> import qualified Data.ByteString.Lazy as BL
> import           Data.Proxy (Proxy(Proxy))
> import           GHC.TypeLits (symbolVal)
> import           Test.HUnit      (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util as U (t, tt)
> import           Text.RawString.QQ

encode `type` string

data Proxy a = Proxy
- value-level reps of type level info
- `a` can be anything
  - e.g., `Proxy "foo"` is a type (`DataKinds`)

Make a type:
:t (Proxy :: Proxy "foo")
=>                        :: Proxy "foo"

> t2p1 :: Proxy "foo"
> t2p1  = (Proxy :: Proxy "foo")

> t2p2 :: Proxy "foo"
> t2p2  = undefined

Turn a type into a value:
 https://hackage.haskell.org/package/singletons-2.2/docs/Data-Singletons-TypeLits.html#t:KnownSymbol
-- `Symbol` to `String`
symbolVal :: KnownSymbol s => Proxy s -> String

> t2e1 = U.tt "t2e1"
>      [ symbolVal t2p1
>      , symbolVal t2p2 -- even though `undefined` this returns "foo"
>      ]
>      "foo"

create JSON instances of Proxy

> instance {-# OVERLAPPING #-} ToJSON   (Proxy "foo") where
>   toJSON p = object [ "type" .= symbolVal p ]

> instance {-# OVERLAPPING #-} FromJSON (Proxy "foo") where
>   parseJSON (Object o) = o .: "type" >>= handle
>     where
>       handle (A.String "foo") = return (Proxy :: Proxy "foo")
>       handle _                = mzero
>   parseJSON _  = mzero

Note: Aeson defines : forall k (a :: k). FromJSON (Proxy a)

     without OVERLAPPING:

    • Overlapping instances for FromJSON (Proxy "foo")
        arising from a use of ‘decode’
      Matching instances:
        instance forall k (a :: k). FromJSON (Proxy a)
          -- Defined in ‘aeson-0.11.2.1:Data.Aeson.Types.Instances’
        ...plus one instance involving out-of-scope types
          instance FromJSON (Proxy "foo")
    • In the expression: decode t2_jsonS :: Maybe (Proxy "foo")

 https://hackage.haskell.org/package/aeson-1.1.0.0/docs/src/Data-Aeson-Types-ToJSON.html#toJSON

  instance ToJSON1 Proxy where
    liftToJSON _ _ _ = Null
    {-# INLINE liftToJSON #-}

    liftToEncoding _ _ _ = E.null_
    {-# INLINE liftToEncoding #-}

  instance ToJSON (Proxy a) where
    toJSON _ = Null
    {-# INLINE toJSON #-}

    toEncoding _ = E.null_
    {-# INLINE toEncoding #-}

 https://hackage.haskell.org/package/aeson-1.1.0.0/docs/src/Data-Aeson-Types-FromJSON.html

  instance FromJSON1 Proxy where
    {-# INLINE liftParseJSON #-}
    liftParseJSON _ _ Null = pure Proxy
    liftParseJSON _ _ v    = typeMismatch "Proxy" v

  instance FromJSON (Proxy a) where
    {-# INLINE parseJSON #-}
    parseJSON Null = pure Proxy
    parseJSON v    = typeMismatch "Proxy" v

> t2j  = [r|{"type":"foo"}|]

> t2e2 = U.t "t2e2"
>      (encode (Proxy :: Proxy "foo"))
>      t2j

> t2e3 = U.t "t2e3"
>      (encode (Proxy :: Proxy "bar"))
>      "null"

> t2e4 = U.t "t2e4"
>      (eitherDecode t2j)
>      (Right (Proxy :: Proxy "foo"))

 https://hackage.haskell.org/package/aeson-1.1.0.0/docs/src/Data-Aeson-Types-FromJSON.html

typeMismatch :: String -- ^ The name of the type you are trying to parse.
             -> Value  -- ^ The actual value encountered.
             -> Parser a
typeMismatch expected actual =
    fail $ "expected " ++ expected ++ ", encountered " ++ name
  where
    name = case actual of
             Object _ -> "Object"
             Array _  -> "Array"
             String _ -> "String"
             Number _ -> "Number"
             Bool _   -> "Boolean"
             Null     -> "Null"

> t2e5 = U.t "t2e5"
>      (eitherDecode t2j :: Either String  (Proxy "bar"))
>      (Left "Error in $: expected Proxy, encountered Object")

------------------------------------------------------------------------------

> t2test =
>   runTestTT $ TestList $ t2e1 ++ t2e2 ++ t2e3 ++ t2e4 ++ t2e5
