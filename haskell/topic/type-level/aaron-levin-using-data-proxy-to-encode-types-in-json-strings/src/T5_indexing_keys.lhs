> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE FlexibleContexts    #-}
> {-# LANGUAGE FlexibleInstances   #-}
> {-# LANGUAGE GADTs               #-}
> {-# LANGUAGE KindSignatures      #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeFamilies        #-}
>
> module T5_indexing_keys where
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

global index of keys/type-level strings and corresponding type
- accomplished via closed type family : serves as index

TypeFamilies (https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/type-families.html)

> type family TypeKey (a :: *) :: Symbol where
>   TypeKey Int    = "int"
>   TypeKey String = "string"

> instance {-# OVERLAPPING #-} KnownSymbol s => ToJSON   (Proxy s) where
>   toJSON = A.String . pack . symbolVal

> instance {-# OVERLAPPING #-} KnownSymbol s => FromJSON (Proxy s) where
>   parseJSON (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) = return (Proxy :: Proxy s)
>   parseJSON _      = mzero

To incorporate type family need to update `Payload s a` as a GADT to get constructor return values of specific type
- Generalized Algebraic Data Type (https://wiki.haskell.org/Generalised_algebraic_datatype)

> data Payload (s :: Symbol) a :: * where
>   Payload :: a -> Payload (TypeKey a) a

For `ToJSON`/`FromJSON` need equality constraints to work around limitations in type-level computations.

Ideal : `instance ToJSON (ToJSON a, KnownSymbol (TypeKey a)) => ToJSON (Payload (TypeKey a) a)`
- states: if there is `ToJSON` instance for `a` and `TypeKey` mapping on `a` results in known symbol
  then can write a `ToJSON` instance for `Payload`.

but results in error:

05-indexing-your-keys.hs|50 col 28 error| Could not deduce (s ~ Proxy.TypeKey a)
|| from the context (GHC.TypeLits.KnownSymbol (Proxy.TypeKey a),
||                   aeson-0.8.0.2:Data.Aeson.Types.Class.FromJSON a)
||   bound by the instance declaration
||   at /home/aterica/dev/tmp/blogpost/05-indexing-your-keys.hs:47:10-72
||   ‘s’ is a rigid type variable bound by
||       the instance declaration
||       at /home/aterica/dev/tmp/blogpost/05-indexing-your-keys.hs:47:10
|| Expected type: a -> Proxy.Payload s a
||   Actual type: a -> Proxy.Payload (Proxy.TypeKey a) a

workaround
- equality constraint `s ~ TypeKey a`

`(s ~ TypeKey a, KnownSymbol s, ToJSON a)`
- if `s` is constrained to be equal to `TypeKey a` (i.e. `s` is a type of kind `Symbol`)
- and
- `s` is also a `KnownSymbol`
- then can create `ToJSON` instance for `Payload s a`

> instance (s ~ TypeKey a, KnownSymbol s, ToJSON   a) => ToJSON   (Payload s a) where
>   toJSON (Payload a) = object [ "type" .= (Proxy :: Proxy s)
>                               , "data" .= a
>                               ]

> instance (s ~ TypeKey a, KnownSymbol s, FromJSON a) => FromJSON (Payload s a) where
>   parseJSON (Object o) = (o .: "type" :: Parser (Proxy s))
>                          >>
>                          Payload <$> o .: "data"
>   parseJSON _          = mzero

> instance (KnownSymbol s, Show a) => Show (Payload s a) where
>   show (Payload a) = "Payload " <> symbolVal (Proxy :: Proxy s) <> " " <> show a

> -- TODO : is this adequate? what about `s`?
> instance (KnownSymbol s, Eq   a) => Eq   (Payload s a) where
>   (Payload a) == (Payload b) = a == b

> t5j :: BL.ByteString
> t5j  = "{\"type\":\"string\",\"data\":\"cool\"}"

> t5p :: Payload "int" Int
> t5p  = Payload 10

> t5e1 = U.t "t5e1"
>      (decode t5j :: Maybe (Payload "string" String))
>      (Just (Payload "cool"))

> -- t5e2 = decode t5j :: Maybe (Payload "int" String)

    Couldn't match type ‘"string"’ with ‘"int"’

> t5e2 = U.t "t5e2"
>      (encode t5p)
>      "{\"data\":10,\"type\":\"int\"}"

------------------------------------------------------------------------------

> t5test =
>   runTestTT $ TestList $ t5e1 <> t5e2

