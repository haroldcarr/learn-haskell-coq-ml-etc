> {-# LANGUAGE DataKinds           #-}
> {-# LANGUAGE FlexibleInstances   #-}
> {-# LANGUAGE OverloadedStrings   #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module T3_more_general_proxy where
>
> -- import Control.Applicative ((<$>), (<*>))
> import           Control.Monad        (mzero)
> import           Data.Aeson
> import qualified Data.Aeson           as A
> import qualified Data.ByteString.Lazy as BL
> import           Data.Proxy           (Proxy(Proxy))
> import           Data.Text            (pack)
> import           GHC.TypeLits         (KnownSymbol, symbolVal)
> import           Test.HUnit           (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util      as U (t, tt)
> import           Text.RawString.QQ

Avoid writing `FromJSON` instance for `Proxy "bar"` and every type-level string that might appear.

> instance {-# OVERLAPPING #-} KnownSymbol s => ToJSON   (Proxy s) where
>   toJSON p = object [ "type" .= symbolVal p ]

> instance {-# OVERLAPPING #-} KnownSymbol s => FromJSON (Proxy s) where
>   parseJSON (Object o) = o .: "type" >>= handleType
>     where
>       handleType (A.String s) | s == pack (symbolVal (Proxy :: Proxy s)) = return (Proxy :: Proxy s)
>       handleType _ = mzero
>   parseJSON _      = mzero

> t3f = "{\"type\":\"foo\"}"
> t3b = "{\"type\":\"bar\"}"

> t3e1 = U.t "t3e1"
>      (eitherDecode t3f)
>      (Right (Proxy :: Proxy "foo"))

> t3e2 = U.t "t3e2"
>      (eitherDecode t3f :: Either String (Proxy "bar"))
>      (Left "Error in $: mzero")

> t3e3 = U.t "t3e3"
>      (eitherDecode t3b)
>      (Right (Proxy :: Proxy "bar"))

Needs `ScopedTypeVariables`, without:

  compilation error:

    Couldn't match kind ‘*’ with ‘GHC.TypeLits.Symbol’
    Expected type: Value
                   -> aeson-0.8.0.2:Data.Aeson.Types.Internal.Parser (Proxy s)
      Actual type: Value
                   -> aeson-0.8.0.2:Data.Aeson.Types.Internal.Parser (Proxy s0)
    In the second argument of ‘(>>=)’, namely ‘handleType’
    In the expression: o .: "type" >>= handleType

problem
- GHC differentiates between vars that appear in type signature from vars in function's definition
- so, `s` in `Proxy s` in type signature different from `s` in `(Proxy :: Proxy s)` in definition

resolution
- ScopedTypeVariables (https://wiki.haskell.org/Scoped_type_variables)
- extends scope of type var throughout function definition
- enables GHC to infer `s` satisfies `KnownSymbol` constraint

------------------------------------------------------------------------------

> t3test =
>   runTestTT $ TestList $ t3e1 ++ t3e2 ++ t3e3
