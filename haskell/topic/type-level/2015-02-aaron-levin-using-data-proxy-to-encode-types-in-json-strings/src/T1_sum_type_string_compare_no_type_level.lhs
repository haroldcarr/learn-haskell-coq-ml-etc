> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE QuasiQuotes       #-}
>
> module T1_sum_type_string_compare_no_type_level where
>
> import           Control.Applicative ((<$>), (<*>))
> import           Control.Monad       (mzero)
> import           Data.Aeson
> import qualified Data.Aeson          as A
> import           Data.ByteString
> import           Data.Text           as T
> import           Test.HUnit          (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util     as U (e, t, tt)
> import           Text.RawString.QQ

goal
- dispatch on key in `type`

> -- | each constructor is associated with a JSON `type` field value
> data Payloads
>   = One   String
>   | Two   Int    String
>   | Three String String Int
>   deriving (Eq, Show)

parse
- inspect `type` field value, dispatch accordingly
- forces keeping json keys in one place
- but loses generality
  - not polymorphic
    - working with specific constructors
    - not an arbitrary, abstract container
- has expression problem
  - add a new payload, add to dispatch and other places

> -- | dispatch on `type` field value
> instance FromJSON Payloads where
>   parseJSON (Object o) = o .: "type" >>= handle
>     where
>       handle (A.String "one") =
>         o .: "data" >>= \d -> One <$> d .: "msg"
>
>       handle (A.String "two") =
>         o .: "data" >>= \d -> Two <$> d .: "id" <*> d .: "msg"
>
>       handle (A.String "three") =
>         o .: "data" >>= \d -> Three <$> d .: "first"
>                                     <*> d .: "last"
>                                     <*> d .: "age"
>       handle (A.String t) = error $ "unrecognized type: " ++ T.unpack t -- mzero
>   parseJSON _             = error "no 'type' field"                     -- mzero

> t1j = [r|{ "type" : "three",
>            "data" : { "first" : "harold",
>                       "last"  : "carr",
>                       "age"   : 45 } }|]

> t1jMissingType = [r|{ "data" : 0 } |]

> t1jUnrecognizedType = [r|{ "type" : "bad",
>                            "data" : 0 }|]

> t1e1 = U.t "t1e1"
>      (eitherDecode t1j)
>      (Right (Three "harold" "carr" 45))

TODO: why does t1e2 get a Left and t1e3 raise an error?

> t1e2 = U.t "t1e2"
>      (eitherDecode t1jMissingType :: Either String Payloads)
>      (Left "Error in $: key \"type\" not present")

> t1e3 = eitherDecode t1jUnrecognizedType :: Either String Payloads

t1_t3 gets:
  unrecognized type: bad
CallStack (from HasCallStack):
  ...

------------------------------------------------------------------------------

> t1test =
>   runTestTT $ TestList $ t1e1 ++ t1e2
