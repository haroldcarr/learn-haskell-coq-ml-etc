> module T0_intro_TOC where
>
> import           T1_sum_type_string_compare_no_type_level
> import           T2_data_proxy
> import           T3_more_general_proxy
> import           T4_proxy_and_payload
> import           T5_indexing_keys
>
> import           Test.HUnit      (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util as U (t, tt)

http://aaronlevin.ca/post/111871447488/using-dataproxy-to-encode-types-in-your-json
https://gist.github.com/aaronlevin/4aa22bd9c79997029167

problem
- json-encoded message with generic payloads
- each payload contains keys `type` and `data`
- `type` : string-encoded value to indicate how to demux/parse `data`


{ "from"    : "you"
, "to"      : "me"
, "payload" : { "type" : "identity_validation_request"
              , "data" : { ... }
              }
}

goal:
- create data type to encode `type` value as type-level string
  - holds type-level string in its type
- parse `json` encoded string if `type` value matches type-level string
  - parse :: ByteString -> Maybe (Payload s a)

data Payload s a
- `s` holds value of type
- `a` is data type of payload

T1_sum_type_string_compare_no_type_level
- typical Haskell/JSON handling

T2_data_proxy
T3_more_general_proxy
- de/serialize `Proxy` values of type `Proxy (s :: Symbol)`

T4_proxy_and_payload
- de/serialize `Payload (s :: Symbol) (a :: *)` datatype
  to associcate arbitrary payloads with type-level strings

T5_indexing_keys
- `TypeKey` type family to maintain a global index of types and their assumed keys
- de/serialize values of type `Payload (TypeKey a) a`

T6_polymorphic_containers
de/serialize values of type `Message a`
- wrapper around `Payload (TypeKey a) a` : interface for clients

------------------------------------------------------------------------------

want to deserialize JSON that required dispatch on specific value of json key `type`
- based on that value, parse JSON into a specific type

attempts

1. Ad-hoc
2. Using a sum type
3. Encoding expected value of `type` in type-level string

mostly explored last option

1. de/serialize `Proxy` values of type `Proxy (s :: Symbol)`
  - enables encoding `type` value as  type-level string in proxy
2. using `1` created `Payload (s :: Symbol) (a :: *)` datatype to associcate arbitrary payloads with type-level strings
3. ca de/serialize values of  type `Payload s a`
4. global index of types and assumed keys using type family `TypeKey`
5. using `4` de/serialize values of type `Payload (TypeKey a) a`
   - encoding json-key assumptions at compiletime in global, unique index
6. added `Message a` datatype to wrap `Payload (TypeKey a) a`
   - interface for clients
7. compiler error if try to deserialize `Message a` where `a` has no entry in `TypeKey` type family index

further inspiration, try to grok
- reflection (https://hackage.haskell.org/package/reflection)

------------------------------------------------------------------------------

> test = do
>   t1test
>   t2test
>   t3test
>   t4test
>   t5test
