> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE DataKinds       #-}
> {-# LANGUAGE KindSignatures  #-}
> {-# LANGUAGE PolyKinds       #-}
> {-# LANGUAGE TypeFamilies    #-}
> {-# LANGUAGE TypeOperators   #-}
>
> module E1_impl where
>
> import           Data.ByteString as BS
> import           Data.Proxy
> import qualified Data.Serialize  as S
> import qualified Database.Redis  as Hedis
> import qualified GHC.TypeLits    as T

https://arxiv.org/pdf/1708.09158.pdf

Type Safe Redis Queries:
A Case Study of Type-Level Programming in Haskell

2nd Workshop on Type-Driven Development (TyDe 2017), September 3, 2017, Oxford, UK

Redis
- in-memory data structure store : key/value pairs
- dynamically typed
  - key can be discarded/reassociated to value of different type
  - when command fetches value of type it does not expect: signals runtime error
- string is primitive type : e.g., numbers (de)serialized to string

interactive Redis:

keys : some-set, another-set : assigned a set of strings

if keys do not exist, they are created

calls to SADD return sizes of the resulting sets

redis> SADD some-set a b c
(integer) 3
redis> SADD another-set a b
(integer) 2

intersection:

redis> SINTER some-set another-set
1) "a"
2) "b"

libraries provide access Redis through TCP.

https://hackage.haskell.org/package/hedis

Redis computation returning a value of type a is represented by

    Redis (Either Reply a)

where
- Redis is a monad
- Either Reply a
  - computation returns value of type 'a'
  - fails with error message : Reply

previous example in Redis

    program :: Redis (Either Reply [ByteString])
    program = do
      sadd "some-set" ["a", "b"]
      sadd "another-set" ["a", "b", "c"]
      sinter ["some-set", "another-set"]

            key           values
    sadd :: ByteString → [ByteString] → Redis (Either Reply Integer)

Problems

commands work only with data of certain types

 # assigned a string foo to key some-string
redis> SET some-string foo
OK
 # SADD adds a value to a set
redis> SADD some-string bar
(error) WRONGTYPE Operation against a key
holding the wrong kind of value

2 Indexed Monads

Stateful computations often reasoned using Hoare logic.

Hoare triple {P}S{Q} denotes
- if statement S executed in state satisfying predicate P (precondition)
- then when it terminates, the state must satisfy predicate Q (postcondition)

represent stateful computations with monads

to label a state monad with its pre and postcondition use indexed monad (aka parameterised monad or monadish)
- in addition to type of its result
- takes two type parameters representing initial and final states

> class IMonad m where
>   unit :: a -> m p p a
>   bind :: m p q a -> (a -> m q r b) -> m p r b

add the pre/postconditions at type level:

> newtype Edis p q a = Edis {unEdis :: Hedis.Redis a}
> instance IMonad Edis where
>   unit     = Edis . return
>   bind m f = Edis (unEdis m >>= unEdis . f )

properties of state that are tracked
- set of currently allocated keys
- types of the key's values

Redis commands executed in two contexts
- normal context : represented by Redis (Either Reply a)          -- only cover this in paper
- transaction    : represented by two datatypes RedisTx (Quued a)

Redis PING replies with message PONG if connection is alive

Edis version applies an additional constructor

> -- does not alter data store, so postcondition/precondition the same
> ping :: Edis xs xs (Either Hedis.Reply Hedis.Status)
> ping  = Edis Hedis.ping

3 Type-Level Dictionaries

dictionary : keep track of types of existing keys
- e.g., [ ("A", Int), ("B", Char), ("C", Bool) ]

dictionaries are parameters to indexed monad Edis
- to index a monad, dictionaries must a type as well as term

3.1 Datatype Promotion

distinction between
- values : categorized by types
- types  : categorized by kinds
- kinds  : categorized by arity

  data Nat = Zero | Suc Nat
  data [a] = []   | a : [a]

left-hand  : defines type
           : defines kind - DataKinds
right-hand : defines value constructors
           : defines type - DataKinds

> type DictEmpty = '[]                             -- :k DictEmpty => DictEmpty :: [*]
> type Dict0     = '[ '("key", Bool) ]             -- :k Dict0     => [(ghc-prim-0.5.1.1:GHC.Types.Symbol, *)]
> -- type Dict1     = '[ '("A"  , Int), '("B", "A") ] -- compiler complains
> type Dict2     = '[ '("B", "A")    ]             -- :k Dict2     => [(ghc-prim-0.5.1.1:GHC.Types.Symbol, ...Symbol)]
> type Dict3     = '[ '("C", Int)    ]             -- :k Dict3     => [(ghc-prim-0.5.1.1:GHC.Types.Symbol, *)]

3.2 Type-Level Functions

operations on dictionary as type

> type family Or (a :: Bool) (b :: Bool) :: Bool where
>   Or 'True _ = 'True
>   Or     _ b = b
>
> type family Not a where
>   Not 'False = 'True
>   Not 'True  = 'False
>
> type family If (c :: Bool) (t :: a) (f :: a) :: a where
>   If 'True  t _ = t
>   If 'False _ f = f

operations on type-level dictionaries

Get xs k returns entry associated with k in xs
- first case : type-level equality expressed by unifying type variables with same name
- partial function on types
  - Get '['("A", Int)] "A" evaluates to Int
  - Get '['("A", Int)] "B" : no applicable rules, stays un-reduced

> -- gets entry associated with k
> type family Get    (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol)          :: *                 where
>   Get ('(k, x) ': xs) k = x
>   Get ('(t, x) ': xs) k = Get xs k
> -- inserts or updates an entry
> type family Set    (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol) (x :: *) :: [ (T.Symbol, *) ] where
>   Set               '[]  k x = '(k, x) ': '[]
>   Set    ('(k, y) ': xs) k x = '(k, x) ': xs
>   Set    ('(t, y) ': xs) k x = '(t, y) ': Set xs k x
> -- removes an entry
> type family Del    (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol)          :: [ (T.Symbol, *) ] where
>   Del               '[]  k   = '[]
>   Del    ('(k, y) ': xs) k   = xs
>   Del    ('(t, y) ': xs) k   = '(t, y) ': Del xs k
> -- membership
> type family Member (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol)          :: Bool              where
>   Member             '[] k   = 'False
>   Member ('(k, x) ': xs) k   = 'True
>   Member ('(t, x) ': xs) k   = Member xs k

4 Embedding Hedis Commands

4.1 Proxies and Singleton Types

Hedis : del::[ByteString] → Redis (Either Reply Integer)
- takes list of keys and removes associated entries.

Edis, first attemp

    del :: String → Edis xs (Del xs ?) (Either Reply Integer)
    del key = Edis (Hedis.del [encode key ])

encode : converts String to ByteString

term-level : our del calls Hedis.del
type-level : if status of database before del meets constraint represented by  dictionary xs
             status afterwards should meet constraint Del xs ?

what to fill in place of ?

Can't use 'Del xs key'
- because key is a runtime value, not a type

singleton types : use to represent a type as a value
- term can be thought of as a representative of its type at the realm of runtime values

use

    data Proxy t = Proxy

To call del
- instead of passing a key as a String value
- give it a proxy

    del (Proxy :: Proxy "A")

where "A" is not a value : it is a string lifted to a type (of kind Symbol)

   del :: Proxy k → Edis xs (Del xs k) (Either Reply Integer)

next problem

del, at term level, gets a value constructor Proxy without further info,
but it needs to pass a ByteString key to Hedis.del

Every concrete string literal lifted to a type (e.g., "A") belongs to type class KnownSymbol.

For all types k in KnownSymbol

    symbolVal :: KnownSymbol k ⇒ proxy k → String ,

retrieves the string associated with a type-level literal

> del :: T.KnownSymbol k => Proxy k -> Edis xs (Del xs k) (Either Hedis.Reply Integer)
> del key = Edis (Hedis.del [encodeKey key])
>
> encodeKey :: T.KnownSymbol k => Proxy k -> BS.ByteString
> encodeKey = S.encode . T.symbolVal

4.2 Automatic Serialization

Hedis programmers manually convert data of other types to strings before saving them into the data store.

In Edis, to keep track of intended types of strings in the data store, use:

> data StringOf :: * -> *
> data ListOf   :: * -> *
> data SetOf    :: * -> *

If key associated with StringOf Int in dictionary
- mean its value in store was serialized from an Int and should be used as an Int

ListOf a : list of type a
SetOf  a : set  of type a

> set
>  :: (T.KnownSymbol k, S.Serialize a)
>  => Proxy k
>  -> a
>  -> Edis xs (Set xs k (StringOf a)) (Either Hedis.Reply Hedis.Status)
> set key v = Edis (Hedis.set (encodeKey key) (S.encode v))

> get
>   :: (T.KnownSymbol s, S.Serialize x, StringOf x ~ (Get xs s)) -- FromJust (Get xs s))
>   => Proxy s
>   -> Edis xs xs (Either Hedis.Reply (Maybe x))
> get key = Edis $ Hedis.get (encodeKey key) >>= decodeAsMaybe

executing
    set (Proxy::Proxy "A") True
updates the dictionary with an entry
    '("A", StringOf Bool)
If "A" not in dictionary, entry is added; otherwise existing type of "A" is updated

Redis INCR reads string value of given key, parses it as integer, increments it, stores it back (as string).

> incr :: (T.KnownSymbol k, Get xs k ~ StringOf Integer)
>      => Proxy k
>      -> Edis xs xs (Either Hedis.Reply Integer)
> incr key = Edis (Hedis.incr (encodeKey key))
>
> incrbyfloat
>      :: (T.KnownSymbol k, Get xs k ~ StringOf Double)
>      => Proxy k
>      -> Double
>      -> Edis xs xs (Either Hedis.Reply Double)
> incrbyfloat key eps = Edis (Hedis.incrbyfloat (encodeKey key) eps)

EQUALITY CONSTRAINTS : ∼

enforce that intended type of value of k must Integer and Double

4.3 Disjunctive Constraints

commands LPUSH key val
         LLEN key
succeed either when key appears in the data store and is assigned a list, or when key does not appear at all.

Need predicate equivalent to Get xs k == ListOf a ∨ ¬ (Member xs k).

To make conjunctive constraint P ∧ Q, put them both in the type: (P, Q) ⇒ ....

To make disjunctive constraint : Get xs k ∼ ListOf a ‘Or‘ Not (Member xs k)

avoid referring to a, which might not exist
define/use predicate IsList :: ∗ → Bool
such that IsList t reduces to 'True only if t = ListOf a

preconditions: "well-typed, or nonexistent" :

> type family IsList   (t :: *) :: Bool where
>   IsList   (ListOf   a) = 'True
>   IsList             _  = 'False
> type family IsSet    (t :: *) :: Bool where
>   IsSet    (SetOf    a) = 'True
>   IsSet              _  = 'False
> type family IsString (t :: *) :: Bool where
>   IsString (StringOf a) = 'True
>   IsString           _  = 'False
> type ListOrNX    xs k   = (IsList   (Get xs k) `Or` Not (Member xs k)) ~ 'True
> type SetOrNX     xs k   = (IsSet    (Get xs k) `Or` Not (Member xs k)) ~ 'True
> type StringOrNX  xs k   = (IsString (Get xs k) `Or` Not (Member xs k)) ~ 'True

> lpush
>   :: (T.KnownSymbol k, S.Serialize a, ListOrNX xs k)
>   => Proxy k
>   -> a
>   -> Edis xs (Set xs k (ListOf a)) (Either Hedis.Reply Integer)
> lpush key val = Edis (Hedis.lpush (encodeKey key) [S.encode val])
>
> llen
>   :: (T.KnownSymbol k, ListOrNX xs k)
>   => Proxy k
>   -> Edis xs xs (Either Hedis.Reply Integer)
> llen key = Edis (Hedis.llen (encodeKey key))
>
> sadd
>   :: (T.KnownSymbol k, S.Serialize a, SetOrNX xs k)
>   => Proxy k
>   -> a
>   -> Edis xs (Set xs k (SetOf a)) (Either Hedis.Reply Integer)
> sadd key val = Edis (Hedis.sadd (encodeKey key) [S.encode val])

command with more complex type

> setnx
>   :: (T.KnownSymbol k, S.Serialize a)
>   => Proxy k
>   -> a
>   -> Edis xs (If (Member xs k) xs (Set xs k (StringOf a))) (Either Hedis.Reply Bool)
> setnx key val = Edis (Hedis.setnx (encodeKey key) (S.encode val))

creates a new entry (key, val) in data store only if key is fresh

type of setnx computes postcondition as well as serving as documentation

4.4 Hashes

Hash : datatype supported by Redis

Redis data store seen as set of key/value pairs

hash : set of field/value pairs

following assigns hash to key 'user'
- fields are name    , birthyear, verified
- values     banacorn, 1992     , 1

redis> hmset user name banacorn
       birthyear 1992 verified 1
OK
redis> hget user name
"banacorn"
redis> hget user birthyear
"1992"

fields can have different types

to keep track of types of fields in a hash

> data HashOf :: [ (T.Symbol, *) ] -> *
> type family IsHash   (t :: *) :: Bool where
>   IsHash   (HashOf   a) = 'True
>   IsHash             _  = 'False
> type HashOrNX    xs k   = (IsHash   (Get xs k) `Or` Not (Member xs k)) ~ 'True

dictionary entry (k, HashOF ys)
denote that value of key k is hash whose fields and their types are specified by ys, which is also a dictionary.

operations on dictionaries with hashes

> type family GetHash (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol) (f :: T.Symbol)          :: *                 where
>   GetHash ('(k, HashOf hs) ': xs) k f   = Get hs f
>   GetHash ('(l, y)         ': xs) k f   = GetHash xs k f
> type family SetHash (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol) (f :: T.Symbol) (a :: *) :: [ (T.Symbol, *) ] where
>   SetHash                    '[]  k f a = '(k, HashOf (Set '[ ] f a)) ': '[ ]
>   SetHash ('(k, HashOf hs) ': xs) k f a = '(k, HashOf (Set hs f a)) ': xs
>   SetHash ('(l, y)         ': xs) k f a = '(l, y) ': SetHash xs k f a
> type family DelHash (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol) (f :: T.Symbol)          :: [ (T.Symbol, *) ] where
>   DelHash                    '[]  k f   = '[ ]
>   DelHash ('(k, HashOf hs) ': xs) k f   = '(k, HashOf (Del hs f )) ': xs
>   DelHash ('(l, y)         ': xs) k f   = '(l, y) ': DelHash xs k f
> type family MemHash (xs :: [ (T.Symbol, *) ]) (k :: T.Symbol) (f :: T.Symbol)          :: Bool              where
>   MemHash                    '[]  k f   = 'False
>   MemHash ('(k, HashOf hs) ': xs) k f   = Member hs f
>   MemHash ('(k, x)         ': xs) k f   = 'False
>   MemHash ('(l, y)         ': xs) k f   = MemHash xs k f

Let xs be a dictionary
- GetHash xs k f returns the type of field f in the hash assigned to key k, if both k and f exists
- SetHash xs k f a assigns the type a to the field f of hash k
  if either f or k does not exist, the hash/field is created
- Del xs k f removes a field
- MemHash xs k f checks if key k exists in xs, and its value is a hash having field f

definitions make use of functions Get, Set, and Member defined for dictionaries.

embedding Hedis commands for hashes using above:

> hset
>   :: (T.KnownSymbol k, T.KnownSymbol f, S.Serialize a, HashOrNX xs k)
>   => Proxy k
>   -> Proxy f
>   -> a
>   ->Edis xs (SetHash xs k f (StringOf a)) (Either Hedis.Reply Bool)
> hset key field val = Edis (Hedis.hset (encodeKey key) (encodeKey field) (S.encode val))
>
> hget
>   :: (T.KnownSymbol k, T.KnownSymbol f , S.Serialize a, StringOf a ~ GetHash xs k f )
>   => Proxy k
>   -> Proxy f
>   -> Edis xs xs (Either Hedis.Reply (Maybe a))
> hget key field = Edis (Hedis.hget (encodeKey key) (encodeKey field) >>= decodeAsMaybe)
>
> decodeAsMaybe
>   :: S.Serialize a
>   => Either Hedis.Reply (Maybe ByteString)
>   -> Hedis.Redis (Either Hedis.Reply (Maybe a))
> decodeAsMaybe (Left replyErr) = return $ Left replyErr
> decodeAsMaybe (Right Nothing) = return $ Right Nothing
> decodeAsMaybe (Right (Just str)) = case S.decode str of
>   Left decodeErr -> return $ Left (Hedis.Error $ S.encode decodeErr)
>   Right val      -> return $ Right (Just val)

4.5 Assertions

explicitly declare new keys, after ensuring they do not already exist

> -- adds a fresh key with type a into dictionary
> -- does nothing at term level
> declare
>   :: (T.KnownSymbol k, Member xs k ~ False)
>   => Proxy k
>   -> Proxy a
>   -> Edis xs (Set xs k a) ()
> declare key typ = Edis (return ())
>
> -- initialize dictionary to empty list
> start :: Edis '[] '[] ()
> start  = Edis (return ())

> flushall :: Edis xs xs (Either Hedis.Reply Hedis.Status)
> flushall = Edis Hedis.flushall

