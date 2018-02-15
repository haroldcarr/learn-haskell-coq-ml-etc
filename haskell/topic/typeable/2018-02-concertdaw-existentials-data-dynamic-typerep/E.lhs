> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE Rank2Types                #-}
> {-# LANGUAGE ScopedTypeVariables       #-}
>
> module E where
>
> import Data.Proxy
> import Data.Typeable

https://medium.com/@concertdaw/existential-crisis-366cf6ee426a

storing/operating on collections of mixed-type values

Haskell wiki page on heterogeneous collections suggests

- algebraic sum type
- "universal type" — embodied by Data.Dynamic
- "Existential" types
- Hlist package

Problems with using an algebraic data type

> -- | WireValues represent data that our API receives "off the wire"
> data WireValue1 = WvInt1 Int | WvString1 String deriving (Show, Eq)

λ: [WvInt1 3, WvString1 "hello"]
[WvInt1 3,WvString1 "hello"]

but must pattern match everywhere

> wireValueAppend :: WireValue1 -> WireValue1 -> Maybe WireValue1
> wireValueAppend (WvString1 l) (WvString1 r) = Just $ WvString1 $ l ++ r
> wireValueAppend _ _ = Nothing

λ: wireValueAppend (WvString1 "hello") (WvString1 "world")
Just (WvString1 "helloworld")
λ: wireValueAppend (WvString1 "hello") (WvInt1 3)
Nothing

cumbersome when more types added to WireValue1

Another problem

> data WireValue2 = WvInt2 Int | WvString2 String | WvList2 [WireValue2] deriving (Show, Eq)

about type is too permissive
— possible to store a heterogeneous list of values inside a WvList itself
- making it necessary to validate the type of each value of the list at runtime

Do existentials have the answer?

https://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
https://wiki.haskell.org/Existential_type
https://gist.github.com/CMCDragonkai/b203769c588caddf8cb051529339635c

existential type declarations have a type variable on the RHS that is not present on the LHS:

> data WireValue3 = forall a. Show a => WireValue3 a
> instance Show WireValue3 where show (WireValue3 a) = show a

The explicit forall a. on the RHS is the actual existential quantification

λ: WireValue 3
3
λ: WireValue "Hello"
"Hello"

note:
{-# LANGUAGE StandaloneDeriving #-}
deriving instance Show WireValue


extend with Eq constraint

> data WireValue4 = forall a. (Show a, Eq a) => WireValue4 a

but define an Eq instance get compiler error

instance Eq WireValue where
  (WireValue x) == (WireValue y) = x == y
-- Couldn't match expected type ‘a’ with actual type ‘a1’...

cannot guarantee that x and y are of the same type

need a safe way of casting values

λ: :t Data.Typeable.cast
cast :: (Typeable b, Typeable a) => a -> Maybe b

> data WireValue5 = forall a. (Typeable a, Show a, Eq a) => WireValue5 a
> instance Eq WireValue5 where
>   WireValue5 x == WireValue5 y = maybe False (x ==) $ cast y

λ: WireValue 3 == WireValue 3
True
λ: WireValue 3 == WireValue 4
False
λ: WireValue 3 == WireValue "apple"
False


https://chrisdone.com/posts/data-typeable (and related Data.Data package)


get a representation of a type from a value:

λ: :type Data.Typeable.typeOf
Data.Typeable.typeOf :: Typeable a => a -> TypeRep


definition of Dynamic contains an existential

data Dynamic = forall a. TypeRep a => Dynamic a

(translated from original GADT)

says : if type of value a can be represented, then it can be wrapped with Dynamic
(similar to WireValue).

use-case for using types as values (other than cast)

Orderability

defining Ord instance for WireValue has problem
- don’t know what the right answer is when the types don’t match

data WireValue = forall a. (Typeable a, Show a, Eq a, Ord a) => WireValue a
instance Ord WireValue where
  compare (WireValue x) (WireValue y) = maybe _onMismatchedTypes (compare x) $ cast y

_onMismatchedTypes can’t (magically) produce an Ordering value like LT
because inconsistent answers depending on which order you compared the two values.

Values of TypeRep are comparable, so, if the cast fails, compare them.

> data WireValue6 = forall a. (Typeable a, Show a, Eq a, Ord a) => WireValue6 a
> instance Eq WireValue6 where
>   WireValue6 x == WireValue6 y = maybe False (x ==) $ cast y
> instance Ord WireValue6 where
>   compare (WireValue6 x) (WireValue6 y) =
>     maybe
>       (compare (typeOf x) (typeOf y))
>       (compare x) $ cast y


λ: WireValue 3 < WireValue 4
True
λ: WireValue "hello" < WireValue 1
False
λ: WireValue 1 < WireValue "hello"
True

------------------------------------------------------------------------------

Types as values

convert a type to a value (before knowing Data.Typeable existed)

define data types that represent types WireValues can contain

> data WireConcreteType  = WctInt  | WctString deriving (Show, Eq, Ord)
> data WireContainerType = WctList | WctMaybe deriving (Show, Eq, Ord)
> data WireType
>   = WtConc WireConcreteType
>   | WtCont WireContainerType WireType
>   deriving (Show, Eq, Ord)

can be used to represent variety of types

-- Int:
t1 = WtConc WctInt
-- [String]:
t2 = WtCont WctList $ WtConc WctString
-- [Maybe Int]
t3 = WtCont WctList $ WtCont WctMaybe $ WtConc WctInt
...

getType: to extract a type as a value at runtime
- use proxy so actual value not needed
  - useful in case of lists : enable defining getType recursively
    - avoids needing to handle an empty list

> class Wireable1 a where
>   getType :: proxy a -> WireType
> instance Wireable1 Int where
>   getType _ = WtConc WctInt
> instance Wireable1 a => Wireable1 [a] where
>   getType _ = WtCont WctList $ getType (Proxy :: Proxy a)

ScopedTypeVariables so can refer to specific a that getType called with when
building Proxy in Wireable instance for [a].

λ: getType (Proxy :: Proxy Int)
WtConc WctInt
λ: getType (Proxy :: Proxy [[Int]])
WtCont WctList (WtCont WctList (WtConc WctInt))

getType : equivalent operation to Data.Typeable typeRep

WireType : same role as Data.Typeable TypeRep
- but more restricted
  - TypeRep can represent ADTs with multiple args to constructors
  - hash helper functions to unpack and inspect it

TypeApplications extension : better syntax for specifying types of polymorphic functions

λ: :set -XTypeApplications
λ: getType $ Proxy @Int
WtConc WctInt

Summary so far of using existential quantification
- can store values of mixed type in same collection
- Data.Typeable.cast: can safely extract values back out
- fine if code using it
  - doesn’t care about type contained in WireValue, or
  - if know which type needed to extract from it

PROBLEM: act differently based on what is stored in existential type.

extract (WireValue x) = x

Couldn't match expected type ‘p’ with actual type ‘a’
  because type variable ‘a’ would escape its scope
This (rigid, skolem) type variable is bound by
  a pattern with constructor:
    WireValue :: forall a. (Typeable a, Show a) => a -> WireValue,
        in an equation for ‘extract’

skolem : can’t know at compile time what x will be, but it will be something

cast is polymorphic in its return type because it the caller of cast determines what to cast

WANT :
- callee chooses type
- caller provide polymorphism

> overWv1 :: WireValue5 -> (forall a. Typeable a => a -> b) -> b
> overWv1 (WireValue5  x) f = f x

Rank2Types
- explicit forall to bind variable a at tighter scope than variable b
- given a WireValue, and fun that can take value of ANY Typeable a
  to a value of some type b that you choose
  and I’ll give you a value of that b type.

To create a function that can handle any Typeable a use Wireable class (above)

> class Wireable2 a where
>   myFoo :: a -> Bool
>
> data WireValue7 = forall a. (Wireable2 a, Typeable a) => WireValue7 a
>
> overWv2 :: WireValue7 -> (forall a. Wireable2 a => a -> b) -> b
> overWv2 (WireValue7 x) f = f x
> doFoo :: WireValue7 -> Bool
> doFoo wv = overWv2 wv myFoo

But not good
- couples Wireable, WireValue
- etc
