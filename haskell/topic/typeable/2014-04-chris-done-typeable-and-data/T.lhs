> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module T where
>
> import Control.Monad.State
> import Data.Char (isUpper)
> import Data.Data
> import Data.Generics.Aliases (extQ)
> import Data.Maybe (fromJust)

Typeable and Data in Haskell

https://chrisdone.com/posts/data-typeable

a way to do generic programming

don’t know what data types are being given,
want to work upon them as if you did

Requirements

data types must be instances of 'Typeable' and 'Data' to do generic programming

can be derived using : {-# LANGUAGE DeriveDataTypeable #-}

> data X = X
>   deriving (Data,Typeable)

can print type of any instance of Typeable
find some instances

λ> :i Typeable
class Typeable a where typeOf :: a -> TypeRep
instance [overlap ok] (Typeable1 s, Typeable a) => Typeable (s a)
instance [overlap ok] Typeable TypeRep
instance [overlap ok] Typeable TyCon
instance [overlap ok] Typeable Ordering
instance [overlap ok] Typeable Integer
instance [overlap ok] Typeable Int
instance [overlap ok] Typeable Float
instance [overlap ok] Typeable Double
instance [overlap ok] Typeable Char
instance [overlap ok] Typeable Bool
instance [overlap ok] Typeable ()

only one method in the Typeable class

typeOf :: a -> TypeRep

λ> :i TypeRep
instance [overlap ok] Eq TypeRep
instance [overlap ok] Ord TypeRep
instance [overlap ok] Show TypeRep
instance [overlap ok] Typeable TypeRep

------------------------------------------------------------------------------
Use-case 1: Print the type of something

λ> :t typeOf 'a'
typeOf 'a' :: TypeRep

λ> typeOf 'a'
Char

use
- debugging
- writing generic encoders
- need an identifier to be associated with some generic value

------------------------------------------------------------------------------
Use-case 2: Compare the types of two things

λ> typeOf 'a' == typeOf 'b'
True
λ> typeOf 'a' == typeOf ()
False

use
- code that needs to allow any type to be passed in,
  but needs to check for a specific type.

> uc2 :: (Ord a, Typeable a) => a -> Bool
> uc2 a = typeOf a == typeOf 'c'

------------------------------------------------------------------------------
Use-case 3: Reifying from generic to concrete

given a generic value, if type is right,
work with the value as the concrete type, not a polymorphic type.

using

    cast :: (Typeable a, Typeable b) => a -> Maybe b

to convert polymorphic value to concrete

> -- | if given a Char, return its string representation, otherwise, return "unknown"
> char :: Typeable a => a -> String
> char a = case cast a of
>   Just (a' :: Char) -> show a'
>   Nothing           -> "unknown"

λ> char 'a'
"'a'"
λ> char 5
"unknown"
λ> char ()
"unknown"

That’s where interesting practical applications of Typeable ends.

------------------------------------------------------------------------------
The Data class

but Data class can take advantage of Typeable

why:
- enables looking into a data type’s
- constructors and fields
- and traverse across or fold over them

instance Data a => Data [a]
instance Data Ordering
instance Data a => Data (Maybe a)
instance Data Integer
instance Data Int
instance Data Float
instance (Data a, Data b) => Data (Either a b)
instance Data Double
instance Data Char
instance Data Bool

------------------------------------------------------------------------------
Use-case 1: Get the data type

use dataTypeOf to get a unique representation of a data type (similar to using TypeRep):

dataTypeOf :: Data a => a -> DataType

λ> dataTypeOf (Just 'a')
DataType {tycon = "Prelude.Maybe", datarep = AlgRep [Nothing,Just]}

Representations can be used as references from which to reify into more concrete values.

------------------------------------------------------------------------------
Use-case 2: Inspecting a data type

get a list of constructors that a type contains

λ> :t dataTypeConstrs
dataTypeConstrs :: DataType -> [Constr]
λ> dataTypeConstrs (dataTypeOf (Nothing :: Maybe ()))
[Nothing,Just]

it is common to need to know what constructor is at a particular index using:

λ> indexConstr (dataTypeOf (Nothing :: Maybe ())) 2
Just

to ask if a data type is algebraic
(i.e., does it have constructors and is it not one of the built-in types like Int/Float/etc)?

λ> isAlgType (dataTypeOf (Just 'a'))
True
λ> isAlgType (dataTypeOf 'a')
False

------------------------------------------------------------------------------
Use-case 3: Get the constructor of a value

toConstr :: a -> Constr

λ> :i Constr
data Constr
instance Eq Constr
instance Show Constr

can’t do much with a constructor as-is, but compare and print it (which can be useful):

λ> toConstr (Just 'a')
Just
λ> toConstr (Just 'a') == toConstr (Nothing :: Maybe Char)
False

can also get the DataRep of a constructor:

λ> constrType (toConstr (Just 'a'))
DataType {tycon = "Prelude.Maybe", datarep = AlgRep [Nothing,Just]}

------------------------------------------------------------------------------
Use-case 4: Get fields of a constructor

get the field names of a constructor

> data X4 = X4 { foo :: Int, bar :: Char } deriving (Typeable,Data)

λ> toConstr (X4 0 'a')
X4
λ> constrFields (toConstr (X4 0 'a'))
["foo","bar"]

use
- serialization
- debugging

------------------------------------------------------------------------------
Use-case 5: Make a real value from its constructor

fromConstr :: Data a => Constr -> a

λ> fromConstr (toConstr (Nothing :: Maybe ())) :: Maybe ()
Nothing

if constructor has fields:

fromConstrB :: forall a. Data a
            => (forall d. Data d => d) -> Constr -> a

Haskell beginners: Don’t fear the rank-N type. What it’s saying is
merely that the fromConstrB function determines what the type of d
will be by itself, by looking at Constr. It’s not provided externally
by the caller, as it would be if the forall d. were at the same level
as the a. Think of it like scope. let a = d in let d = … doesn’t make
sense: the d is in a lower scope. That means we can’t just write:

fromConstrB (5 :: Int) (toConstr (Just 1 :: Maybe Int)) :: Maybe Int

The Int cannot unify with the d because the quantification is one
level lower. It basically doesn’t exist outside of the (forall d. Data
d => d) (nor can it escape). That’s okay, though. There is a
type-class constraint which lets us be generic. We already have a
function producing a value of that type:

λ> :t fromConstr (toConstr (1 :: Int))
fromConstr (toConstr (1 :: Int)) :: Data a => a

λ> fromConstrB (fromConstr (toConstr (1 :: Int)))
               (toConstr (Just 1 :: Maybe Int)) :: Maybe Int
Just 1

if there are more fields, need to provide more than one, and of different types

fromConstrM :: forall m a. (Monad m, Data a)
            => (forall d. Data d => m d)
            -> Constr
            -> m a

Because it’s monadic we can use a state monad to keep an index

λ> :t execState
execState :: State s a -> s -> s
λ> :t execState (modify (+1))
execState (modify (+1)) :: Num s => s -> s
λ> :t execState (forM_ [1..5] (const (modify (+1))))
execState (forM_ [1..5] (const (modify (+1)))) :: Num s => s-> s
λ> execState (forM_ [1..5] (const (modify (+1)))) 5
10

> data Fooey = Fooey Int Char deriving (Data,Show,Typeable)
>
> es :: Fooey
> es = evalState
>       (fromConstrM
>         (do i <- get
>             modify (+1)
>             return
>               (case i of
>                 0 -> fromConstr (toConstr (5::Int))
>                 1 -> fromConstr (toConstr 'b')
>                 _ -> error "NO"))
>         (toConstr (Fooey 4 'a')))
>       (0 :: Int) :: Fooey

Foo 5 'b'

keep an index starting at 0
increase it each iteration that fromConstrM does
when at index 0, return an Int
when at index 1, ...

------------------------------------------------------------------------------
Use-case 6: mapping over data structures generically

gmapT :: forall a. Data a
      => (forall b. Data b => b -> b) -> a -> a

> gmd :: Typeable p => p -> p
> gmd d = case cast d of -- use cast to determine if type is one needed
>   Nothing -> d -- not the one
>   Just x  ->
>     fromJust (cast -- cast concrete Char back into generic type
>                -- x is Char because type inference from isUpper
>                -- does 'isUpper' on field value in Char constructor
>                (if isUpper x then '!' else x))
>
> gmt1 :: Fooey
> gmt1 = gmapT gmd (Fooey 4 'a')

Fooey 4 'a'

> gmt2 :: Fooey
> gmt2 = gmapT gmd (Fooey 4 'A')

Foo 4 '!'

like fromConstrMr, if need to operate on exact indices of the constructor
rather than going by type, you can use gmapM and a state monad

------------------------------------------------------------------------------
Use-case 7: generating from data structures generically

walk over the values of a data structure, collecting the result.

can use gmapM and state or write monad

but function exists:

gmapQ :: forall a. Data a => (forall d. Data d => d -> u) -> a -> [u]

> gmq :: [Constr]
> gmq = gmapQ toConstr (Fooey 5 'a')

[5,'a']

useful example from structured-haskell-mode
https://github.com/chrisdone/structured-haskell-mode/blob/18c011978acfca30bac800d0ac0e9e31e653c440/src/Main.hs#L96
walks over Haskell syntax tree
collects source spans into a flat list

example in present package
https://github.com/chrisdone/present/blob/master/src/Present.hs#L75

example in Fay
https://github.com/faylang/fay/blob/master/src/Fay/Convert.hs#L54
to encode types to JSON with a specific Fay-runtime-specific encoding.

Printer example

> {-# ANN gshows ("HLint: ignore Use any" :: String) #-}
> gshows :: Data a => a -> ShowS
> gshows = render `extQ` (shows :: String -> ShowS) where
>   render t
>     | isTuple = showChar '('
>               . drop 1
>               . commaSlots
>               . showChar ')'
>     | isNull = showString "[]"
>     | isList = showChar '['
>              . drop 1
>              . listSlots
>              . showChar ']'
>     | otherwise = showChar '('
>                 . constructor
>                 . slots
>                 . showChar ')'
>    where
>     constructor = showString . showConstr . toConstr $ t
>     slots       = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
>     commaSlots  = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
>     listSlots   = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
>     isTuple     = all (==',') (filter (not . flip elem "()") (constructor ""))
>     isNull      = null (filter (not . flip elem "[]") (constructor ""))
>     isList      = constructor "" == "(:)"

wrote it because GHC API doesn’t have Show instances for most of its data types

> data Foo = Foo Int Char deriving (Data,Typeable) -- no Show

λ> gshows (Foo 5 'a') ""
"(Foo (5) ('a'))"

λ> gshows ([Just (2::Int)],'c',Foo 5 'a') ""
"([(Just (2))],('c'),(Foo (5) ('a')))"

See also: Data.Generics.Aliases

