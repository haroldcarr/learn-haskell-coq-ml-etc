{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           Control.Monad.Trans.State
import           Data.Char
import           Data.Data
import           Data.Generics.Aliases     (extQ)
import           Data.Maybe
import           Test.Hspec

{-
-- https://chrisdone.com/posts/data-typeable/

Data.Typeable / Data.Data for generic programming : SYB

do not know what data types are given, but work on them as if you did

Requirements

data types must be instances of Typeable and Data via DeriveDataTypeable
-}

data X = X deriving (Data, Typeable)

{-
Use-case 1: Print the type of something

λ: :i typeOf
typeOf :: Typeable a => a -> TypeRep 	-- Defined in ‘Data.Typeable’

λ: :i typeRep
typeRep ::
  forall k (proxy :: k -> *) (a :: k).
  Typeable a =>
  proxy a -> TypeRep
  	-- Defined in ‘Data.Typeable’

λ> :t typeOf 'a'
                :: TypeRep
λ> typeOf 'a'
Char
-}

e1 = it "typeOf" $ showsTypeRep (typeOf 'a') "" `shouldBe` "Char"

{-
useful for
- debugging
- generic encoders
- any code that needs an identifier to be associated with some generic value

Use-case 2: Compare the types of two things
-}

e2 = it "typeOf 'a' == typeOf 'b'" $ typeOf 'a' == typeOf 'b' `shouldBe` True
e3 = it "typeOf 'a' == typeOf ()"  $ typeOf 'a' == typeOf ()  `shouldBe` False

{-
Use-case 3: Reifying from generic to concrete

given a generic value, work with the value as the concrete type, not a polymorphic type.

e.g., printing function:
-}
-- Given an Char, return its string rep, else "unknown".
-- Uses 'cast' to convert from polymorphic to concrete value.
--    cast :: (Typeable a, Typeable b) => a -> Maybe b
char :: Typeable a => a -> String
char x = case cast x of
  Just (x' :: Char) -> show x'
  Nothing           -> "unknown"

e4 = it "char 'a'" $ char 'a'      `shouldBe` "'a'"
e5 = it "char  5"  $ char (5::Int) `shouldBe` "unknown"

{-
------------------------------------------------------------------------------
Data.Data class

functions for traversing/folding over a type’s constructors and fields

Use-case 1: Get the data type (similar to Typeable.TypeRep) : dataTypeOf :: Data a => a -> DataType
-}

e6 = it "dataTypeOf" $ dataTypeRep (dataTypeOf 'a') `shouldBe` dataTypeRep (mkCharType "a")
                       -- dataTypeOf 'a' : DataType {tycon = "Prelude.Char", datarep = CharRep}


{-
Representations (so-called FooRep) tend to be references which can be reified into more concrete values.

Use-case 2: Inspecting a data type : get a list of constructors : dataTypeConstrs :: DataType -> [Constr]
-}

e7 = it "dataTypeConstrs" $ dataTypeConstrs (dataTypeOf (Nothing :: Maybe ())) `shouldBe`
                            [ mkConstr (dataTypeOf (Nothing:: Maybe ())) "Nothing" [   ] Prefix
                            , mkConstr (dataTypeOf (Just            ())) "Just"    ["a"] Prefix ]

-- constructor at a particular index

e8 = it "indexConstr" $ indexConstr (dataTypeOf (Nothing :: Maybe ())) 2 `shouldBe`
                              mkConstr (dataTypeOf (Just            ())) "Just"    ["a"] Prefix

-- is algebraic? (does it have constructors and not a built-in types like Int/Float/etc)?

e9  = it "isAlgType Just" $ isAlgType (dataTypeOf (Just 'a')) `shouldBe` True
e10 = it "isAlgType 'a'"  $ isAlgType (dataTypeOf 'a')        `shouldBe` False

{-
Use-case 3: Get the constructor of a value : toConstr :: a -> Constr

Which given any instance of Data will yield a constructor.
-}

e11 = it "toConstr Just"  $ show (toConstr (Just 'a')) `shouldBe` "Just"
e12 = it "Constr == Constr" $ toConstr (Just 'a') == toConstr (Nothing :: Maybe Char) `shouldBe`
                              False

-- get the DataRep of a constructor:

e13 = it "constrType . toConstr" $ show (constrType (toConstr (Just 'a'))) `shouldBe`
                       "DataType {tycon = \"Maybe\", datarep = AlgRep [Nothing,Just]}"


-- Use-case 4: Get fields of a constructor : constrFields (for debugging/serialization)

data Y = Y { foo :: Int, bar :: Char } deriving (Data, Typeable)

e14 = it "constrFields" $ constrFields (toConstr (Y 0 'a')) `shouldBe` ["foo","bar"]

-- Use-case 5: Make a real value from its constructor : fromConstr :: Data a => Constr -> a

e15 = it "fromConstr"  $ (fromConstr  (toConstr (Nothing :: Maybe ())) :: Maybe ()) `shouldBe` Nothing

{-
for one arg constructors

fromConstrB :: forall a. Data a => (forall d. Data d => d) -> Constr -> a


Uses rank-N : fromConstrB determines type of d itself, by looking at Constr.
It’s not provided externally by the caller (as it would be if forall d. were at same level as a).
Think of it like scope:
   let a = d in let d = … doesn’t make sense: the d is in a lower scope
so cannot write:

  fromConstrB (5 :: Int) (toConstr (Just 1 :: Maybe Int)) :: Maybe Int

The Int cannot unify with the d because the quantification is one level lower.

It does not exist outside of the (forall d. Data d => d) (nor can it escape).
There is a type-class constraint to be generic via:

λ> :t fromConstr (toConstr (1 :: Int))
fromConstr (toConstr (1 :: Int)) :: Data a => a
-}

e16 = it "fromConstrB" $ (fromConstrB (fromConstr (toConstr (1 :: Int)))
                                      (toConstr (Just 1 :: Maybe Int))
                           :: Maybe Int) `shouldBe`
                         Just 1
{-
If > 1 fields:

fromConstrM :: forall m a. (Monad m, Data a)
            => (forall d. Data d => m d) -> Constr -> m a

monadic, so use a state monad to keep an index
-}

data Foo = Foo Int Char deriving (Data, Eq, Typeable, Show)

e17 = it "fromConstrM" $
  (evalState
    (fromConstrM
      (do i <- get
          modify (+1)
          return
            (case i of
              0 -> fromConstr (toConstr (5::Int))
              1 -> fromConstr (toConstr 'b')
              n -> error (show n)))
      (toConstr (Foo 4 'a')))
     0 :: Foo)
  `shouldBe` Foo 5 'b'
{-

Use-case 6: mapping over data structures generically

gmapT :: forall a. Data a
      => (forall b. Data b => b -> b) -> a -> a

like fromConstr* : rank-n type b refers to each type in constructor of type a.
-}

e1819f d = case cast d of -- use cast to reify generic d into concrete Char.
  Nothing -> d
  Just x  -> fromJust (cast (if isUpper x then '!' else x)) -- cast concrete Char back to generic d

e18 = it "gmapT 1" $ gmapT e1819f (Foo 4 'a') `shouldBe` Foo 4 'a'
e19 = it "gmapT 2" $ gmapT e1819f (Foo 4 'A') `shouldBe` Foo 4 '!'

{-
like fromConstrM above, to operate on exact indices of the constructor rather than by type,
use gmapM and a state monad

Use-case 7: generating from data structures generically


walk over values of a data structure, collecting the resul

can do via gmapM + state monad or a writer, or:

gmapQ :: forall a. Data a => (forall d. Data d => d -> u) -> a -> [u]

Trivial example:
-}

e20 = it "gmapQ" $ show (gmapQ toConstr (Foo 5 'a')) `shouldBe` "[5,'a']"

{-
examples:

in structured-haskell-mode
- walks over Haskell syntax tree
- collects source spans into list

in 'present package'

in Fay to encode types to JSON with a specific Fay-runtime-specific encoding.

Printer example
-}
gshows :: Data a => a -> ShowS
gshows = render `extQ` (shows :: String -> ShowS)
 where
  render t
    | isTuple = showChar '('
              . drop 1
              . commaSlots
              . showChar ')'
    | isNull = showString "[]"
    | isList = showChar '['
             . drop 1
             . listSlots
             . showChar ']'
    | otherwise = showChar '('
                . constructor
                . slots
                . showChar ')'
   where
    constructor = showString . showConstr . toConstr $ t
    slots       = foldr (.) id . gmapQ ((showChar ' ' .) . gshows) $ t
    commaSlots  = foldr (.) id . gmapQ ((showChar ',' .) . gshows) $ t
    listSlots   = foldr (.) id . init . gmapQ ((showChar ',' .) . gshows) $ t
    isTuple     = all (==',') (filter (not . flip elem "()") (constructor ""))
    isNull      = not (any (not . flip elem "[]") (constructor ""))
    isList      = constructor "" == "(:)"

data Bar = Bar Char Int deriving (Data, Eq, Typeable) -- NO Show

e21 = it "gshows" $
  gshows ([Just (2::Int)], 'c', Bar 'a' 5)
         ""
  `shouldBe` "([(Just (2))],('c'),(Bar ('a') (5)))"

{-
gshows motivation: GHC API does not have Show instances many of its data types,
therefore hard to inspect in REPL.

Summary

- query
- cast
- walk or generate

See also: Data.Generics.Aliases
-}
