{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Lib where

import           Data.Void
import           Numeric.Natural
import           Test.HUnit
import qualified Test.HUnit.Util as U (t)

{-
Alexis King
An introduction to typeclass metaprogramming (TMP)
2021-03-25

Technique to automatically generate term-level code from static type information.
Used in servant ecosystem.
Mechanism used to implement generic programming via GHC generics.

-- ===========================================================================
* Part 1: Basic building blocks

------------------------------------------------------------------------------
Typeclasses as functions from types to terms

typeclasses : mechanism for operator overloading

TMP : typeclasses are functions from types to (runtime) terms.
-}

class TypeOf1 a where
  typeOf1 :: a -> String

instance TypeOf1 Bool where
  typeOf1 _ = "Bool"

instance TypeOf1 Char where
  typeOf1 _ = "Char"

instance (TypeOf1 a, TypeOf1 b) => TypeOf1 (a, b) where
  typeOf1 (a, b) = "(" ++ typeOf1 a ++ ", " ++ typeOf1 b ++ ")"

ttypeOf1 :: [Test]
ttypeOf1  = U.t "typeOf1" (typeOf1 (True, 'a')) "(Bool, Char)"

{-
Instances ignore args : whole point TypeOf class is to get access to type info,
which is same regardless of value provided.

Use GHC extensions to eliminate value-level args

{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}
-}

class TypeOf a where
  typeOf :: String -- AllowAmbiguousTypes (to remove arg)
{-
type parameter a does not appear anywhere in body.
The type of each method of a typeclass is implicitly extended with the typeclass’s constraint.
E.g., in
-}

class Show0 a where
  show0 :: a -> String

{-
type of show method is implicitly extended with a 'Show a' constraint:

show0 :: Show0 a => a -> String

Also, using foralls, each typeclass method is implicitly quantified over class’s type parameters:

show0 :: forall a. Show0 a => a -> String

Therefore:

typeOf :: forall a. TypeOf a => String

Type param 'a' does not appear to right of =>.
Makes type param ambiguous (impossible for GHC to infer 'a' at any call site.
Use TypeApplications to pass a type:
-}

instance TypeOf Bool where
  typeOf = "Bool"

instance TypeOf Char where
  typeOf = "Char"

instance (TypeOf a, TypeOf b) => TypeOf (a, b) where
  typeOf = "(" ++ typeOf @a ++ ", " ++ typeOf @b ++ ")"
-- TypeApplications      ^                    ^
-- ScopedTypeVariables    ^                    ^

ttypeOf :: [Test]
ttypeOf  = U.t "typeOf" (typeOf @(Bool, Char)) "(Bool, Char)"

{-
KEY POINT : illustrates how typeclasses can be seen as FUNCTIONS FROM TYPES TO TERMS.
- 'typeOf' accepts takes type as arg, returns term-level String.

------------------------------------------------------------------------------
Type-level interpreters

Consequence of eliminating value arg to typeOf : no need for arg type to be inhabited.
-}

instance TypeOf Void where
  typeOf = "Void"

{-
Important point: language of types is mostly blind to term-level meaning of those types.
Typeclasses usually written to operate on values, but that is not essential.

Important in practice, e.g.,:
-}

instance TypeOf a => TypeOf [a] where
  typeOf = "[" ++ typeOf @a ++ "]"

{-
If typeOf required a value arg, not just a type, instance would not work on empty list,
since it would have no value of type a to recursively apply typeOf to.

Can use typeclasses to write functions on uninhabited types.
-}

-- impossible to contruct values of these types
data Z
data S a

-- Can use a typeclass to distinguish and convert to term-level values:

-- think of reifyNat as an interpreter of a type-level language
class ReifyNat a where
  reifyNat :: Natural

instance ReifyNat Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat (S a) where
  reifyNat = 1 + reifyNat @a

treifyNatZ   :: [Test]
treifyNatZ    = U.t "treifyNatZ" (reifyNat       @Z)   0
treifyNatSZ  :: [Test]
treifyNatSZ   = U.t "treifyNatZ" (reifyNat    @(S Z))  1
treifyNatSSZ :: [Test]
treifyNatSSZ  = U.t "treifyNatZ" (reifyNat @(S (S Z))) 2

{-
------------------------------------------------------------------------------
ANTIPATTERN: Overlapping instances e.g., instance for Show (Maybe a) and (Maybe Bool)
- not clear if show (Just True) should use 1st or 2nd instance
GHC rejects instance overlap when detected
- to preserve coherency : same combination of type args always selects same instance

For TMP, ignore this rule of thumb

example

ordinary, value-level function, would be something like this pseudo-Haskell:

isUnit :: * -> Bool
isUnit () = True
isUnit _  = False
-}
class IsUnit a where
  isUnit :: Bool

-- typeclass instances, need overlap:

instance {-# OVERLAPPING #-} IsUnit () where
  isUnit = True

-- needs FlexibleInstances
instance IsUnit a where
  isUnit = False

tisUnitT :: [Test]
tisUnitT  = U.t "tisUnitT" (isUnit @() ) True
tisUnitF :: [Test]
tisUnitF  = U.t "tisUnitF" (isUnit @Int) False


{-
a function definition has a closed set of clauses matched from top to bottom.
typeclass instances are open and unordered

OVERLAPPING
- relaxes overlap checker as long as the instance is
  strictly more specific than instance(s) it overlaps with

overlap useful for TMP
- makes it possible to write piecewise functions on types
  in same way it is possible to write piecewise functions on terms.

use with care : produce unintuitive results; e.g.,

guardUnit :: forall a. a -> Either String a
guardUnit x = case isUnit @a of
  True  -> Left "unit is not allowed"
  False -> Right x

even though IsUnit () marked overlapping, get an error:

error:
    • Overlapping instances for IsUnit a arising from a use of ‘isUnit’
      Matching instances:
        instance IsUnit a
        instance [overlapping] IsUnit ()
    • In the expression: isUnit @a

GHC does not what type a (i.e., what 'guardUnit' will be called with)
- This error is a good thing.

In this particular case, can do:
-}

guardUnit :: forall a. IsUnit a => a -> Either String a
guardUnit x = case isUnit @a of
  True  -> Left "unit is not allowed"
  False -> Right x

{-
------------------------------------------------------------------------------
Type families are functions from types to types

above, typeclasses as functions from types to terms

now : functions from types to types
-}

-- TypeFamilies
-- closed type family : similar to term level function def via pattern matching
type family Sum a b where
  Sum Z     b = b
  Sum (S a) b = S (Sum a b)

tSum :: [Test]
tSum  = U.t "tSum" (reifyNat @(Sum (S Z) (S (S Z)))) 3

{-
Type families enable computation at type-level during compile-time.
Result can be passed to typeclass method to produce a term-level value from the result.

Example 1: Generalized concat

1st practical TMP : flatten function (like provided by some dynamically-typed languages)
- like concat, but works on list of arbitrary depth, i.e., e.g.,

  flatten [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
          [1, 2, 3, 4, 5, 6, 7, 8]

Lists of different depths have different types in Haskell,
so multiple levels of concat have to be applied explicitly.
Using TMP, write a generic flatten function that operates on lists of any depth.
-}

-- obtain element type for list of any depth
type family ElementOf a where
  ElementOf [[a]] = ElementOf [a]
  ElementOf [a]   = a

class Flatten a where
  flatten :: a -> [ElementOf a]

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten x = x
--            ^
-- Couldn't match type ‘a’ with ‘ElementOf [a]’
-- (Needs type equality constraint to avoid this)

-- inductive case : nested list
-- Non type-variable argument in the constraint: Flatten [a]
--                        FlexibleContexts
--                             v
instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten x = flatten (concat x)
{-

Type equality constraint needed for first instance.
Why doesn’t GHC think a and ElementOf [a] are the same type?
Consider what would happen if we picked a type like [Int] for a.
Then [a] would be [[Int]], a nested list, so the first case of ElementOf would apply.
Therefore, GHC refuses to pick the second equation of ElementOf so hastily.

So use:

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten x = x

a ~ b : a must be same type as b

Type equality constraints useful with type families
- use to require a type family reduce to a certain type
- here : asserting ElementOf [a] must be a : enables instance to typecheck


-}

tflatten1 :: [Test]
tflatten1  = U.t "tflatten1" (flatten [ [ [1, 2], [3, 4]      ]
                                      , [ [5, 6], [7, 8::Int] ]
                                      ])
                             [1,2,3,4,5,6,7,8]
tflatten2 :: [Test]
tflatten2  = U.t "tflatten1" (flatten [ [ [ [1, 2, 3, 4     ] ] ]
                                      , [ [ [5, 6, 7, 8::Int] ] ]
                                      ])
                             [1,2,3,4,5,6,7,8]
{-
why Int type annotation?

from polymorphic nature of number literals

someone could define a Num [a] instance, in which case 1 could actually have a list type,
and either case of ElementOf could match depending on the choice of Num instance

flatten : example of what useful TMP can look like

------------------------------------------------------------------------------
Typeclasses as compile-time code generation

to think about Flatten as a function from types to terms
shift perspective to consider equivalent Flatten instances written using point-free style:

instance (ElementOf [a] ~ a) => Flatten [a] where
  flatten = id

instance {-# OVERLAPPING #-} Flatten [a] => Flatten [[a]] where
  flatten = flatten . concat

These defs no longer (syntactically) depend on term-level arguments.

Helps in considering what flatten might “expand to” given a type argument alone:

flatten @[Int] is id
- since Flatten [a] instance selected

flatten @[[Int]] is flatten @[Int] . concat
- since Flatten [[a]] instance is selected
- That then becomes id . concat, which is simplified to concat

flatten @[[[Int]]] is flatten @[[Int]] . concat
- simplifies to concat . concat

flatten @[[[[Int]]]] is concat . concat . concat

Each application of flatten takes a type as an argument
and produces some number of composed concats as a result.

Flatten is performing a kind of compile-time code generation,
synthesizing an expression to do the concatenation on the fly by inspecting the type information.

-- ===========================================================================
Part 2: Generic programming

Part 1 gave TMP foundational techniques.
Part 2 generic functions that operate on arbitrary datatypes

------------------------------------------------------------------------------
Open type families and associated types

open type families are functions from types to types (like closed type families)
- but not defined with a closed set of equations
- new equations added using type instance declarations
e.g.,
-}

type family   SumO a b
type instance SumO  Z    b = b
type instance SumO (S a) b = S (SumO a b)

{-
advantage of open type families is similar to the advantage of typeclasses
- new equations can be added at any time
  even in modules other than the one that declares the open type family

open type families used less for type-level computation
more for type-level maps that associate types with other types
e.g.,

-- relates types to the types used to index them

type family Key a
type instance Key (Vector a) = Int
type instance Key (Map k v) = k
type instance Key (Trie a) = ByteString

-- combined with a typeclass to provide a generic way
-- to see if a data structure contains a given key:
class HasKey a where
  hasKey :: Key a -> a -> Bool

instance HasKey (Vector a) where
  hasKey i vec = i >= 0 && i < Data.Vector.length vec

instance HasKey (Map k v) where
  hasKey = Data.Map.member

instance HasKey (Trie a) where
  hasKey = Data.Trie.member


This pattern so common that GHC provides option to make connection explicit
by defining them together: ASSOCIATED TYPES

class HasKey a where
  type Key a
  hasKey :: Key a -> a -> Bool

instance HasKey (Vector a) where
  type Key (Vector a) = Int
  hasKey i vec = i >= 0 && i < Data.Vector.length vec

instance HasKey (Map k v) where
  type Key (Map k v) = k
  hasKey = Data.Map.member

instance HasKey (Trie a) where
  type Key (Trie a) = ByteString
  hasKey = Data.Trie.member

open type families and associated types : useful for abstracting over similar types
with different structure : example MONO-TRAVERSABLE library (but not a TMP example)

------------------------------------------------------------------------------
Example 2: Datatype-generic programming

techniques for writing generic functions that operate on arbitrary data structures
- e.g., equality, comparison, hashing, (de)serialization
by exploiting structure of datatype definitions themselves

popular approach leverages GHC generics

will now show how to construct a simplified version of GHC generics that highlights key role of TMP

Generic datatype representations

all non-GADT Haskell datatypes can be represented as sums of products
-}

type Username  = String
type Password  = String
type PublicKey = String

data Authentication
  = AuthBasic Username Password
  | AuthSSH PublicKey

-- equivalent to:

type AuthenticationSOP = Either (Username, Password) PublicKey

{-
A function that can operate of a nested tree built out of Eithers and pairs,
can be used on any such datatype.

TMP : compile-time code generation based on type information.
Use same technique to generate impls of equality, comparison, etc. from
statically-known info about structure of a datatype.

example

generic function to counts number of fields stored in an arbitrary constructor
  numFields (AuthBasic "alyssa" "pass1234") return 2
  numFields (AuthSSH "<key>")               return 1

TMP implemenation
-}

class GNumFields a where
  gnumFields :: a -> Natural

-- base case: leaf value
instance GNumFields a where
  gnumFields _ = 1

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance {-# OVERLAPPING #-} (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left  a) = gnumFields a
  gnumFields (Right b) = gnumFields b

{-
like Flatten class, GNumFields uses type-level structure of arg to choose what to do:
- pair/product : recur into both sides and sum results
- Left/Right   : corresponds to "spine" differentiating different constructors
  - recur into contained value
- any other value : "leaf"  in Eithers/pair tree
  - corresponds to single field, so return 1
-}

-- needs IncoherentInstances
tgnumFieldsL :: [Test]
tgnumFieldsL  = U.t "tgnumFieldsL" (gnumFields (Left ("alyssa", "pass1234"))) 2
tgnumFieldsR :: [Test]
tgnumFieldsR  = U.t "tgnumFieldsR" (gnumFields (Right "<key>")) 1

{-
To use, need something like:

genericizeAuthentication :: Authentication -> Either (Username, Password) PublicKey
genericizeAuthentication (AuthBasic user pass) = Left (user, pass)
genericizeAuthentication (AuthSSH key)         = Right key

numFieldsAuthentication :: Authentication -> Natural
numFieldsAuthentication  = gnumFields . genericizeAuthentication

Need way to define a generic numFields that works on arbitrary datatypes
that implement this conversion to sums-of-products.
-}

class Generic a where
  type Rep a               -- maps 'a' into its SOP rep
  genericize :: a -> Rep a -- converts value to SOP rep

-- there is no magic (until this is derived automatically)
instance Generic Authentication where
  type Rep Authentication = Either (Username, Password) PublicKey
  genericize (AuthBasic user pass) = Left (user, pass)
  genericize (AuthSSH key)         = Right key

-- numFields works with any datatype that provides a Generic instance
-- where the magic happens
-- when applying numFields to a datatype
-- - Rep retrieves its generic, sums-of-products representation type
-- - GNumFields class uses TMP to generate a numFields impl on the fly from structure of Rep a
-- - that generated numFields impl is applied to genericized term-level value, and result produced
numFields :: (Generic a, GNumFields (Rep a)) => a -> Natural
numFields = gnumFields . genericize

tnumFieldsL :: [Test]
tnumFieldsL  = U.t "tnumFieldsL" (numFields (AuthBasic "alyssa" "pass1234")) 2
tnumFieldsR :: [Test]
tnumFieldsR  = U.t "tnumFieldsR" (numFields (AuthSSH "<key>")) 1

{-
value proposition of generic programming
- work up front to normalize datatype to a generic representation once
- get generic operations on it for free

------------------------------------------------------------------------------
Improving definition of Generic

Generic def above does not match GHC.Generic.

Distinguishing leaves from the spine

One problem with our version of Generic is that it provides no way to distinguish an Either or pair that should be considered a “leaf”, as in a type like this:

data Foo = A (Either Int String) | B (Char, Bool)
Given this type, Rep Foo should be Either (Either Int String) (Char, Bool), and numFields (Right ('a', True)) will erroneously return 2 rather than 1. To fix this, we can introduce a simple wrapper newtype that distinguishes leaves specifically:

newtype Leaf a = Leaf { getLeaf :: a }
Now our Generic instances look like this:

instance Generic Authentication where
  type Rep Authentication = Either (Leaf Username, Leaf Password) (Leaf PublicKey)
  genericize (AuthBasic user pass) = Left (Leaf user, Leaf pass)
  genericize (AuthSSH key)         = Right (Leaf key)

instance Generic Foo where
  type Rep Foo = Either (Leaf (Either Int String)) (Leaf (Char, Bool))
  genericize (A x) = Left (Leaf x)
  genericize (B x) = Right (Leaf x)
Since the Leaf constructor now distinguishes a leaf, rather than the absence of an Either or (,) constructor, we’ll have to update our GNumFields instances as well. However, this has the additional pleasant effect of eliminating the need for overlapping instances:

instance GNumFields (Leaf a) where  
  gnumFields _ = 1

instance (GNumFields a, GNumFields b) => GNumFields (a, b) where
  gnumFields (a, b) = gnumFields a + gnumFields b

instance (GNumFields a, GNumFields b) => GNumFields (Either a b) where
  gnumFields (Left a)  = gnumFields a
  gnumFields (Right b) = gnumFields b
This is a good example of why overlapping instances can be so seductive, but they often have unintended consequences. Even when doing TMP, explicit tags are almost always preferable.

Handling empty constructors
Suppose we have a type with nullary data constructors, like the standard Bool type:

data Bool = False | True
How do we write a Generic instance for Bool? Using just Either, (,), and Leaf, we can’t, but if we are willing to add a case for (), we can use it to denote nullary constructors:

instance GNumFields () where
  gnumFields _ = 0

instance Generic Bool where
  type Rep Bool = Either () ()
  genericize False = Left ()
  genericize True  = Right ()
In a similar vein, we could use Void to represent datatypes that don’t have any constructors at all.

Continuing from here
The full version of Generic has a variety of further improvements useful for generic programming, including:

Support for converting from Rep a to a.

Special indication of self-recursive datatypes, making generic tree traversals possible.

Type-level information about datatype constructor and record accessor names, allowing them to be used in serialization.

Fully automatic generation of Generic instances via the DeriveGeneric extension, which reduces the per-type boilerplate to essentially nothing.

The module documentation for GHC.Generics discusses the full system in detail, and it provides an additional example that uses the same essential TMP techniques discussed here.

Part 3: Dependent typing
It’s time for the third and final part of this blog post: an introduction to dependently typed programming in Haskell. A full treatment of dependently typed programming is far, far too vast to be contained in a single blog post, so I will not attempt to do so here. Rather, I will cover some basic idioms for doing dependent programming and highlight how TMP can be valuable when doing so.

Datatype promotion
In part 1, we used uninhabited datatypes like Z and S a to define new type-level constants. This works, but it is awkward. Imagine for a moment that we wanted to work with type-level booleans. Using our previous approach, we could define two empty datatypes, True and False:

data True
data False
Now we could define type families to provide operations on these types, such as Not:

type family Not a where
  Not True  = False
  Not False = True
However, this has some frustrating downsides:

First, it’s simply inconvenient that we have to define these new True and False “dummy” types, which are completely distinct from the Bool type provided by the prelude.

More significantly, it means Not has a very unhelpful kind:

ghci> :kind Not
Not :: * -> *
Even though Not is only supposed to be applied to True or False, its kind allows it to be applied to any type at all. You can see this in practice if you try to evaluate something like Not Char:

ghci> :kind! Not Char
Not Char :: *
= Not Char
Rather than getting an error, GHC simply spits Not Char back at us. This is a somewhat unintuitive property of closed type families: if none of the clauses match, the type family just gets “stuck,” not reducing any further. This can lead to very confusing type errors later in the typechecking process.

One way to think about Not is that it is largely dynamically kinded in the same way some languages are dynamically typed. That isn’t entirely true, as we technically will get a kind error if we try to apply Not to a type constructor rather than a type, such as Maybe:

ghci> :kind! Not Maybe

<interactive>:1:5: error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
…but * is still a very big kind, much bigger than we would like to permit for Not.

To help with both these problems, GHC provides datatype promotion via the DataKinds language extension. The idea is that for each normal, non-GADT type definition like

data Bool = False | True
then in addition to the normal type constructor and value constructors, GHC also defines several promoted constructors:

Bool is allowed as both a type and a kind.

'True and 'False are defined as new types of kind Bool.

We can see this in action if we remove our data True and data False declarations and adjust our definition of Not to use promoted constructors:

{-# LANGUAGE DataKinds #-}

type family Not a where
  Not 'True  = 'False
  Not 'False = 'True
Now the inferred kind of Not is no longer * -> *:

ghci> :kind Not
Not :: Bool -> Bool
Consequently, we will now get a kind error if we attempt to apply Not to anything other than 'True or 'False:

ghci> :kind! Not Char

<interactive>:1:5: error:
    • Expected kind ‘Bool’, but ‘Char’ has kind ‘*’
This is a nice improvement. We can make a similar change to our definitions involving type-level natural numbers:

data Nat = Z | S Nat

class ReifyNat (a :: Nat) where
  reifyNat :: Natural

instance ReifyNat 'Z where
  reifyNat = 0

instance ReifyNat a => ReifyNat ('S a) where
  reifyNat = 1 + reifyNat @a
Note that we need to add an explicit kind signature on the definition of the ReifyNat typeclass, since otherwise GHC will assume a has kind *, since nothing in the types of the typeclass methods suggests otherwise. In addition to making it clearer that Z and S are related, this prevents someone from coming along and defining a nonsensical instance like ReifyNat Char, which previously would have been allowed but will now be rejected with a kind error.

Datatype promotion is not strictly required to do TMP, but makes the process significantly less painful. It makes Haskell’s kind language extensible in the same way its type language is, which allows type-level programming to enjoy static typechecking (or more accurately, static kindchecking) in the same way term-level programming does.

GADTs and proof terms
So far in this blog post, we have discussed several different function-like things:

Ordinary Haskell functions are functions from terms to terms.

Type families are functions from types to types.

Typeclasses are functions from types to terms.

A curious reader may wonder about the existence of a fourth class of function:

??? are functions from terms to types.

To reason about what could go in the ??? above, we must consider what “a function from terms to types” would even mean. Functions from terms to terms and types to types are straightforward enough. Functions from types to terms are a little trickier, but they make intuitive sense: we use information known at compile-time to generate runtime behavior. But how could information possibly flow in the other direction? How could we possibly turn runtime information into compile-time information without being able to predict the future?

In general, we cannot. However, one feature of Haskell allows a restricted form of seemingly doing the impossible—turning runtime information into compile-time information—and that’s GADTs.

GADTs4 are described in detail in the GHC User’s Guide, but the key idea for our purposes is that pattern-matching on a GADT constructor can refine type information. Here’s a simple, silly example:

data WhatIsIt a where
  ABool :: WhatIsIt Bool
  AnInt :: WhatIsIt Int

doSomething :: WhatIsIt a -> a -> a
doSomething ABool x = not x
doSomething AnInt x = x + 1
Here, WhatIsIt is a datatype with two nullary constructors, ABool and AnInt, similar to a normal, non-GADT datatype like this one:

data WhatIsIt a = ABool | AnInt
What’s special about GADTs is that each constructor is given an explicit type signature. With the plain ADT definition above, ABool and AnInt would both have the type forall a. WhatIsIt a, but in the GADT definition, we explicitly fix a to Bool in the type of ABool and to Int in the type of AnInt.

This simple feature allows us to do very interesting things. The doSomething function is polymorphic in a, but on the right-hand side of the first equation, x has type Bool, while on the right-hand side of the second equation, x has type Int. This is because the WhatIsIt a argument effectively constrains the type of a, as we can see by experimenting with doSomething in GHCi:

ghci> doSomething ABool True
False
ghci> doSomething AnInt 10
11
ghci> doSomething AnInt True

error:
    • Couldn't match expected type ‘Int’ with actual type ‘Bool’
    • In the second argument of ‘doSomething’, namely ‘True’
      In the expression: doSomething AnInt True
      In an equation for ‘it’: it = doSomething AnInt True
One way to think about GADTs is as “proofs” or “witnesses” of type equalities. The ABool constructor is a proof of a ~ Bool, while the AnInt constructor is a proof of a ~ Int. When you construct ABool or AnInt, you must be able to satisfy the equality, and it is in a sense “packed into” the constructor value. When code pattern-matches on the constructor, the equality is “unpacked from” the value, and the equality becomes available on the right-hand side of the pattern match.

GADTs can be much more sophisticated than our simple WhatIsIt type above. Just like normal ADTs, GADT constructors can have parameters, which makes it possible to write inductive datatypes that carry type equality proofs with them:

infixr 5 `HCons`

data HList as where
  HNil  :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)
This type is a heterogenous list, a list that can contain elements of different types:

ghci> :t True `HCons` "hello" `HCons` 42 `HCons` HNil
True `HCons` "hello" `HCons` 42 `HCons` HNil
  :: Num a => HList '[Bool, [Char], a]
An HList is parameterized by a type-level list that keeps track of the types of its elements, which allows us to highlight another interesting property of GADTs: if we restrict that type information, the GHC pattern exhaustiveness checker will take the restriction into account. For example, we can write a completely total head function on HLists like this:

head :: HList (a ': as) -> a
head (x `HCons` _) = x
Remarkably, GHC does not complain that this definition of head is non-exhaustive. Since we specified that the argument must be of type HList (a ': as) in the type signature for head, GHC knows that the argument cannot be HNil (which would have the type HList '[]), so it doesn’t ask us to handle that case.

These examples illustrate the way GADTs serve as a general-purpose construct for relating type- and term-level information. Information flows bidirectionally: type information refines the set of type constructors that can be matched on, and matching on type constructors exposes new type equalities.

Proofs that work together
This interplay is wonderfully compositional. Suppose we wanted to write a function that accepts an HList of exactly 1, 2, or 3 elements. There’s no easy way to express that in the type signature the way we did with head, so it might seem like all we can do is write an entirely new container datatype that has three constructors, one for each case.

However, a more interesting solution exists that takes advantage of the bidirectional nature of GADTs. We can start by writing a proof term that contains no values, it just encapsulates type equalities on a type-level list:

data OneToThree a b c as where
  One   :: OneToThree a b c '[a]
  Two   :: OneToThree a b c '[a, b]
  Three :: OneToThree a b c '[a, b, c]
We call it a proof term because a value of type OneToThree a b c as constitutes a proof that as has exactly 1, 2, or 3 elements. Using OneToThree, we can write a function that accepts an HList accompanied by a proof term:

sumUpToThree :: OneToThree Int Int Int as -> HList as -> Int
sumUpToThree One   (x `HCons` HNil)                     = x
sumUpToThree Two   (x `HCons` y `HCons` HNil)           = x + y
sumUpToThree Three (x `HCons` y `HCons` z `HCons` HNil) = x + y + z
As with head, this function is completely exhaustive, in this case because we take full advantage of the bidirectional nature of GADTs:

When we match on the OneToThree proof term, information flows from the term level to the type level, refining the type of as in that branch.

The refined type of as then flows back down to the term level, restricting the shape the HList can take and refinine the set of patterns we have to match.

Of course, this example is not especially useful, but in general proof terms can encode any number of useful properties. For example, we can write a proof term that ensures an HList has an even number of elements:

data Even as where
  EvenNil  :: Even '[]
  EvenCons :: Even as -> Even (a ': b ': as)
This is a proof which itself has inductive structure: EvenCons takes a proof that as has an even number of elements and produces a proof that adding two more elements preserves the evenness. We can combine this with a type family to write a function that “pairs up” elements in an HList:

type family PairUp as where
  PairUp '[]            = '[]
  PairUp (a ': b ': as) = (a, b) ': PairUp as

pairUp :: Even as -> HList as -> HList (PairUp as)
pairUp EvenNil         HNil                     = HNil
pairUp (EvenCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp even xs
Once again, this definition is completely exhaustive, and we can show that it works in GHCi:

ghci> pairUp (EvenCons $ EvenCons EvenNil)
             (True `HCons` 'a' `HCons` () `HCons` "foo" `HCons` HNil)
(True,'a') `HCons` ((),"foo") `HCons` HNil
This ability to capture properties of a type using auxiliary proof terms, rather than having to define an entirely new type, is one of the things that makes dependently typed programming so powerful.

Proof inference
While our definition of pairUp is interesting, you may be skeptical of its practical utility. It’s fiddly and inconvenient to have to pass the Even proof term explicitly, since it must be updated every time the length of the list changes. Fortunately, this is where TMP comes in.

Remember that typeclasses are functions from types to terms. As its happens, a value of type Even as can be mechanically produced from the structure of the type as. This suggests that we could use TMP to automatically generate Even proofs, and indeed, we can. In fact, it’s not at all complicated:

class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenCons evenProof
We can now adjust our pairUp function to use IsEven instead of an explicit Even argument:

pairUp :: IsEven as => HList as -> HList (PairUp as)
pairUp = go evenProof where
  go :: Even as -> HList as -> HList (PairUp as)
  go EvenNil         HNil                     = HNil
  go (EvenCons even) (x `HCons` y `HCons` xs) = (x, y) `HCons` go even xs
This is essentially identical to its old definition, but by acquiring the proof via IsEven rather than passing it explicitly, we can call pairUp without having to construct a proof manually:

ghci> pairUp (True `HCons` 'a' `HCons` () `HCons` "foo" `HCons` HNil)
(True,'a') `HCons` ((),"foo") `HCons` HNil
This is rather remarkable. Using TMP, we are able to get GHC to automatically construct a proof that a list is even, with no programmer guidance beyond writing the IsEven typeclass. This relies once more on the perspective that typeclasses are functions that accept types and generate term-level code: IsEven is a function that accepts a type-level list and generates an Even proof term.

From this perspective, typeclasses are a way of specifying a proof search algorithm to the compiler. In the case of IsEven, the proofs being generated are rather simple, so the proof search algorithm is quite mechanical. But in general, typeclasses can be used to perform proof search of significant complexity, given a sufficiently clever encoding into the type system.

Aside: GADTs versus type families
Before moving on, I want to explicitly call attention to the relationship between GADTs and type families. Though at first glance they may seem markedly different, there are some similarities between the two, and sometimes they may be used to accomplish similar things.

Consider again the type of the pairUp function above (without the typeclass for simplicity):

pairUp :: Even as -> HList as -> HList (PairUp as)
We used both a GADT, Even, and a type family, PairUp. But we could have, in theory, used only a GADT and eliminated the type family altogether. Consider this variation on the Even proof term:

data EvenPairs as bs where
  EvenNil  :: EvenPairs '[] '[]
  EvenCons :: EvenPairs as bs -> EvenPairs (a ': b ': as) ((a, b) ': bs)
This type has two type parameters rather than one, and though there’s no distinction between the two from GHC’s point of view, it can be useful to think of as as an “input” parameter and bs as an “output” parameter. The idea is that any EvenPairs proof relates both an even-length list type and its paired up equivalent:

EvenNil has type EvenPairs '[] '[],

EvenCons EvenNil has type EvenPairs '[a, b] '[(a, b)],

EvenCons (EvenCons EvenNil) has type EvenPairs '[a, b, c, d] '[(a, b), (c, d)],

…and so on.

This allows us to reformulate our pairUp type signature this way:

pairUp :: EvenPairs as bs -> HList as -> HList bs
The definition is otherwise unchanged. The PairUp type family is completely gone, because now EvenPairs itself defines the relation. In this way, GADTs can be used like type-level functions!

The inverse, however, is not true, at least not directly: we cannot eliminate the GADT altogether and exclusively use type families. One way to attempt doing so would be to define a type family that returns a constraint rather than a type:

import Data.Kind (Constraint)

type family IsEvenTF as :: Constraint where
  IsEvenTF '[]            = ()
  IsEvenTF (_ ': _ ': as) = IsEvenTF as
The idea here is that IsEvenTF as produces a constraint can only be satisfied if as has an even number of elements, since that’s the only way it will eventually reduce to (), which in this case means the empty set of constraints, not the unit type (yes, the syntax for that is confusing). And in fact, it’s true that putting IsEvenTF as => in a type signature successfully restricts as to be an even-length list, but it doesn’t allow us to write pairUp. To see why, we can try the following definition:

pairUp :: IsEvenTF as => HList as -> HList (PairUp as)
pairUp HNil                     = HNil
pairUp (x `HCons` y `HCons` xs) = (x, y) `HCons` pairUp xs
Unlike the version using the GADT, this version of pairUp is not considered exhaustive:

warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for ‘pairUp’: Patterns not matched: HCons _ HNil
This is because type families don’t provide the same bidirectional flow of information that GADTs do, they’re only type-level functions. The constraint generated by IsEvenTF provides no term-level evidence about the shape of as, so we can’t branch on it the way we can branch on the Even GADT.5 (In a sense, IsEvenTF is doing validation, not parsing.)

For this reason, I caution against overuse of type families. Their simplicity is seductive, but all too often you pay for that simplicity with inflexibility. GADTs combined with TMP for proof inference can provide the best of both worlds: complete control over the term-level proof that gets generated while still letting the compiler do most of the work for you.

Guiding type inference
So far, this blog post has given relatively little attention to type inference. That is in some part a testament to the robustness of GHC’s type inference algorithm: even when fairly sophisticated TMP is involved, GHC often manages to propagate enough type information that type annotations are rarely needed.

However, when doing TMP, it would be irresponsible to not at least consider the type inference properties of programs. Type inference is what drives the whole typeclass resolution process to begin with, so poor type inference can easily make your fancy TMP construction next to useless. To take advantage of GHC to the fullest extent, programs should proactively guide the typechecker to help it infer as much as possible as often as possible.

To illustrate what that can look like, suppose we want to use TMP to generate an HList full of () values of an arbitrary length:

class UnitList as where
  unitList :: HList as

instance UnitList '[] where
  unitList = HNil

instance UnitList as => UnitList (() ': as) where
  unitList = () `HCons` unitList
Testing in GHCi, we can see it behaves as desired:

ghci> unitList :: HList '[(), (), ()]
() `HCons` () `HCons` () `HCons` HNil
Now suppose we write a function that accepts a list containing exactly one element and returns it:

unsingleton :: HList '[a] -> a
unsingleton (x `HCons` HNil) = x
Naturally, we would expect these to compose without a hitch. If we write unsingleton unitList, our TMP should generate a list of length 1, and we should get back (). However, it may surprise you to learn that isn’t, in fact, what happens:6

ghci> unsingleton unitList

error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘unitList’
      prevents the constraint ‘(UnitList '[a0])’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance UnitList as => UnitList (() : as)
What went wrong? The type error says that a0 is ambiguous, but it only lists a single matching UnitList instance—the one we want—so how can it be ambiguous which one to select?

The problem stems from the way we defined UnitList. When we wrote the instance

instance UnitList as => UnitList (() ': as) where
we said the first element of the type-level list must be (), so there’s nothing stopping someone from coming along and defining another instance:

instance UnitList as => UnitList (Int ': as) where
  unitList = 0 `HCons` unitList
In that case, GHC would have no way to know which instance to pick. Nothing in the type of unsingleton forces the element in the list to have type (), so both instances are equally valid. To hedge against this future possibility, GHC rejects the program as ambiguous from the start.

Of course, this isn’t what we want. The UnitList class is supposed to always return a list of () values, so how can we force GHC to pick our instance anyway? The answer is to play a trick:

instance (a ~ (), UnitList as) => UnitList (a ': as) where
  unitList = () `HCons` unitList
Here we’ve changed the instance so that it has the shape UnitList (a ': as), with a type variable in place of the (), but we also added an equality constraint that forces a to be (). Intuitively, you might think these two instances are completely identical, but in fact they are not! As proof, our example now typechecks:

ghci> unsingleton unitList
()
To understand why, it’s important to understand how GHC’s typeclass resolution algorithm works. Let’s start by establishing some terminology. Note that every instance declaration has the following shape:

instance <constraints> => C <types>
The part to the left of the => is known as the instance context, while the part to the right is known as the instance head. Now for the important bit: when GHC attempts to pick which typeclass instance to use to solve a typeclass constraint, only the instance head matters, and the instance context is completely ignored. Once GHC picks an instance, it commits to its choice, and only then does it consider the instance context.

This explains why our two UnitList instances behave differently:

Given the instance head UnitList (() ': as), GHC won’t select the instance unless it knows the first element of the list is ().

But given the instance head UnitList (a ': as), GHC will pick the instance regardless of the type of the first element. All that matters is that the list is at least one element long.

After the UnitList (a ': as) instance is selected, GHC attempts to solve the constraints in the instance context, including the a ~ () constraint. This forces a to be (), resolving the ambiguity and allowing type inference to proceed.

This distinction might seem excessively subtle, but in practice it is enormously useful. It means you, the programmer, have direct control over the type inference process:

If you put a type in the instance head, you’re asking GHC to figure out how to make the types match up by some other means. Sometimes that’s very useful, since perhaps you want that type to inform which instance to pick.

But if you put an equality constraint in the instance context, the roles are reversed: you’re saying to the compiler “you don’t tell me, I’ll tell you what type this is,” effectively giving you a role in type inference itself.

From this perspective, typeclass instances with equality constraints make GHC’s type inference algorithm extensible. You get to pick which decisions are made and when, and crucially, you can use knowledge of your own program structure to expose more information to the typechecker.

Given all of the above, consider again the definition of IsEven from earlier:

class IsEven as where
  evenProof :: Even as

instance IsEven '[] where
  evenProof = EvenNil

instance IsEven as => IsEven (a ': b ': as) where
  evenProof = EvenCons evenProof
Though it didn’t cause any problems in the examples we tried, this definition isn’t optimized for type inference. If GHC needed to solve an IsEven (a ': b0) constraint, where b0 is an ambiguous type variable, it would get stuck, since it doesn’t know that someone won’t come along and define an IsEven '[a] instance in the future.

To fix this, we can apply the same trick we used for UnitList, just in a slightly different way:

instance (as ~ (b ': bs), IsEven bs) => IsEven (a ': as) where
  evenProof = EvenCons evenProof
Again, the idea is to move the type information we learn from picking this instance into the instance context, allowing it to guide type inference rather than making type inference figure it out from some other source. Consistently applying this transformation can dramatically improve type inference in programs that make heavy use of TMP.

Example 3: Subtyping constraints
At last, we have reached the final example of this blog post. For this one, I have the pleasure of providing a real-world example from a production Haskell codebase: while I was working at Hasura, I had the opportunity to design an internal parser combinator library that captures aspects of the GraphQL type system. One such aspect of that type system is a form of subtyping; GraphQL essentially has two “kinds” of types—input types and output types—but some types can be used as both.

Haskell has no built-in support for subtyping, so most Haskell programs do their best to get away with parametric polymorphism instead. However, in our case, we actually need to distinguish (at runtime) types in the “both” category from those that are exclusively input or exclusively output types. Consequently, our GQLKind datatype has three cases:

data GQLKind
  = Both
  | Input
  | Output
We use DataKind-promoted versions of this GQLKind type as a parameter to a GQLType GADT:

data GQLType k where
  TScalar      :: GQLType 'Both
  TInputObject :: InputObjectInfo -> GQLType 'Input
  TIObject     :: ObjectInfo -> GQLType 'Output
  -- ...and so on...
This allows us to write functions that only accept input types or only accept output types, which is a wonderful property to be able to guarantee at compile-time! But there’s a problem: if we write a function that only accepts values of type GQLType 'Input, we can’t pass a GQLType 'Both, even though we really ought to be able to.

To fix this, we can use a little dependently typed programming. First, we’ll define a type to represent proof terms that witness a subkinding relationship:

data SubKind k1 k2 where
  KRefl :: SubKind k k
  KBoth :: SubKind 'Both k
The first case, KRefl, states that every kind is trivially a subkind of itself. The second case, KBoth, states that Both is a subkind of any kind at all. (This is a particularly literal example of using a type to define axioms.) The next step is to use TMP to implement proof inference:

class IsSubKind k1 k2 where
  subKindProof :: SubKind k1 k2

instance IsSubKind 'Both k where
  subKindProof = KBoth

instance (k ~ 'Input) => IsSubKind 'Input k where
  subKindProof = KRefl

instance (k ~ 'Output) => IsSubKind 'Output k where
  subKindProof = KRefl
These instances use the type equality trick described in the previous section to guide type inference, ensuring that if we ever need to prove that k is a superkind of 'Input or 'Output, type inference will force them to be equal.

Using IsSubKind, we can easily resolve the problem described above. Rather than write a function with a type like this:

nullable :: GQLParser 'Input a -> GQLParser 'Input (Maybe a)
…we simply use an IsSubKind constraint, instead:

nullable :: IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
Now both 'Input and 'Both kinds are accepted. In my experience, this caused no trouble at all for callers of these functions; everything worked completely automatically. Consuming the SubKind proofs was slightly more involved, but only ever so slightly. For example, we have a type family that looks like this:

type family ParserInput k where
  ParserInput 'Both   = InputValue
  ParserInput 'Input  = InputValue
  ParserInput 'Output = SelectionSet
This type family is used to determine what a GQLParser k a actually consumes as input, based on the kind of the GraphQL type it corresponds to. In some functions, we need to prove to GHC that IsSubKind k 'Input implies ParserInput k ~ InputValue.

Fortunately, that is very easy to do using the (:~:) type from Data.Type.Equality in base to capture a term-level witness of a type equality. It’s an ordinary Haskell GADT that happens to have an infix type constructor, and this is its definition:

data a :~: b where
  Refl :: a :~: a
Just as with any other GADT, (:~:) can be used to pack up type equalities and unpack them later; a :~: b just happens to be the GADT that corresponds precisely to the equality a ~ b. Using (:~:), we can write a reusable proof that IsSubKind k 'Input implies ParserInput k ~ InputValue:

inputParserInput :: forall k. IsSubKind k 'Input => ParserInput k :~: InputValue
inputParserInput = case subKindProof @k @'Input of
  KRefl -> Refl
  KBoth -> Refl
This function is a very simple proof by cases, where Refl can be read as “Q.E.D.”:

In the first case, matching on KRefl refines k to 'Input, and ParserInput 'Input is InputValue by definition of ParserInput.

Likewise, in the second case, matching on KBoth refines k to 'Both, and ParserInput 'Both is also InputValue by definition of ParserInput.

This inputParserInput helper allows functions like nullable, which internally need ParserInput k ~ InputValue, to take the form

nullable :: forall k a. IsSubKind k 'Input => GQLParser k a -> GQLParser k (Maybe a)
nullable parser = case inputParserInput @k of
  Refl -> {- ...implementation goes here... -}
Overall, this burden is quite minimal, so the additional type safety is more than worth the effort. The same could not be said without IsSubKind doing work to infer the proofs at each use site, so in this case, TMP has certainly paid its weight!

Wrapping up and closing thoughts
So concludes my introduction to Haskell TMP. As seems to happen all too often with my blog posts, this one has grown rather long, so allow me to provide a summary of the most important points:

Typeclass metaprogramming is a powerful technique for performing type-directed code generation, making it a form of “value inference” that infers values from types.

Unlike most other metaprogramming mechanisms, TMP has a wonderful synergy with type inference, which allows it to take advantage of information the programmer may not have even written explicitly.

Though I’ve called the technique “typeclass metaprogramming,” TMP really leverages the entirety of the modern GHC type system. Type families, GADTs, promoted types, and more all have their place in usefully applying type-level programming.

Finally, since TMP relies so heavily on type inference to do its job, it’s crucial to be thoughtful about how you design type-level code to give the typechecker as many opportunities to succeed as you possibly can.

The individual applications of TMP covered in this blog post—type-level computation, generic programming, and dependent typing—are all useful in their own right, and this post does not linger on any of them long enough to do any of them justice. That is, perhaps, the cost one pays when trying to discuss such an abstract, general technique. However, I hope that readers can see the forest for the trees and understand how TMP can be a set of techniques in their own right, applicable to the topics described above and more.

Readers may note that this blog post targets a slightly different audience than my other recent writing has been. That is a conscious choice: there is an unfortunate dearth of resources to help intermediate Haskell programmers become advanced Haskell programmers, in part because it’s hard to write them. The lack of resources makes tackling topics like this rather difficult, as too often it feels as though an entire web of concepts must be explained all at once, with no obvious incremental path that provides sufficient motivation every step of the way.

It remains to be seen whether my stab at the problem will be successful. But on the chance that it is, I suspect some readers will be curious about where to go next. Here are some ideas:

As mentioned earlier in this blog post, the GHC.Generics module documentation is a great resource if you want to explore generic programming further, and generic programming is a great way to put TMP to practical use.

I have long believed that the GHC User’s Guide is a criminally under-read and underappreciated piece of documentation. It is a treasure trove of knowledge, and I highly recommend reading through the sections on type-related language extensions if you want to get a better grasp of the mechanics of the Haskell type system.

Finally, if dependently typed programming in Haskell intrigues you, and you don’t mind staring into the sun, the singletons library provides abstractions and design patterns that can considerably cut down on the boilerplate. (Also, the accompanying paper is definitely worth a read if you’d like to go down that route.)

Even if you don’t decide to pursue type-level programming in Haskell, I hope this blog post helps make some of the concepts involved less mystical and intimidating. I, for one, think this stuff is worth the effort involved in understanding. After all, you never know when it might come in handy.

Not to be confused with C++’s template metaprogramming, though there are significant similarities between the two techniques. ↩

There have been proposals to introduce ordered instances, known in the literature as instance chains, but as of this writing, GHC does not implement them. ↩

Note that this also preserves an important property of the Haskell type system, parametricity. A function like id :: a -> a shouldn’t be allowed to do different things depending on which type is chosen for a, which our first version of guardUnit tried to violate. Typeclasses, being functions on types, can naturally do different things given different types, so a typeclass constraint is precisely what gives us the power to violate parametricity. ↩

Short for generalized algebraic datatypes, which is a rather unhelpful name for actually understanding what they are or what they’re for. ↩

If GHC allowed lightweight existential quantification, we could make that term-level evidence available with a sufficiently clever definition for IsEvenTF:

type family IsEvenTF as :: Constraint where
  IsEvenTF '[]       = ()
  IsEvenTF (a ': as) = exists b as'. (as ~ (b ': as'), IsEvenTF as')
The type refinement provided by matching on HCons would be enough for the second case of IsEvenTF to be selected, which would provide an equality proof that as has at least two elements. Sadly, GHC does not support anything of this sort, and it’s unclear if it would be tractable to implement at all. ↩

Actually, I’ve cheated a little bit here, because unsingleton unitList really does typecheck in GHCi under normal circumstances. That’s because the ExtendedDefaultRules extension is enabled in GHCi by default, which defaults ambiguous type variables to (), which happens to be exactly what’s needed to make this contrived example typecheck. However, that doesn’t say anything very useful, since the same expression really would fail to typecheck inside a Haskell module, so I’ve turned ExtendedDefaultRules off to illustrate the problem. ↩
-}
