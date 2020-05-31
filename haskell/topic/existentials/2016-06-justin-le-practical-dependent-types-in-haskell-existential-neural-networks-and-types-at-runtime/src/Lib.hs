{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import           Control.Monad.Random
import           Data.Binary                  as B
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeLits
import           GHC.Generics                 (Generic)
import           GHC.Natural
import           Numeric.LinearAlgebra.Static
import           Prelude

{-# ANN module ("HLint: ignore Eta reduce" :: Prelude.String) #-}
{-# ANN module ("HLint: ignore Redundant lambda" :: Prelude.String) #-}

------------------------------------------------------------------------------
--import           Test.Hspec
------------------------------------------------------------------------------
{-
https://blog.jle.im/entry/practical-dependent-types-in-haskell-2.html

Practical Dependent Types in Haskell 2: Existential Neural Networks and Types at Runtime
by Justin Le ♦ Thursday June 30, 2016

using dependent types to write type-safe neural networks!

Part 1 dealt with types that are fixed at compile-time.

Part 2 types that depend runtime info
-}
randomWeights
  :: (MonadRandom m, KnownNat i, KnownNat o)
  => m (Weights i o)
randomWeights = do
  s1 :: Int <- getRandom
  s2 :: Int <- getRandom
  let wB = randomVector  s1 Uniform * 2 - 1
      wN = uniformSample s2 (-1) 1
  return $ W wB wN

runLayer
  :: (KnownNat i, KnownNat o)
  => Weights i o
  -> R i
  -> R o
runLayer (W wB wN) v = wB + wN #> v

-- * Network
--

data Network :: Nat -> [Nat] -> Nat -> * where
  O     :: !(Weights i o)
        -> Network i '[] o
  (:&~) :: KnownNat h
        => !(Weights i h)
        -> !(Network h hs o)
        -> Network i (h ': hs) o
infixr 5 :&~

deriving instance (KnownNat i, KnownNat o) => Show (Network i hs o)

logistic :: Floating a => a -> a
logistic x = 1 / (1 + exp (-x))

runNet
  :: (KnownNat i, KnownNat o)
  => Network i hs o
  -> R i
  -> R o
runNet (O w)      !v = logistic (runLayer w v)
runNet (w :&~ n') !v = let v' = logistic (runLayer w v)
                       in  runNet n' v'
{-
------------------------------------------------------------------------------
Types at Runtime

type for neural networks:

:k Network
Network :: Nat -> [Nat] -> Nat -> *
           ^      ^        ^
           |      |        size of the output vector
           |      list of hidden layer sizes
           size of input vector

above requires knowing entire structure of the network at compile-time

------------------------------------------------------------------------------
An Existential Crisis

two main ways to solve this issue : actually just two styles of doing the same thing.

------------------------------------------------------------------------------
Types hiding behind constructors

a user only needs to know input and output vectors, so Neural Network is R i -> R o

downside of having the structure in the type
- can’t store them in the same list or data structure
  Network 10 '[5,3] 1  and Network 10 '[5,2] 1

If written without the internal structure in the type then could:

data OpaqueNet i o

OpaqueNet a useful type for users
- only exposes types relevant to usage/API
- can store in list or MVar : [OpaqueNet 10 3] and MVar (OpaqueNet 10 3)
- serialize/deserialize

OpaqueNet as an "existential" wrapper over Network ('hs' is existential)
-}
data OpaqueNet :: Nat -> Nat -> * where
  ONet :: Network i hs o -> OpaqueNet i o
{-
Given        Network 6 '[10,6,3] 2
can create OpaqueNet 6           2
When using ONet constructor, the structure of the hidden layers disappears from the type.

Use the Network inside by pattern matching on ONet:
-}
runOpaqueNet
  :: (KnownNat i, KnownNat o)
  => OpaqueNet i o
  -> R i
  -> R o
runOpaqueNet (ONet n) x = runNet n x

numHiddens :: OpaqueNet i o -> Int
numHiddens (ONet n) = go n
 where
  go :: Network i hs o -> Int
  go  = \case
    O     _  -> 0
    _ :&~ n' -> 1 + go n'
{-
ScopedTypeVariables
- enables bringing hs back into scope:

case oN of
  ONet (n :: Network i hs o) -> ...

Above pattern is sometimes called dependent pair:
- pattern matching on ONet yields the hidden existentially quantified type : hs
  and also a value whose type is based on it (Network i hs o)
- like hs “paired” with Network i hs o

Pattern match on the results to give both the type 'hs' and the data structure.

(could have implemented it as ONet :: Sing hs -> Network i hs o -> OpaqueNet i o)

key to making this work
- after pattern matching on ONet, must handle 'hs' in a completely polymorphic way
- cannot assume anything about 'hs'

e.g., not ok:

bad :: OpaqueNet i o -> Network i hs o
bad (ONet n) = n

because the type signature means the caller can decide what 'hs' can be.
but the function has a specific 'hs' : the network that ONet hides.
The caller must accommodate whatever is inside ONet.

------------------------------------------------------------------------------
The Universal and the Existential

central to using existential types
- who has the power to decide what the types will be instantiated as

Most polymorphic functions in Haskell are "universally qualified", e.g.,

    map :: (a -> b) -> [a] -> [b]

the caller of map decides what a and b are

map is defined such that it will work for any a and b that the caller chooses

But for

foo :: [Int] -> OpaqueNet i o

caller of 'foo' chooses i and o
'foo' implementation chooses 'hs'
To use what 'foo' returns, must being able to handle anything 'foo'returns.

summary:

universal   : caller chooses
              function’s implementation must accommodate any choice

existential : function’s implementation chooses
              caller must accommodate any choice

earlier showed to Network i hs o inside the OpaqueNet i o
must deal with it in a parametrically polymorphic way (i.e., handle any 'hs')

------------------------------------------------------------------------------
A familiar friend

OpaqueNet i o  is a "dependent pair"
- pairs 'hs' with 'Network i hs o'

another common term : DEPENDENT SUM

"sum types" are Either-like types that can be one thing or another.

Dependent pairs/existential types actually are similar to Either/sum types

working with existential types is no different than working with Either

Given:

foo :: String -> Either Int Bool

then must handle the result for both cases.
- the function gets to pick which

(can view OpaqueNet i o as being an infinite Either over '[], '[1], '[1,2], ...)

handle Either via pattern matching
OpaqueNet i o is the same
- do not know the type of Network i hs o until pattern matching (a "dependent pattern match")

------------------------------------------------------------------------------
REIFICATION

re-write randomNet with explicit singleton input style
-}
randomNet'
  :: forall m i hs o. (MonadRandom m, KnownNat i, KnownNat o)
  => Sing hs -> m (Network i hs o)
randomNet' = \case
  SNil            ->     O <$> randomWeights
  SNat `SCons` ss -> (:&~) <$> randomWeights <*> randomNet' ss

randomNet
  :: forall m i hs o. (MonadRandom m, KnownNat i, SingI hs, KnownNat o)
  => m (Network i hs o)
randomNet = randomNet' sing
{-
use sing :: SingI hs => Sing hs to call the Sing hs ->-style function
from the SingI hs => one

still need to get the list of integers to the type level
- to create a Network i hs o
- to put into ONet

Use 'SomeSing' (a lot like OpaqueNet)
- wrapping the Sing a inside an existential data constructor
- toSing
  - takes term-level value (here [Integer])
  - returns 'SomeSing' wrapping the type-level value (here [Nat])
- pattern match on SomeSing constructor brings 'a' into scope

SomeSing implementation:

data SomeSing :: * -> * where
    SomeSing :: Sing (a :: k) -> SomeSing k

so,

foo :: SomeSing Bool
foo  = SomeSing STrue

bar :: SomeSing Nat
bar  = SomeSing (SNat :: Sing 10)
-}

mSing :: IO ()
mSing  = do
  putStrLn "How many cats do you own?"
  c <- readLn :: IO Natural
  case toSing c of SomeSing sc -> print (fromSing sc + 2)

xIn :: Int -> SomeSing Nat
xIn  = toSing . intToNatural

xOut :: SomeSing Nat -> Int
xOut  = \case SomeSing n -> naturalToInt (fromSing n)

xio :: Int -> Int
xio  = xOut . xIn
{-
inside the case statement branch the type n :: Nat is in scope.
pattern matching on the SNat constructor gives a KnownNat n instance

toSing can convert value x of type a to a singleton representing type x with kind a

Can use this to write randomONet:
-}
randomONet
  :: (MonadRandom m, KnownNat i, KnownNat o)
  => [Natural]
  -> m (OpaqueNet i o)
randomONet hs = case toSing hs of
  SomeSing ss -> ONet <$> randomNet' ss
{-
converting term-level value to type level type known as reification

original goal is now within reach:
-}
main :: IO ()
main  = do
  putStrLn "What hidden layer structure do you want?"
  hs <- readLn
  n  <- randomONet hs
  case n of
    ONet (net :: Network 10 hs 3) ->
      print net
      -- more stuff with dynamically generated net
{-
------------------------------------------------------------------------------
The Boundary

existentially quantified types (e.g., SomeSing),
enables ability to work with types that depend on runtime results

toSing and SomeSing is a BOUNDARY between untyped world and typed world
reification : cleanly separates the two

Reification boundary similar to boundary between pure code and impure (IO, etc.) code.

working with typed/untyped worlds as the same thing
- write as as possible in typed world
- write only what absolutely must be in untyped world
  (e.g., values from runtime environment)

------------------------------------------------------------------------------
Continuation-Based Existentials

another way to work with existentials that can be more natural to use

when pattern matching on existential, must work with ANY values in constructor

oNetToFoo :: OpaqueNet i o -> Foo
oNetToFoo (ONet n) = f n

f take 'Network i hs o' and work for all 'hs'

could skip the constructor and represent an existential type
as something taking the continuation f and giving it what it needs
-}

type OpaqueNet' i o r = (forall hs. Network i hs o -> r) -> r

{-
"Tell me how you would make an r
- given a 'Network i hs o' that works for any 'hs'
- then I will make it for you”

Uses Rank-N types

Using above is similar to using constructor-style:
-}
runOpaqueNet'
  :: (KnownNat i, KnownNat o)
  => OpaqueNet' i o (R o)
  -> R i
  -> R o
runOpaqueNet' oN x = oN (`runNet` x)
--            :: ((forall hs. Network i hs o -> R o) -> R o)
--            -> R i
--            -> R o

numHiddens' :: OpaqueNet' i o Int -> Int
numHiddens' oN = oN go
 where
  go :: Network i hs o -> Int
  go  = \case
    O     _  -> 0
    _ :&~ n' -> 1 + go n'
--          :: ((forall hs. Network i hs o -> Int) -> Int)
--          -> Int
{-
Above "continuation transformation" formally known as skolemization

wrap
-}

oNet' :: Network i hs o -> OpaqueNet' i o r
oNet' n = \f -> f n

-- randomONet that returns a continuation-style existential:

withRandomONet'0
  :: (MonadRandom m, KnownNat i, KnownNat o)
  => [Natural]
  -> (forall hs. Network i hs o -> m r)
  -> m r
-- aka
-- => [Integer]
-- -> OpaqueNet' i o (m r)
withRandomONet'0 hs f = case toSing hs of
  SomeSing ss -> do
    net <- randomNet' ss
    f net
{-
-- version of `toSing` that returns a skolemized `SomeSing`
withSomeSing :: [Integer]
             -> (forall (hs :: [Nat]). Sing hs -> r)
             -> r
-}

withRandomONet'
  :: (MonadRandom m, KnownNat i, KnownNat o)
  => [Natural]
  -> (forall hs. Network i hs o -> m r)
  -> m r
--aka,
-- => [Integer]
-- -> OpaqueNet' i o (m r)
withRandomONet' hs f = withSomeSing hs $ \ss -> do
  net <- randomNet' ss
  f net

main' :: IO ()
main' = do
  putStrLn "What hidden layer structure do you want?"
  hs <- readLn
  withRandomONet' hs $ \(net :: Network 10 hs 3) ->
    print net
    -- blah blah stuff with our dynamically generated net
{-
Just like the case statement pattern match represented the lexical boundary
between the untyped and typed world when using constructor-style existentials,
the ... $ \net -> ... is also a BOUNDARY for continuation-style existentials.

------------------------------------------------------------------------------
A Tale of Two Styles

two equivalent ways representing/working with existential types
can always convert between them

which one to use or offer can make a difference in code clarity

PROS/CONS

continuation-style
- does not require a data type/constructor
  - can use newtypes, but they force users to learn those types/constructors
    for each existential type returned
  - if there are many existentials, then that means a lot of un/wrapping
- easier to use when functions return existentials
  - especially if intended to immediately use them
    - saves an extraneous pattern match
- when using several existentials at once, continuation-style is better
  because each nested existential doesn’t force another level of indentation:

foo = withSomeSing x $ \sx ->
      withSomeSing y $ \sy ->
      withSomeSing z $ \sz ->
        -- ...
vs.

foo = case toSing x of
        SomeSing sx ->
          case toSing y of
            SomeSing sy ->
              case toSing z of
                SomeSing sz ->
                  -- ...
In monadic context, use do notation and ScopedTypeVariables for a non-nested style

main = do
  ONet n1 <- randomONet [7,5,3] :: IO (OpaqueNet 10 1)
  ONet n2 <- randomONet [5,5,5] :: IO (OpaqueNet 10 1)
  ONet n3 <- randomONet [5,4,3] :: IO (OpaqueNet 10 1)
  hs <- readLn
  ONet (n4 :: Network 10 hs 1) <- randomONet hs
    -- ...

nicer than

main = withRandomONet' [7,5,3] $ \n1 ->
       withRandomONet' [5,5,5] $ \n2 ->
       withRandomONet' [5,4,3] $ \n3 -> do
         hs <- readLn
         withRandomONet' hs $ \(n4 :: Network 10 hs 1) -> do
           -- ...
trick less useful for functions like toSing where things are not returned in a monad
You could wrap it in Identity, but that’s kind of silly:

foo = runIdentity $ do
        SomeSing sx <- Identity $ toSing x
        SomeSing sy <- Identity $ toSing y
        SomeSing sz <- Identity $ toSing z
        return $ -- ...

Constructor-style
- necessary for writing typeclass instances
  - i.e., cannowrite a Show instance for (forall hs. Network i hs o -> r) -> r
    but can write one for OpaqueNet i o
  - i.e., Binary de/serialization instances

Haskell does not support using Rank-N types as arguments to type constructors,
so [OpaqueNet i o] is possible,
but not [OpaqueNet' i o r] or [(forall hs. Network i hs o -> r) -> r]

MVar (OpaqueNet i o) is OK
but not MVar ((forall hs. Network i hs o -> r) -> r)

(latter are known as impredicative types)

If the type constructor is a Monad, you can use ContT-style skolemization,
like (forall hs. Network i hs o -> [r]) -> [r] and (forall hs. Network i hs o -> IO r) -> IO r.
But doesn’t work for MVar and other useful type constructors.

When writing functions that take existentials as inputs,
the constructor-style is arguably more natural. But barely.

For example, we wrote a function to find the number of hidden layers in a network earlier:

numHiddens :: OpaqueNet i o -> Int

But the continuation-style version has a slightly messier type:

numHiddens' :: ((forall hs. Network i hs o -> Int) -> Int)
            -> Int

Even with with the type synonym, it’s still a little awkward:

numHiddens' :: OpaqueNet' i o Int -> Int

This is why you’ll encounter many more functions returning continuation-style existentials
in libraries than taking them, for the most part.

------------------------------------------------------------------------------
Serializing Networks

apply existential types and reification to serialization

------------------------------------------------------------------------------
Recap on the Binary Library

Serializing networks whose sizes are statically in
their types — is straightforward --- an advantages of having sizes in types

binary library : typeclass-based de/serializing

monads (Get, Put) for describing serialization schemes and also a
typeclass used to provide serialization instructions for different
types.

usually do not write instances
use GHC generics:
-}
data Weights i o = W
  { wBiases :: !(R o)
  , wNodes  :: !(L o i)
  } deriving (Show, Generic)

instance (KnownNat i, KnownNat o) => Binary (Weights i o)
{-
gives get and put:

get :: (KnownNat i, KnownNat o) => Get (Weights i o)
put :: (KnownNat i, KnownNat o) => Weights i o -> Put
However, for GADTs like Network, we have to things manually.

------------------------------------------------------------------------------
Serializing Network
-}
putNet :: (KnownNat i, KnownNat o) => Network i hs o -> Put
putNet  = \case
  O w     -> put w
  w :&~ n -> put w *> putNet n
{-
Usually a flag is needed  to tell what constructor is being serialized,
so the deserializer can know what constructor to deserialize.
But need needed for Network because it is known know how many :&~ layers to expect
via the type: deserializing Network 5 '[10,6,3] 2 means three (:&~)’s and one O.
-}
getNet :: forall i hs o. (KnownNat i, KnownNat o) => Sing hs -> Get (Network i hs o)
getNet  = \case
  SNil            ->     O <$> get
  SNat `SCons` ss -> (:&~) <$> get <*> getNet ss
{-
getNet similar to randomNet': pattern match on 'hs' to get constructors
and ollow what the singleton’s structure says.

a Network Binary instance : cannot have get take a Sing hs input
- change the arity/type of the function
- switch to SingI-style and have their Binary instances require a SingI hs constraint.
-}
instance (KnownNat i, SingI hs, KnownNat o) => Binary (Network i hs o) where
  put = putNet
  get = getNet sing
{-
------------------------------------------------------------------------------
Serializating OpaqueNet

to de/serialize, must know complete structure

the complete structure is not in the type
- need to encode it as a flag so deserializer will which constructors
-}
hiddenStruct :: Network i hs o -> [Natural]
hiddenStruct = \case
    O _                           -> []
    _ :&~ (n' :: Network h hs' o) -> natVal (Proxy @h) : hiddenStruct n'

{-
natVal :: KnownNat n => Proxy n -> Natural
returns the value-level Natural corresponding to type-level n :: Nat

natVal and hiddenStruct : turn type-level infor ('n', 'hs') into term-level values

inverse of reification functions (like toSing)

REFECTION: type level to the value level
-}
putONet :: (KnownNat i, KnownNat o) => OpaqueNet i o -> Put
putONet (ONet net) = do
  put (hiddenStruct net) -- put the structure (as a binary flag)
  putNet net             -- then put the network itself

-- deserialize: load the list of Integers and reify it
getONet :: (KnownNat i, KnownNat o) => Get (OpaqueNet i o)
getONet = do
  hs <- get              -- load flag
  withSomeSing hs        -- reify it
    (fmap ONet . getNet) -- create it

instance (KnownNat i, KnownNat o) => Binary (OpaqueNet i o) where
  put = putONet
  get = getONet
{-
used constructor-style existential (instead of continuation-style)
because typeclass instances for the latter are not possible

------------------------------------------------------------------------------
An Existence For All

learned
- reification : untyped world to typed world to create tyypes depend on runtime values
- reflection : de/serialization shows type safety in dynamic context

------------------------------------------------------------------------------
Exercises

1. Implement putONet' and getONet' using continuation-style existentials
https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped2.hs#L188-L193
https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped2.hs#L195-L203

2. Work with an existential wrapper over the entire network structure (inputs and outputs, too):

data SomeNet where
  SNet :: (KnownNat i, KnownNat o)
       => Network i hs o
       -> SomeNet

(Need KnownNat because of type erasure, to recover original input/output dimensions
back on pattern matching)

And write:

- A function to convert SomeNets to OpaqueNets.
  Return OpaqueNet with existentially quantified i and o in continuation-style.
  (Also try a data type to return it in constructor-style)
https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped2.hs#L231-L234

- randomSNet, returning m SomeNet.
https://github.com/mstksg/inCode/tree/master/code-samples/dependent-haskell/NetworkTyped2.hs#L236-L245

While you’re at it, write it to return a random continuation-style SomeNet, too! (See the type of withRandomONet' for reference on how to write the type)

The binary instance for SomeNet.

Hint: Remember natVal :: KnownNat n => Proxy n -> Integer!

Hint: Remember that toSomeSing also works for Integers, to get Sings for Nats, too!

A bit of a stretch, because the set of all [Nat]s is non-enumerable and uncountable, but hopefully you get the picture!↩︎

Recall that I recommend (personally, and subjectively) a style where your external API functions and typeclass instances are implemented in SingI a => style, and your internal ones in Sing a -> style. This lets all of your internal functions fit together more nicely (Sing a -> style tends to be easier to write in, especially if you stay in it the entire time, because Sings are normal first-class values, unlike those global and magical typeclasses) while at the same time removing the burden of calling with explicit singletons from people using the functionality externally.↩︎

In older versions of singletons, before GHC 8 and TypeInType, we had to implement it using “kind proxies”. Don’t worry if you’re following along in 7.10; the basic usage of SomeSing is essentially identical either way.↩︎

Skolemization is probably one of the coolest words you’ll encounter when learning/using Haskell, and sometimes just knowing that you’re “skolemizing” something makes you feel cooler. Thank you Thoralf Skolem. If you ever see a “rigid, skolem” error in GHC, you can thank him for that too! He is also the inspiration behind my decision to name my first-born son Thoralf. (My second son’s name will be Curry)↩︎

It even lets you write Storable instances!↩︎
-}
