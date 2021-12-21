> {-# LANGUAGE DataKinds            #-}
> {-# LANGUAGE EmptyCase            #-}
> {-# LANGUAGE GADTs                #-}
> {-# LANGUAGE InstanceSigs         #-}
> {-# LANGUAGE LambdaCase           #-}
> {-# LANGUAGE RankNTypes           #-}
> {-# LANGUAGE ScopedTypeVariables  #-}
> {-# LANGUAGE StandaloneDeriving   #-}
> {-# LANGUAGE TemplateHaskell      #-}
> {-# LANGUAGE TypeFamilies         #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Part2 where
>
> import Data.Singletons
> import Data.Singletons.Prelude
> import Data.Singletons.TH
> import qualified GHC.Types as G

https://blog.jle.im/entry/introduction-to-singletons-2.html

Introduction to Singletons (Part 2)
Justin Le : Tuesday January 9, 2018

GHC 8.2.2; lts-10.0; singletons-2.3.1

derive
- type DoorState; values  Opened,  Closed, and  Locked
- kind DoorState; types  'Opened, 'Closed, and 'Locked
- singletons
- implicit-style typeclass instances
- reflectors, etc

> $(singletons [d|
>   data DoorState = Opened | Closed | Locked
>     deriving (Show, Eq)
>   |])

indexed by a type of kind DoorState
- picking a different type variable gives a different "type" of Door
- Door 'Opened is a type
- Door 'Closed is a type
- Door 'Locked is a type
- really are defining three distinct types

> data Door :: DoorState -> G.Type where
>   UnsafeMkDoor :: { doorMaterial :: String } -> Door s

but do not necessarily know if doors are opened, closed, or locked at compile-time

Ditching the Phantom

Sometimes do not care about state of door in the type of the door.

Need type to just represent a door, without the status in its type.

SomeDoor0
- instead of DoorState being type parameter, it is a runtime value

> data SomeDoor0 :: G.Type where
>   MkSomeDoor0 ::
>     { someDoorState0    :: DoorState
>     , someDoorMaterial0 :: String
>     } -> SomeDoor0

because SomeDoor0 is a different type, can’t re-use any Door functions.

The Existential Datatype

implement SomeDoor in terms of Door, using an existential data type:
- MkSomeDoor is constructor for existential data type
- data type "hides" type variable 's'
- result forgets 's'

> data SomeDoor :: G.Type where
>   MkSomeDoor :: Sing s -> Door s -> SomeDoor
>                         -- ^ data Door s = UnsafeMkDoor String

Since it uses the original Door s means can use Door functions on SomeDoors

> fromDoor :: Sing s -> Door s -> SomeDoor
> fromDoor = MkSomeDoor
>
> fromDoor_ :: SingI s => Door s -> SomeDoor
> fromDoor_ = fromDoor sing
>
> closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
> closeSomeOpenedDoor (MkSomeDoor s d) = case s of
>   SOpened -> Just . fromDoor_ $ closeDoor d
>   SClosed -> Nothing
>   SLocked -> Nothing
>
> closeDoor :: Door 'Opened -> Door 'Closed
> closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m
>
> lockAnySomeDoor :: SomeDoor -> SomeDoor
> lockAnySomeDoor (MkSomeDoor s d) = fromDoor_ $ lockAnyDoor s d
>
> lockDoor :: Door 'Closed -> Door 'Locked
> lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m
>
> lockAnyDoor :: Sing s -> Door s -> Door 'Locked
> lockAnyDoor = \case
>   -- dependent pattern match:
>   -- - pattern match on SingDSM s to reveal what s is, to the type checker
>   -- when s ~ 'OpenedM, say SOpenedM is a runtime witness to s being 'OpenedM
>   SOpened -> lockDoor . closeDoor  -- s is 'OpenedM
>   SClosed -> lockDoor              -- s is 'ClosedM
>   SLocked -> id                    -- s is 'LockedM

Using existential wrapper with singleton : unwrapping and rewrapping.

secret ingredient is 'Sing s' inside MkSomeDoor
- pattern matcher can then deduce 's' type

If MkSomeDoor did not have the Sing:

data SomeDoor where
    MkSomeDoor  :: Door s -> SomeDoor       -- no Sing s ???

closeSomeOpenedDoor :: SomeDoor -> Maybe SomeDoor
closeSomeOpenedDoor (MkSomeDoor d) =
            -- is the door opened, closed, or locked?
            -- there's no way to know!
            -- curses, type erasure!

The Link

having existentially quantified singleton : same as having value of corresponding type

having existentially quantified SingDS s  : same as having value of DoorState type

singletons library gives a direct existential wrapper

-- from singletons (not the actual definition)
data SomeSing DoorState :: Type where
    SomeSing :: Sing s -> SomeSing DoorState

with three values

SomeSing SOpened :: SomeSing DoorState
SomeSing SClosed :: SomeSing DoorState
SomeSing SLocked :: SomeSing DoorState

value of type SomeSing DoorState (which contains an existentially quantified Sing s – a SingDS)
- same as a value of type DoorState
- two types are isomorphic

Why do we sing?

why use a Sing or the new SomeDoor at all? Why not just use a DoorState value?

reasons
- code-reuse
- main: singleton lib enables recovering the type
- 'Sing s'
  - contains whether it is Opened/Closed/Locked
  - contains it in a way that GHC can use to bring it all back to the type level

forall s. SomeDoor (Sing s) (Door s)
- contains s with Door s
- read this as
  forall s. SomeDoor s (Door s)

see     : forall s. SomeDoor (Sing s) (Door s)
read as : forall s. SomeDoor       s  (Door s)

role of Sing s to be a run-time stand-in for the type s itself

need to know s at runtime
- storing Sing s gives GHC exactly that

Some Lingo

dependently typed programming vocabulary : SomeDoor is a dependent sum:

data SomeDoor
  = SDOpened (Door 'Opened)
  | SDClosed (Door 'Closed)
  | SDLocked (Door 'Locked)

also called a dependent pair : tuple  where
- type of second item (our Door s)
- is determined by value of first item (our Sing s)

Types at Runtime

make a door with status unknown until runtime:

> mkSomeDoor :: DoorState -> String -> SomeDoor
> mkSomeDoor = \case
>   Opened -> fromDoor_ . mkDoor SOpened
>   Closed -> fromDoor_ . mkDoor SClosed
>   Locked -> fromDoor_ . mkDoor SLocked

> mkDoor :: Sing s -> String -> Door s
> mkDoor = \case
>   SOpened -> UnsafeMkDoor
>   SClosed -> UnsafeMkDoor
>   SLocked -> UnsafeMkDoor

with mkSomeDoor: can pass in DoorState created at runtime

> mySomeDoor :: SomeDoor
> mySomeDoor  = mkSomeDoor Opened "Birch"
> pSomeDoor :: SomeDoor -> IO ()
> pSomeDoor d = putStrLn $ case d of
>   MkSomeDoor SOpened _ -> "mySomeDoor was opened!"
>   MkSomeDoor SClosed _ -> "mySomeDoor was closed!"
>   MkSomeDoor SLocked _ -> "mySomeDoor was locked!"

The Existential Type

existentially quantified type
- hidden from user/consumer
- chosen by producer

contrast : universally quantified type
- type chosen by user
- producer has to handle any possible type that the user asks for, e.g.,

    read :: Read a => String -> a

universally quantified over a
- caller picks type
- read implemention must be able to handle

another way to express existentially quantified type : CPS encoding
- does not require creating intermediate helper data type

mkSomeDoor
    :: DoorState
    -> String
    -> SomeDoor
mkSomeDoor s m = case s of
    Opened -> fromDoor_ (mkDoor SOpened m)
    Closed -> fromDoor_ (mkDoor SClosed m)
    Locked -> fromDoor_ (mkDoor SLocked m)

CPS-style:

> withDoor
>   :: DoorState
>   -> String
>   -> (forall s. Sing s -> Door s -> r) -> r  -- RankNTypes
> withDoor s m f = case s of
>   Opened -> f SOpened (mkDoor SOpened m)
>   Closed -> f SClosed (mkDoor SClosed m)
>   Locked -> f SLocked (mkDoor SLocked m)

caller of withDoor provide handler that can handle any s
- gives the result of the handler function called on the resulting Sing s and Door s
- handler function must be polymorphic over all possible 's'
- producer is "returning" an s – existentially quantified

> testWithDoor :: String
> testWithDoor = withDoor Opened "Birch" $ \s _d -> case s of
>   SOpened -> "Opened door!"
>   SClosed -> "Closed door!"
>   SLocked -> "Locked door!"

------------------------------------------------------------------------------
REIFICATION

reification : lifting runtime VALUE to type level as a TYPE
- opposite of REFLECTION

singletons lib generates functions to reify DoorState values:

-- reify DoorState as an existentially quantified data type
toSing       :: DoorState -> SomeSing DoorState

-- reify in CPS-style
withSomeSing :: DoorState -> (forall s. Sing s        -> r) -> r
withSomeSing :: DoorState -> (forall s. SDoorState s  -> r) -> r
                                     -- ^ using the convenience type synonym

use to rewrite mkSomeDoor and withDoor without pattern matching on constructors

> mkSomeDoorS :: DoorState -> String -> SomeDoor
> mkSomeDoorS ds = case toSing ds of
>   SomeSing s -> fromDoor s . mkDoor s
>
> withDoorS :: DoorState -> String -> (forall s. Sing s -> Door s -> r) -> r
> withDoorS ds m f = withSomeSing ds $ \s -> f s (mkDoor s m)

Zooming Out

main thing: indexed type:

    Sing :: Type -> Type

a runtime witness for s

SEE:

lockAnyDoor :: Sing s -> Door s -> Door 'Locked
MkSomeDoor  :: Sing s -> Door s -> SomeDoor

READ AS:

lockAnyDoor :: { s } -> Door s -> Door 'Locked
MkSomeDoor  :: { s } -> Door s -> SomeDoor

Sing is poly-kinded
 - can have Sing 'Opened; Sing 'True; Sing 5; Sing '['Just 3, 'Nothing, 'Just 0]; ...

SingI
- supports implicitly passing Sings to functions:

class SingI s where
    sing :: Sing s

SEE:

lockAnyDoor :: Sing  s -> Door s -> Door 'Locked
fromDoor    :: Sing  s -> Door s -> SomeDoor

FUNCATIONALLY SAME AS:

lockAnyDoor :: SingI s => Door s -> Door 'Locked
fromDoor    :: SingI s => Door s -> SomeDoor

passing in runtime witness on s

convert from 'SingI s ->' to 'SingI s =>' using sing:

lockAnyDoor_ :: SingI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor sing

fromDoor_ :: SingI s => Door s -> SomeDoor
fromDoor_ = fromDoor sing

convert from 'SingI s =>' to 'SingI s' -> using withSingI:

lockAnyDoor :: Sing s -> Door s -> Door 'Locked
lockAnyDoor s d = withSingI s (lockAnyDoor_ d)

fromDoor :: Sing s -> Door s -> SomeDoor
fromDoor s d = withSingI s (fromDoor_ d)

Reflection and Reification

Reflection : type-level thing to a value at term level
- "losing" the type information in the process
Reification : bringing value at the term level to the type level

Haskell limitation
- no actual link between the type DoorState and its values
- with the kind DoorState with its types
- constructors have same names, but language doesn’t actually link them together

SingKind

library handles using a typeclass with associated types to implement
a generalized reflection and reification process.
Gives the SingKind "kindclass":

class SingKind k where      -- `k` is a kind!
    -- | Associate a kind k with its reflected type
    type Demote k = (r :: Type)

    -- | Reflect a singleton to its term-level value
    fromSing :: Sing (a :: k) -> Demote k

    -- | Reify a term-level value to the type level, as an existentially
    -- quantified singleton
    toSing :: Demote k -> SomeSing k

SingKind instances are (promoted) kinds
- Bool-the-kind, DoorState-the-kind, etc.
- Demote is an associated type/type family
  - associates each instance with the type it is promoted from.
  - requires TypeInType extension : treat kinds as types

e.g, data DoorState = Opened | Closed | Locked
- created the type DoorState (with value constructors Opened, Closed, and Locked)
- also    the kind DoorState (with type constructors 'Opened, 'Closed, and 'Locked)
- kind DoorState would be the instance of SingKind,
- and Demote DoorState would be the type DoorState.

explicit Demote associated type is needed because GHC doesn’t actually link the type and its promoted kind.

Demote lets us explicitly specify what type a Kind should expect its term-level reflected values to be.

Example

generated instance of SingKind for the DoorState kind:

instance SingKind DoorState where       -- the *kind* DoorState
    type Demote DoorState = DoorState   -- the *type* DoorState

    fromSing
        :: Sing (s :: DoorState)        -- the *kind* DoorState
        -> DoorState                    -- the *type* DoorState
    fromSing = \case
        SOpened -> Opened
        SClosed -> Closed
        SLocked -> Locked

    toSing
        :: DoorState                    -- the *type* DoorState
        -> SomeSing DoorState           -- the *kind* DoorState
    toSing = \case
        Opened -> SomeSing SOpened
        Closed -> SomeSing SClosed
        Locked -> SomeSing SLocked

instance for Bool, to compare:

-- Bool singletons have two constructors:
SFalse :: Sing 'False
STrue  :: Sing 'True

instance SingKind Bool where    -- the *kind* Bool
    type Demote Bool = Bool     -- the *type* Bool

    fromSing
        :: Sing (b :: Bool)        -- the *kind* Bool
        -> Bool                    -- the *type* Bool
    fromSing = \case
        SFalse -> False
        STrue  -> True

    toSing
        :: Bool                    -- the *type* Bool
        -> SomeSing Bool           -- the *kind* Bool
    toSing = \case
        False -> SomeSing SFalse
        True  -> SomeSing STrue

instance for Maybe:

-- Maybe singletons have two constructors:
SNothing :: Sing 'Nothing
SJust    :: Sing x -> Sing ('Just x)

instance SingKind k => SingKind (Maybe k) where     -- the *kind* Maybe
    type Demote (Maybe k) = Maybe (Demote k)        -- the *type* Maybe

    fromSing
        :: Sing (m :: Maybe k)        -- the *kind* Maybe
        -> Maybe (Demote k)           -- the *type* Maybe
    fromSing = \case
        SNothing -> Nothing
        SJust sx -> Just (fromSing sx)

    toSing
        :: Maybe (Demote k)             -- the *type* Maybe
        -> SomeSing (Maybe k)           -- the *kind* Maybe
    toSing = \case
        Nothing -> SomeSing SNothing
        Just x  -> case toSing x of
          SomeSing sx -> SomeSing (SJust sx)

SomeSing

generic poly-kinded existential wrapper

data SomeSing :: Type -> Type where
    SomeSing :: Sing (x :: k) -> SomeSing k

says that SomeSing k contains a Sing x, where x is of kind k. This is why we had, earlier:

SomeSing :: Sing (s :: DoorState) -> SomeSing DoorState
SomeSing :: Sing (s :: Bool)      -> SomeSing Bool
SomeSing :: Sing (s :: Maybe k)   -> SomeSing (Maybe k)

If we use SomeSing with, say, SClosed, we get SomeSing :: Sing 'Closed -> SomeSing DoorState. SomeSing is an indexed type that tells us the kind of the type variable we existentially quantifying over. The value SomeSing STrue would have the type SomeSing Bool. The value SomeSing (SJust SClosed) would have the type SomeSing (Maybe DoorState).


