> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE TypeApplications #-}
>
> module S1A where
>
> import Data.Kind (Type)

https://blog.jle.im/entry/introduction-to-singletons-1.html

CodeJustin Le
by Justin Le : Friday December 22, 2017

Part 1 (this one)
- using singleton pattern for reflection
- show how singletons library helps
Part 2
- using library for reification : types that depend on values
Part 3
- basics of singleton’s defunctionalization system
- how to promote value-level functions to type-level functions
Part 4
- applications of defunctionalization.

Uses GHC 8.2.2 / lts-10.0 (i.e., singletons-2.3.1)

should work on GHC 8.0

------------------------------------------------------------------------------
phantom types : add layer of type safety for types and/or DSLs
- restrict values functions can take
- encode pre/post-conditions into types

> data Foo a = MkFoo -- a is phantom : declared on left, not used in right

    ghci> :t MkFoo :: Foo Int
    Foo Int
    ghci> :t MkFoo :: Foo Bool
    Foo Bool
    ghci> :t MkFoo :: Foo Either        -- requires -XPolyKinds
    Foo Either
    ghci> :t MkFoo :: Foo Monad         -- requires -XConstraintKinds
    Foo Monad

use case
- prohibit certain functions on different types of values
- more descriptive in how functions work together
  - e.g., https://ren.zone/articles/safe-money

> data DoorState = Opened | Closed | Locked deriving (Show, Eq)
>
> -- | phantom 's'
> newtype Door (s :: DoorState) = UnsafeMkDoor { doorMaterial :: String }

    UnsafeMkDoor "Oak"

DataKinds : create both
- type DoorState
  - value constructors Opened,  Closed, and  Locked
- kind DoorState
  - type constructors 'Opened, 'Closed, and 'Locked

    ghci> :k 'Opened
    DoorState
    ghci> :k 'Locked
    DoorState

phantom

    ghci> :t UnsafeMkDoor "Birch" :: Door 'Opened
    Door 'Opened
    ghci> :t UnsafeMkDoor "Iron" :: Door 'Locked
    Door 'Locked

    We can also use the TypeApplications extension to write this in a bit more convenient way –

    ghci> :t UnsafeMkDoor @'Opened "Birch"
    Door 'Opened
    ghci> :t UnsafeMkDoor @'Locked "Iron"
    Door 'Locked

> li = UnsafeMkDoor @'Locked "Iron"

Door using GADT syntax (specifies type of constructors).

    data Door :: DoorState -> Type where
      UnsafeMkDoor :: { doorMaterial :: String } -> Door s

Door is an indexed data type
- aka "type family" (in dependently typed programming world)
- (not to be confused with the -XTypeFamily language mechanic in GHC Haskell).

functions that expect a certain type of Door and/or return a specific type of Door:

encodes pre/post-conditions in type:

> closeDoor :: Door 'Opened -> Door 'Closed
> closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

    ghci> let myDoor = UnsafeMkDoor @'Opened "Spruce"
    ghci> :t myDoor
    Door 'Opened
    ghci> :t closeDoor myDoor
    Door 'Closed
    ghci> let yourDoor = UnsafeMkDoor @'Closed "Acacia"
    ghci> :t closeDoor yourDoor
    TYPE ERROR!  TYPE ERROR!

    ghci> :t closeDoor . closeDoor
    TYPE ERROR!  TYPE ERROR!  TYPE ERROR!

compositions that are type-checked to all be legal

> lockDoor :: Door 'Closed -> Door 'Locked
> lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m
>
> openDoor :: Door 'Closed -> Door 'Opened
> openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

    ghci> :t closeDoor . openDoor
    Door 'Closed -> Door 'Closed
    ghci> :t lockDoor . closeDoor . openDoor
    Door 'Closed -> Door 'Locked
    ghci> :t lockDoor . openDoor
    TYPE ERROR!  TYPE ERROR!  TYPE ERROR!

    ghci> let myDoor = UnsafeMkDoor @'Opened "Spruce"
    ghci> :t myDoor
    Door 'Opened
    ghci> :t lockDoor
    Door 'Closed -> Door 'Closed
    ghci> :t lockDoor myDoor
    TYPE ERROR!  TYPE ERROR!  TYPE ERROR!
    ghci> :t closeDoor myDoor
    Door 'Closed
    ghci> :t lockDoor (closeDoor myDoor)
    Door 'Locked

practical problems using phantom types this way

1. how to write fun to get state of a door?

    doorStatus :: Door s -> DoorState
    doorStatos _ = -- ?

can be done with typeclass, but not simple

2. create a Door with state not known until runtime?

    mkDoor :: DoorState -> String -> Door s
    mkDoor Opened = UnsafeMkDoor
    mkDoor Closed = UnsafeMkDoor
    mkDoor Locked = UnsafeMkDoor

But does not work
- for a polymorphic type forall s. DoorState -> String -> Door s
- the caller picks the type variable

    ghci> :t mkDoor Opened "Acacia" :: Door 'Closed
    Door 'Closed

hit on fundamental issue in Haskell’s type system: type erasure
- types only exist at compile-time
- great for performance

------------------------------------------------------------------------------
The Singleton Pattern

singleton
- type (of kind * - aka 'Type') that has ONE inhabitant
- refers to a parameterized type that, for each pick of parameter,
  gives a type with exactly one inhabitant.
- written so pattern matching on constructor of that value reveals the
  unique type parameter.

> data SingDS :: DoorState -> Type where
>   SOpened :: SingDS 'Opened
>   SClosed :: SingDS 'Closed
>   SLocked :: SingDS 'Locked

if use  SOpened         get   SingDS 'Opened
if have SingDS 'Opened, know  constructed using SOpened

The Power of the Pattern Match

can now pattern match on types

    closeDoor :: Door 'Opened -> Door 'Closed
    lockDoor :: Door 'Closed -> Door 'Locked

> -- | takes door of any state (a Door s of any s)
> --   locks it if necessary.
> -- Types ensure the s in SingDS s is the same s in the Door s.
> lockAnyDoor :: SingDS s -> (Door s -> Door 'Locked)
> lockAnyDoor = \case
>   -- dependent pattern match:
>   -- - pattern match on SingDS s to reveal what s is, to the type checker
>   -- when s ~ 'Opened, say SOpened is a runtime witness to s being 'Opened
>   SOpened -> lockDoor . closeDoor  -- in this branch, s is 'Opened
>   SClosed -> lockDoor              -- in this branch, s is 'Closed
>   SLocked -> id                    -- in this branch, s is 'Locked

singletons give runtime values to be used as witnesses for types and type variables.
- values "bypass" type erasure
- types are erased, but use runtime tokens to determine type

------------------------------------------------------------------------------
Reflection

> doorStatus0 :: SingDS s -> Door s -> DoorState
> doorStatus0 = \case
>   SOpened -> const Opened
>   SClosed -> const Closed
>   SLocked -> const Locked

singleton relies on fact that s in SingDS s is  same as s in Door s

Since door is not used, can write:

> doorStatus :: SingDS s -> Door s -> DoorState
> doorStatus s _ = fromSingDS s
>  where
>   fromSingDS :: SingDS s -> DoorState
>   fromSingDS = \case
>     SOpened -> Opened
>     SClosed -> Closed
>     SLocked -> Locked

REFLECTION: turning type variable into dynamic runtime value

------------------------------------------------------------------------------
Recovering Implicit Passing

downside : must pass in witness

use typeclasses to obviate

> class SingDSI s where
>   singDS :: SingDS s
>
> instance SingDSI 'Opened where
>   singDS = SOpened
> instance SingDSI 'Closed where
>   singDS = SClosed
> instance SingDSI 'Locked where
>   singDS = SLocked
>
> lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
> lockAnyDoor_ = lockAnyDoor singDS
>
> doorStatus_ :: SingDSI s => Door s -> DoorState
> doorStatus_ = doorStatus singDS

type inference tells GHC that singDS :: SingDS s is needed
- it will find the appropriate singleton

    ghci> let myDoor = UnsafeMkDoor @'Opened "Birch"
    ghci> :t lockAnyDoor SOpened myDoor -- our original method!
    Door 'Locked
    ghci> :t lockAnyDoor singDS myDoor  -- the power of type inference!
    Door 'Locked
    ghci> :t lockAnyDoor_ myDoor        -- no explicit singleton being passed!
    Door 'Locked

The constraint SingDSI s => is essentially same as passing in SingDS s explicitly.

implicit : SingDSI s =>
explicit : SingDS s ->

lockAnyDoor (explicit) and lockAnyDoor_ (implicit) are the "same function"

above: IMPLICIT to EXPLICIT : SingDSI s => to SingDS s ->

------------------------------------------------------------------------------

EXPLICIT to IMPLICIT : SingDS s -> to SingDSI s =>

typical done via CPS-like function:

To use x, SingDSI s must instance available.
- in each branch, s is now a specific, monomorphic, concrete s
- GHC knows that such an instance exists for every branch.

> -- | takes a SingDS s,
> --   a value of type r that requires a SingDSI s instance to be created.
> --   It creates that value.
> withSingDSI :: SingDS s -> (SingDSI s => r) -> r
> withSingDSI s x = case s of
>   -- s ~ 'Opened
>   -- explicitly wrote instance of SingDSI for 'Opened, and GHC "knows" it
>   -- so x available
>   SOpened -> x
>   SClosed -> x
>   SLocked -> x
>
> lockAnyDoor__ :: SingDS s -> Door s -> Door 'Locked
> lockAnyDoor__ s d = withSingDSI s (lockAnyDoor_ d)

nicer version of mkDoor using singletons:

> mkDoor :: SingDS s -> String -> Door s
> mkDoor = \case
>   SOpened -> UnsafeMkDoor
>   SClosed -> UnsafeMkDoor
>   SLocked -> UnsafeMkDoor

SingDS s "locks in" s type variable for Door s

    ghci> :t mkDoor SOpened "Oak"
    Door 'Opened
    ghci> :t mkDoor SLocked "Spruce"
    Door 'Locked
