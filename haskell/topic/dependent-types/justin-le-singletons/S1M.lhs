> {-# LANGUAGE DataKinds        #-}
> {-# LANGUAGE GADTs            #-}
> {-# LANGUAGE KindSignatures   #-}
> {-# LANGUAGE LambdaCase       #-}
> {-# LANGUAGE RankNTypes       #-}
> {-# LANGUAGE TypeApplications #-}
>
> module S1M where
>
> import Data.Kind (Type)

HC NOTE: 'M' at the end means Manual.

https://blog.jle.im/entry/introduction-to-singletons-1.html

CodeJustin Le
by Justin Le : Friday December 22, 2017

Part 1 (this one)
- using singleton pattern for REFLECTION : TURNING TYPE VARIABLE INTO DYNAMIC RUNTIME VALUE
- show how singletons library helps
Part 2
- using library for REIFICATION : TYPES THAT DEPEND ON VALUES
Part 3
- basics of singleton’s defunctionalization system
- how to promote value-level functions to type-level functions
Part 4
- applications of defunctionalization.

Uses GHC 8.2.2 / lts-10.0 (i.e., singletons-2.3.1)

should work on GHC 8.0

------------------------------------------------------------------------------
phantom types : add additional type information

> data FooM a = MkFooM -- a is phantom : declared on left, not used in right

    :t MkFooM :: FooM Int
    => FooM Int
    :t MkFooM :: FooM Bool
    => FooM Bool
    :t MkFooM :: FooM Either        -- requires PolyKinds
    => FooM Either
    :t MkFooM :: FooM Monad         -- requires ConstraintKinds
    => FooM Monad

use case
- restrict functions to certain phantom types
- encode pre/post-conditions into types
  - more descriptive in how functions work together
    - e.g., https://ren.zone/articles/safe-money

> data DoorStateM = OpenedM | ClosedM | LockedM deriving (Eq, Show)
>
> -- | phantom 's'
> -- There are no values of type 's' in the data value (not even possible: there are no values of type 'SClosed, 'SOpened, etc.)
> newtype DoorM (s :: DoorStateM) = UnsafeMkDoorM { doorMaterialM :: String } deriving Show

DoorM using GADT syntax (specifies type of constructors).

    data DoorM :: DoorStateM -> Type where
      UnsafeMkDoorM :: { doorMaterialM :: String } -> DoorM s

DataKinds : create both
- type DoorStateM
  - value constructors OpenedM,  ClosedM, and  LockedM
- kind DoorStateM
  - type constructors 'OpenedM, 'ClosedM, and 'LockedM

    :k 'OpenedM
    => DoorStateM
    :k 'LockedM
    => DoorStateM

phantom

    :t UnsafeMkDoorM "Oak"
    => UnsafeMkDoorM "Oak" :: DoorM s

    :t UnsafeMkDoorM "Birch" :: DoorM 'OpenedM
    => DoorM 'OpenedM
    :t UnsafeMkDoorM "Iron" :: DoorM 'LockedM
    => DoorM 'LockedM

    TypeApplications extension to enables "passing" type:

    :t UnsafeMkDoorM @'OpenedM "Birch"
    => DoorM 'OpenedM
    :t UnsafeMkDoorM @'LockedM "Iron"
    => DoorM 'LockedM

DoorM is an indexed data type
- aka "type family" (in dependently typed programming vocabulary)
- (not to be confused with the -XTypeFamily language mechanic in GHC)

functions that expect and/or return a specific type of DoorM

encodes pre/post-conditions in type

> closeDoorM :: DoorM 'OpenedM -> DoorM 'ClosedM
> closeDoorM (UnsafeMkDoorM m) = UnsafeMkDoorM m

> myDoorM = UnsafeMkDoorM @'OpenedM "Spruce"

    :t myDoorM
    => DoorM 'OpenedM
    :t closeDoorM myDoorM
    => DoorM 'ClosedM

> yourDoorM = UnsafeMkDoorM @'ClosedM "Acacia"

    :t closeDoorM yourDoorM
    =>  Expected type: DoorM 'OpenedM
          Actual type: DoorM 'ClosedM

    :t closeDoorM . closeDoorM
    => Expected type: DoorM 'OpenedM -> DoorM 'OpenedM
         Actual type: DoorM 'OpenedM -> DoorM 'ClosedM

compositions are type-checked

> lockDoorM :: DoorM 'ClosedM -> DoorM 'LockedM
> lockDoorM (UnsafeMkDoorM m) = UnsafeMkDoorM m
>
> openDoorM :: DoorM 'ClosedM -> DoorM 'OpenedM
> openDoorM (UnsafeMkDoorM m) = UnsafeMkDoorM m

    :t closeDoorM . openDoorM
    => DoorM 'ClosedM -> DoorM 'ClosedM
    :t lockDoorM . closeDoorM . openDoorM
    => DoorM 'ClosedM -> DoorM 'LockedM
    :t lockDoorM . openDoorM
    => Expected type: DoorM 'ClosedM -> DoorM 'ClosedM
         Actual type: DoorM 'ClosedM -> DoorM 'OpenedM

> myDoorM2 = UnsafeMkDoorM @'OpenedM "Spruce"

    :t myDoorM2
    => DoorM 'OpenedM
    :t lockDoorM
    => DoorM 'ClosedM -> DoorM 'ClosedM
    :t lockDoorM myDoorM2
    => Expected type: DoorM 'ClosedM
         Actual type: DoorM 'OpenedM
    :t closeDoorM myDoorM2
    => DoorM 'ClosedM
    :t lockDoorM (closeDoorM myDoorM2)
    => DoorM 'LockedM

What can NOT be done with phantom types alone:

1. how to write fun to get state of a door (REFLECTION: turn type var into runtime val)?

    doorStatusM :: DoorM s -> DoorStateM
    doorStatusM _ = ?

can be done with typeclass, but not simple

2. create a DoorM with state not known until runtime (REIFICATION: type that depends on value)?

NOTE: stuff on mkDoorM in this file only works on things known at compiletime.

This does not work:

    mkDoorM :: DoorStateM -> String -> DoorM s
    mkDoorM OpenedM = UnsafeMkDoorM
    mkDoorM ClosedM = UnsafeMkDoorM
    mkDoorM LockedM = UnsafeMkDoorM

for a polymorphic type : forall s. DoorStateM -> String -> DoorM s
- the caller picks the type variable

    :t mkDoorM OpenedM "Acacia" :: DoorM 'ClosedM
    => DoorM 'ClosedM

because of design of Haskell’s type system: type erasure
- types only exist at compile-time (great for performance)

------------------------------------------------------------------------------
The Singleton Pattern

singleton
- type (of kind * - aka 'Type') that has ONE inhabitant
- refers to a parameterized type that
  - for each pick of parameter
    - gives a type with exactly ONE INHABITANT (i.e., "singleton")
- written so pattern matching on constructor of that value
  reveals the unique/single type parameter

singletons give runtime values to be used as witnesses for types and type variables.
- values "bypass" type erasure
- types are erased, but use runtime tokens to determine type

> data SingDSM :: DoorStateM -> Type where
>   SOpenedM :: SingDSM 'OpenedM
>   SClosedM :: SingDSM 'ClosedM
>   SLockedM :: SingDSM 'LockedM

if use  SOpenedM         constructor,  then get   type              SingDSM 'OpenedM
if have SingDSM 'OpenedM type,         then know  constructed using SOpenedM

can now pattern match on types

    closeDoorM :: DoorM 'OpenedM -> DoorM 'ClosedM
    lockDoorM  :: DoorM 'ClosedM -> DoorM 'LockedM

> -- | takes door of any state (a 'DoorM s' of any 's')
> --   locks it if necessary.
> -- types ensure the 's' in 'SingDSM s' is the same 's' in the 'DoorM s'
> lockAnyDoorM :: SingDSM s -> DoorM s -> DoorM 'LockedM
> lockAnyDoorM = \case
>   -- dependent pattern match:
>   -- - pattern match on SingDSM s to reveal what s is, to the type checker
>   -- when s ~ 'OpenedM, say SOpenedM is a runtime witness to s being 'OpenedM
>   SOpenedM -> lockDoorM . closeDoorM  -- s is 'OpenedM
>   SClosedM -> lockDoorM               -- s is 'ClosedM
>   SLockedM -> id                      -- s is 'LockedM

------------------------------------------------------------------------------
REFLECTION: turning type variable into dynamic runtime value

singleton relies on fact that s in SingDSM s is  same as s in DoorM s

> doorStatusM :: SingDSM s -> DoorM s -> DoorStateM
> doorStatusM = \case
>   SOpenedM -> const OpenedM
>   SClosedM -> const ClosedM
>   SLockedM -> const LockedM

    doorStatusM SOpenedM myDoorM
    => OpenedM

downside : must explicitly give witness argument

------------------------------------------------------------------------------
Implicit Passing

use typeclasses to obviate

> class SingDSMI s where
>   singDS :: SingDSM s
>
> instance SingDSMI 'OpenedM where
>   singDS = SOpenedM
> instance SingDSMI 'ClosedM where
>   singDS = SClosedM
> instance SingDSMI 'LockedM where
>   singDS = SLockedM
>
> lockAnyDoorM_ :: SingDSMI s => DoorM s -> DoorM 'LockedM
> lockAnyDoorM_ = lockAnyDoorM singDS
>
> doorStatusM_ :: SingDSMI s => DoorM s -> DoorStateM
> doorStatusM_ = doorStatusM singDS

type inference says singDS :: SingDSM s is needed
- compiler finds appropriate singleton

> myDoorM3 = UnsafeMkDoorM @'OpenedM "Birch"

    -- original method
    :t lockAnyDoorM SOpenedM myDoorM

    -- type inference
    :t lockAnyDoorM singDS   myDoorM

    -- no explicit singleton passed
    :t lockAnyDoorM_         myDoorM

    all return
    => DoorM 'LockedM

constraint 'SingDSMI s =>' : essentially same as passing 'SingDSM s' explicitly

implicit : SingDSMI s =>
explicit : SingDSM  s ->

"same function":
- lockAnyDoorM  (explicit)
- lockAnyDoorM_ (implicit)

above : IMPLICIT to EXPLICIT : SingDSMI s => to SingDSM  s ->

------------------------------------------------------------------------------

here  : EXPLICIT to IMPLICIT : SingDSM  s -> to SingDSMI s =>

typical done via CPS-like function:

To use x, SingDSMI s instance must be available.
- in each branch, s is now a specific, monomorphic, concrete s
- GHC knows that such an instance exists for every branch.

> -- | takes SingDSM s
> --   a value of type r that requires a SingDSMI s instance to be created
> --   It creates that value.
> withSingDSMI :: SingDSM s -> (SingDSMI s => r) -> r
> withSingDSMI s x = case s of
>   -- s ~ 'OpenedM
>   -- explicitly wrote instance of SingDSMI for 'OpenedM, and GHC "knows" it
>   -- so x available
>   SOpenedM -> x
>   SClosedM -> x
>   SLockedM -> x
>
> lockAnyDoorM__ :: SingDSM s -> DoorM s -> DoorM 'LockedM
> lockAnyDoorM__ s d = withSingDSMI s (lockAnyDoorM_ d)

nicer version of mkDoorM using singletons:

> mkDoorM :: SingDSM s -> String -> DoorM s
> mkDoorM = \case
>   SOpenedM -> UnsafeMkDoorM
>   SClosedM -> UnsafeMkDoorM
>   SLockedM -> UnsafeMkDoorM

SingDSM s "locks in" s type variable for DoorM s

    :t mkDoorM SOpenedM "Oak"
    => DoorM 'OpenedM
    :t mkDoorM SLockedM "Spruce"
    => DoorM 'LockedM
