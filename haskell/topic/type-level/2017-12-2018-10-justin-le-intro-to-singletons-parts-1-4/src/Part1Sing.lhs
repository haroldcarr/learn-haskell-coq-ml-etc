> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module Part1Sing where
>
> import Data.Singletons
> import Data.Singletons.Prelude
> import Data.Singletons.TH

https://blog.jle.im/entry/introduction-to-singletons-1.html

------------------------------------------------------------------------------
The Singletons Library

singletons library : unified, coherent system for working with singletons

Template Haskell generates singletons

> data DoorState = Opened | Closed | Locked deriving (Show, Eq)
>
> genSingletons [''DoorState]
> -- or
> -- $(singletons [d| data DoorState = Opened | Closed | Locked deriving (Show, Eq) |] )

generates (essentially):

    data Sing :: DoorState -> Type where
      SOpened :: Sing 'Opened
      SClosed :: Sing 'Closed
      SLocked :: Sing 'Locked

    :t SOpened
    => Sing 'Opened

Sing
- poly-kinded type constructor (a "data family")
- STrue :: Sing 'True is singleton for 'True
- SJust SOpened :: Sing ('Just 'Opened) is singleton for 'Just 'Opened
- etc

also generates instances for SingI (poly-kinded typeclass):

    instance SingI 'Opened where
      sing = SOpened
    instance SingI 'Closed where
      sing = SClosed
    instance SingI 'Locked where
      sing = SLocked

    :i Sing
    => data instance Sing z0 where
         SOpened :: z0 ~ 'Opened => Sing z0
         SClosed :: z0 ~ 'Closed => Sing z0
         SLocked :: z0 ~ 'Locked => Sing z0
           -- Defined at S1S.lhs:24:3

generates instances for singletons of all kinds
- SingI instance for 'True
- SingI instance for 10
- etc

    :t sing :: Sing 'True
    => Sing 'True
    :t sing :: Sing ('Just 'Opened)
    => Sing ('Just 'Opened)




generates : withSingI (equivalent to previous withSingDSMI):

       Justin:
       withSingI ::                      Sing s -> (forall r. SingI s => r) -> r

    :i withSingI
    => withSingI :: forall k (n :: k) r. Sing n ->           (SingI n => r) -> r
  	-- Defined in ‘Data.Singletons’

if singletons for kind       k exist, then
instances     for kind Maybe k exist too
instances     for kind [k]     ...

let singletons library generate singletons instead of writing them manually

    HC: DOES NOT WORK
    :t SOpened `SCons` SClosed `SCons` SLocked `SCons` SNil
    => Sing '[ 'Opened, 'Closed, 'Locked ]
    -- 'SCons : singleton for `:`
    -- 'SNil  : singleton for `[]`

DataKinds, so
- Maybe is a kind constructor
- with two type constructors
- type 'Nothing
- type constructor 'Just :: k -> Maybe k

besides generating singletons

provides functions for working with the different "manifestations" of our types

DoorState has
- type DoorState
  - value constructors :  Opened,  Closed,  Locked
- kind DoorState
  - type constructors  : 'Opened, 'Closed, 'Locked
- singletons for 'Opened, 'Closed, and 'Locked
  - SOpened :: Sing 'Opened
  - SClosed :: Sing 'Closed
  - SLocked :: Sing 'Locked
- SingI instances for 'Opened, 'Closed, and 'Locked'

in future, with real dependent types, all these manifestations will be one thing

for now, must deal with converting between them
- singletons generates

    -- REFLECTION: singletons to values
    fromSing :: Sing (s :: DoorState) -> DoorState

    fromSing SOpened
    => Opened

    :i fromSing
    => class SingKind k where
         ...
         fromSing :: forall (a :: k). Sing a -> DemoteRep k
         ...

above via
- defining type class (actually, a "kind class") : SingKind
- associating each type to corresponding DataKinds-generated kind
- SingKind instance for DoorState links type DoorState to kind DoorState

library generates type synonym

    type SDoorState = Sing

    :i SDoorState
    => type SDoorState =
         Sing :: DoorState -> ghc-prim-0.5.0.0:GHC.Types.Type
               -- Defined at S1S.lhs:24:3

so can do
- SDoorState 'Opened instead of
- Sing       'Opened

------------------------------------------------------------------------------
RECAP
- shortcomings in of using phantom types
- how singletons can help
- singletons library automates the pattern

You can see all of the “manual singletons” code in this post here, and then see the code re-implemented using the singletons library here.

How to create a Door with state not known runtime?

So far, only able to create Door and SingDS from types known at compile-time.

have not yet shown how to convert value level to type level : because of type erasure

Part 2 : will show how.

Part 3 : express more complicated relationships with types and type-level functions / type-level programming

original singletons paper : https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf

Exercise

write in singletons library style, with Sing instead of SingDS and SingI instead of SingDSI


1. Write a function to unlock a door, but only if the user enters an odd number (as a password).

    unlockDoor ::             Int -> Door 'Locked -> Maybe (Door 'Closed)

2. Write a function that can open any door, taking a password, in “implicit Sing” style:

    openAnyDoor :: SingI s => Int -> Door s       -> Maybe (Door 'Opened)

   write in terms of unlockDoor and openDoor (see above) – do not use UnsafeMkDoor directly for openAnyDoor

   If the door is already unlocked or opened, it should ignore the Int input.
