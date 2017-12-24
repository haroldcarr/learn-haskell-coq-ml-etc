> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE TypeFamilies #-}
>
> module S1B where
>
> import Data.Singletons
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

generates instances for singletons of all kinds!
- SingI instance for 'True
- SingI instance for 10
- etc

    ghci> sing :: Sing 'True
    STrue
    ghci> sing :: Sing ('Just 'Opened)
    SJust SOpened

generates : withSingI (equivalent to previous withSingDSI):

withSingI :: Sing s -> (forall r. SingI s => r) -> r

Note that if you have singletons for a kind k, you also have instances for kind Maybe k, as well. And also for [k], even! The fact that we have a unified way of working with and manipulating singletons of so many different types is a major advantage of using the singletons library to manage your singletons instead of writing them yourself.

ghci> :t SOpened `SCons` SClosed `SCons` SLocked `SCons` SNil
Sing '[ 'Opened, 'Closed, 'Locked ]
-- 'SCons is the singleton for `:` (cons),
-- and 'SNil is the singleton for `[]` (nil)

(Remember that, because of DataKinds, Maybe is a kind constructor, who has two type constructors, the type 'Nothing and the type constructor 'Just :: k -> Maybe k)

Singletons for all kinds integrate together seamlessly, and you have mechanisms to generate them for your own type and roll it all into the system!
Extra Goodiestop

In addition to generating singletons for our libraries, it gives us convenient functions for working with the different “manifestations” of our types.

Recall that DoorState has four different things associated with it now:

    The type DoorState, whose value constructors are Opened, Closed, and Locked.
    The kind DoorState, whose type constructors are 'Opened, 'Closed, and 'Locked

    The singletons for 'Opened, 'Closed, and 'Locked:

    SOpened :: Sing 'Opened
    SClosed :: Sing 'Closed
    SLocked :: Sing 'Locked

    The SingI instances for 'Opened, 'Closed, and 'Locked'

Kind of confusing, and in the future, when we have real dependent types, we can combine all of these manifestations into the one thing. But for now, we do have to deal with converting between them, and for that, singletons generates for us fromSing :: Sing (s :: DoorState) -> DoorState. fromSing takes us from singletons to values (reflection):

ghci> fromSing SOpened
Opened

It does this by defining a type class (actually, a “kind class”), SingKind, associating each type to the corresponding datakinds-generated kind. The SingKind instance for DoorState links the type DoorState to the kind DoorState.

The library also defines a neat type synonym, type SDoorState = Sing, so you can do SDoorState 'Opened instead of Sing 'Opened, if you wish.

There are definitely more useful utility functions, but we will investigate these later on in the series! For now, you can look at the documentation for the library to see more interesting utility functions!
The Singularitytop

In this post, at shortcomings in the usage of phantom types, and then saw how singletons could help us with these. Then, we looked at how the singletons library makes using this pattern extremely easy and smooth to integrate into your existing code.

You can see all of the “manual singletons” code in this post here, and then see the code re-implemented using the singletons library here.

However, remember the question that I asked earlier, about creating a Door with a given state that we don’t know until runtime? So far, we are only able to create Door and SingDS from types we know at compile-time. There is no way we have yet to convert a DoorState from the value level to the type level – so it seems that there is no way to “load” a Door s with an s that depends on, say, a file’s contents, or user input. The fundamental issue is still type erasure.

In Part 2, we will delve into how to overcome this and break through from the barrier of the dynamic “unsafe” runtime to the world of safe, typed, verified code, and see how the singletons library gives us great tools for this. Afterwards, in Part 3, we will learn to express more complicated relationships with types and type-level functions using defunctionalization and the tools from the singletons library, and finally break into the world of actual “type-level programming”.

As always, let me know in the comments if you have any questions! You can also usually find me idling on the freenode #haskell channel, as well, as jle`. The singletons issue tracker is also very active. Happy haskelling!

For further reading, check out the original singletons paper! It’s very readable and goes over many of the same techniques in this blog post, just written with a different perspective and tone :)
Exercisestop

Click on the links in the corner of the text boxes for solutions! (or just check out the source file)

These should be written in the singletons library style, with Sing instead of SingDS and SingI instead of SingDSI. Review the singletons file for a comparison, if you are still unfamiliar.

    Write a function to unlock a door, but only if the user enters an odd number (as a password).

    View full source
    unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)

    It should return a closed door in Just if the caller gives an odd number, or Nothing otherwise.

    Write a function that can open any door, taking a password, in “implicit Sing” style:

    View full source
    openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)

    This should be written in terms of unlockDoor and openDoor (see above) – that is, you should not use UnsafeMkDoor directly for openAnyDoor.

    If the door is already unlocked or opened, it should ignore the Int input.

    This series will be based on a talk I gave over the summer, and will expand on it.↩

    The ' ticks are technically optional, but I find that it’s good style, at this point in Haskell, to use them whenever you can. It’ll prevent a lot of confusion, trust me!↩

    Indeed, this is not even possible. There are no values of type 'SClosed, 'SOpened, etc.↩

    Actually, GADT syntax just requires -XGADTSyntax, but -XGADT allows you to actually make GADTs (which we will be doing later), and implies -XGADTSyntax↩

    ~ here refers to “type equality”, or the constraint that the types on both sides are equal. s ~ 'Opened can be read as “s is 'Opened”.↩

    This entry is a part of a series called "Introduction to Singletons". Find the rest of the entries in this series at its series history.

    #dependent types
    #functional programming
    #haskell
    #singletons
    #types
    @HASKELL
    +Introduction to Singletons

submit to reddit

    ← Advent of Code 2017! Ongoing solutions and explanations (Previous)

© 2016 Justin Le

        TwitterGoogle+LinkedInGithubKeybaseBitcoin
        RSSMailing list
