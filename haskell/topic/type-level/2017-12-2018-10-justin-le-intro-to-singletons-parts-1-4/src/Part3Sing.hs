--{-# LANGUAGE DataKinds                      #-}
--{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE GADTs                          #-}
--{-# LANGUAGE InstanceSigs                   #-}
--{-# LANGUAGE KindSignatures                 #-}
--{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
--{-# LANGUAGE StandaloneDeriving             #-}
--{-# LANGUAGE TemplateHaskell                #-}
--{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
--{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}

module Part3Sing where

--import           Data.Kind
--import           Data.Singletons
--import           Data.Singletons.Prelude
--import           Data.Singletons.TH

------------------------------------------------------------------------------
--import           Data.Data
--import           Test.Hspec

{-
https://blog.jle.im/entry/introduction-to-singletons-3.html
Justin Le
Introduction to Singletons (Part 3)
October 1, 2018

------------------------------------------------------------------------------
Singletons Library to the Rescue

summary of manual:
- define type-level  function (StatePass)
- define value-level function (sStatePass)
  - like define singletons (SOpened, SClosed, etc.) at value level to mirror type level

Defining singletons and "singletonized functions: tedious, mechanical process.

singletons lib provides Template Haskell to automate

$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])

defines
- value-level function
- type family StatePass (s :: DoorState) :: Pass (like manual sStatePass)

naming convention
- for functions with non-symbol names : value myFunction
  - generates MyFunction and singleton function sMyFunction
- for functions with symbolic names (operators)
  - takes operator like ++
  - generates the type family ++ (keeping the identical name) and the singleton function %++

A Comparison

Previously : to restrict how functions can be called
- using phantom types with the singleton library and dependent types
- type-level functions

I consider using type-level functions, to be the more “mechanical” way
- less upfront cost in thinking time
- if term-level function possible, then can write a type-level function.
  - made simpler with singletons : write term-level, singletons generate type level

But term-level functions might be “incorrect”, and not verifiable.
- lift potentially incorrect term-level functions lifts the to incorrect type level

if dependently typed proofs used correctly better for safety

two approaches are not necessarily mutually exclusive, can mix the two.

Singleton Library Functions

working with type-level functions with singletons involves at least two parts
- type family working on the type-level values, and
- singleton functions mirroring the type family, working on the term-level singletons

singletons lib template haskell makes this seamless.
A good portion of Prelude and base is promoted and exported by singletons
- Data.Singletons.Prelude

fst :: (a, b) -> a
type family Fst (t :: (a, b)) :: a
sFst :: Sing t -> Sing (Fst t)


isLeft :: Either a b -> Bool
type family IsLeft (t :: Either a b) :: Bool
sIsLeft :: Sing t -> Sing (IsLeft t)

(++) :: [a] -> [a] -> [a]
type family (xs :: [a]) ++ (ys :: [a]) :: [a]
(%++) :: Sing xs -> Sing ys -> Sing (xs ++ ys)


Promoted Typeclasses

promote functions like (==) and max (i.e., typeclass-polymorphic)
- with kindclasses (typeclasses for kinds)


singletons lib provides the type families and the singleton functions
in in a separate typeclass

e.g.,

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

promoted as:

class PEq a where
    type (x :: a) == (y :: a) :: Bool       -- ^ associated type / type family
    type (x :: a) /= (y :: a) :: Bool

class SEq a where
    (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
    (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)

naming convention : add P for the “promoted” type family functions; S for singleton functions.

can promote custom typeclasses:

$(singletons [d|
  class MyClass a where
    myFunc :: a -> a
  |])

creates:
- typeclass MyClass with method myFunc :: MyClass a => a -> a
- promoted typeclass PMyClass with associated type/type family MyFunc (x :: a) :: a
- singletonized typeclass SMyClass with method sMyFunc :: Sing x -> Sing (MyFunc x).

Automatically Promoting Instance

singletons lib automatically promote instances (including derived ones)

$(singletons [d|
  data Pass = Obstruct | Allow

  instance Eq Pass where
      Obstruct == Obstruct = True
      Obstruct == Allow    = False
      Allow    == Obstruct = False
      Allow    == Allow    = True

      Obstruct /= Obstruct = True
      Obstruct /= Allow    = False
      Allow    /= Obstruct = False
      Allow    /= Allow    = True
  |])
generates PEq and SEq instances for Pass:

ghci> :kind! 'Obstruct == 'Obstruct
'True

ghci> SAllow %== SObstruct
SFalse

can also write:

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])


ghci> :kind! Show_ 'Obstruct      -- is named Show_ to not conflict with prelude Show
"Obstruct"

ghci> sMax SObstruct SAllow
SAllow

Next Step

ways to restrict the ways data types can be manipulated
- dependently typed proofs
- type level functions

phantom type
- used to restrict how functions called
- expressive what functions do
- benefit
  - how inputs relate to outputs

next post
- type-level functions to express complex relationships
- code re-use using higher-order functions via singleton’s defunctionalization system

Exercises

We talk about predicates as type constructors with type k -> Type. This fits a lot of things we’ve seen before (all instances of Functor, for example), but some predicates are more interesting than others.

What is the interpretation of SDoorState as a predicate? (remember, SDoorState s is the type synonym for Sing (s :: DoorState)) What “traditional” (that is, a -> Bool) predicate does it correspond to?

What is the type of its decision function? Can you implement it?

Solution available here!

Now let’s practice working with predicates, singletons, and negation via Refuted together.

You may have heard of the principle of “double negation”, where not (not p) implies p. So, we should be able to say that Refuted (Refuted (Knockable s)) implies Knockable s.8 If something is not “not knockable”, then it must be knockable, right?

Try writing refuteRefuteKnockable to verify this principle — at least for the Knockable predicate.

View full source
refuteRefuteKnockable
    :: forall s. SingI s
    => Refuted (Refuted (Knockable s))
    -> Knockable s
While not required, I recommend using isKnockable and writing your implementation in terms of it! Use sing to give isKnockable the singleton it needs.

Solution available here!

Hint: You might find absurd (from Data.Void) helpful:

absurd :: forall a. Void -> a
If you have a Void, you can make a value of any type!9

(This next one is fairly difficult compared to the others, and is only tangentially related to singletons, so feel free to skip it!)

Type-level predicates are logical constructs, so we should be able to define concepts like “and” and “or” with them.

Define a predicate constructor And that takes two predicates and returns a new predicate. This new predicate is true (aka, has an inhabitant) if and only if the two original predicates are true (aka, have inhabitants)

View full source
data And :: (k -> Type) -> (k -> Type) -> (k -> Type) where
Define a predicate constructor Or that takes two predicates and returns a new predicate. This new predicate is true (aka, has an inhabitant) if and only if at least one of the two original predicates are true (aka, have inhabitants)

View full source
data Or :: (k -> Type) -> (k -> Type) -> (k -> Type) where
There are potentially multiple non-trivial variations of this type.

Do And and Or look similar to any types you might have encountered in the past? Maybe, perhaps, similiar to types that are a part of basic beginner Haskell concepts?

Maybe surprisingly, And p q and Or p q are decidable if p and q are. Can we write the decision functions?

View full source
decideAnd
    :: (forall x. Sing x -> Decision (p x))
    -> (forall x. Sing x -> Decision (q x))
    -> Sing a
    -> Decision (And p q a)

decideOr
    :: (forall x. Sing x -> Decision (p x))
    -> (forall x. Sing x -> Decision (q x))
    -> Sing a
    -> Decision (Or p q a)
These functions actually demonstrate, I feel, why Decision having both a Proved a and Disproved (Refuted a) branch is very useful. This is because, if you wrote the structure of And and Or correctly, it’s impossible to incorrectly define decideAnd and decideOr. You can’t accidentally say false when it’s true, or true when it’s false — your implementation is guarunteed correct.

Now let’s use And and Or to prove some useful facts about Knockable and ('Opened :~:). We know that it’s impossible for something to be both Knockable and ('Opened :~:) (that is, both knockable and equal to 'Opened). Write such a witness:

View full source
knockableNotOpened
    :: forall s. SingI s
    => Refuted (And Knockable ((:~:) 'Opened) s)
We also know that a given DoorState is either Knockable or ('Opened :~:) — at least one of these is always true. Write such a witness:

View full source
knockableOrOpened
    :: forall s. SingI s
    => Or Knockable ((:~:) 'Opened) s
Solutions available here!

Instead of creating an entire Knocked type, we could have just said “as long as the door is not 'Opened, you can knock”. This means we could write knock as:

knock :: Refuted (s :~: 'Opened) -> Door s -> IO ()
Which we must pass a proof that s is not equal to 'Opened in order to open our door.

Is this really the same thing? Is Refuted (s :~: 'Opened) the same thing as Knockable s?

Let’s try to say that the two things are the same! Write the following functions to show that Refuted (s :~: 'Opened) is the same logical predicate as Knockable s!

View full source
knockedRefute
    :: forall s. SingI s
    => Knockable s
    -> Refuted (s :~: 'Opened)

refuteKnocked
    :: forall s. SingI s
    => Refuted (s :~: 'Opened)
    -> Knockable s
Solution available here!

Note: knockedRefute is fairly straightforward, but refuteKnocked is definitely trickier, so don’t be discouraged!

Hint: See the note about absurd from Exercise 2!

On our type level function version of knock, we wrote, with a constraint:

knock :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knock d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
We can muddy the waters a bit, for fun, by having this take a proof of the constraint instead:

View full source
knockRefl :: (StatePass s :~: 'Obstruct) -> Door s -> IO ()
knockRefl _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
Rewrite a version of knockSomeDoor in terms of knockRefl, called knockSomeDoorRefl:

View full source
knockSomeDoorRefl
    :: SomeDoor
    -> IO ()
knockSomeDoorRefl (MkSomeDoor s d) =
Remember not to use knock!

Solution available here.

Assume that DoorState has an instance of SDecide, so you can use (%~). This should be derived automatically as long as you derive Eq:

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])
With the function that inverts Pass:

$(singletons [d|
  invertPass :: Pass -> Pass
  invertPass Obstruct = Allow
  invertPass Allow    = Obstruct
|])
Implement knock in a way that lets you knock if invertPass is Allow:

View full source
knockInv :: (InvertPass (StatePass s) ~ 'Allow) => Door s -> IO ()
knockInv d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"
And write knockSomeDoor in terms of it:

View full source
knockSomeDoorInv
    :: SomeDoor
    -> IO ()
knockSomeDoorInv (MkSomeDoor s d) =
Remember again to implement it in terms of knockInv, not knock.

Solution available here!

Let’s work with a toy typeclass called Cycle, based on Enum

$(singletons [d|
  class Cycle a where
    next :: a -> a
    prev :: a -> a
  |])
next is like succ, but loops over to the first item after the last constructor. prev is like pred, but loops over to the last item if pred-ing the first item

View full source
instance Cycle DoorState where
    next Opened = Closed
    next Closed = Locked
    next Locked = Opened

    prev Opened = Locked
    prev Closed = Opened
    prev Locked = Closed
Can you manually promote this instance for DoorState to the type level?

View full source
instance PCycle DoorState where

instance SCycle DoorState where

Solution available here!
-}
