--{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
--{-# LANGUAGE KindSignatures                 #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeInType                     #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}

module Part3Manual where

import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TH

------------------------------------------------------------------------------
--import           Data.Data
--import           Test.Hspec

{-
https://blog.jle.im/entry/introduction-to-singletons-3.html
Justin Le
Introduction to Singletons (Part 3)
October 1, 2018

part 3 of journey through singleton design pattern and library

continuation of Part 1 and Part 2

expands by working with more complex ways to restrict functions based on types

Like previous, start by writing “by hand” then use singletons library

1st half : introduce new application and design pattern that singletons enhances

2nd half : lifting functions to type level

GHC 8.6.1
nightly-2018-09-29 (singletons-2.5)

src for post : https://github.com/mstksg/inCode/tree/master/code-samples/singletons/Door3.hs

In Part1 : Door type, indexed with a phantom DoorState
-}

$(singletons [d|
  data DoorState = Opened | Closed | Locked
    deriving (Show, Eq)
  |])

data Door :: DoorState -> Type where
    UnsafeMkDoor :: { doorMaterial :: String } -> Door s

mkDoor :: Sing s -> String -> Door s
mkDoor _ = UnsafeMkDoor

{-
3 distinct types
- Door 'Opened
- Door 'Closed
- Door 'Locked

use for
- enforcing type-safety
- relating inputs to outputs
- uniting functions polymorphic on all door states.

situations where do “not care” about door status in type system, or

return a door with state determined at runtime.

after going through many “analogous” / equivalent type, arrived at existential wrapper:
-}

data SomeDoor :: Type where
  MkSomeDoor :: Sing s -> Door s -> SomeDoor

mkSomeDoor :: DoorState -> String -> SomeDoor
mkSomeDoor ds mat = withSomeSing ds $ \dsSing -> MkSomeDoor dsSing (mkDoor dsSing mat)

{-
pack the Sing s with the Door s, to enable pattern match at runtime to determine original s

In this post, SomeDoor used in place of a Door s with state (the s) determined at runtime

A Need for More Expressive Restrictionstop
-}

knock0 :: Door s -> IO ()
knock0 d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

{-
But allows knocking on opened door.  Want restrictions on functions.

two ways of doing this
- dependently typed proofs
- type level functions

------------------------------------------------------------------------------
Dependently Typed Proofs

application of singletons and DataKinds

prove an operation is legal

Proofs are witnesses to type-level predicates.

value-level predicate is function         of type: a -> Bool
- given value of type a, return True if predicate satisfied

type-level  predicate is type constructor of kind: k -> Type
- given type  of kind k, if a value exists of that type (or, if a value can be constructed),
  then predicate is satisfied
- that value called a witness or a proof
-}

-- predicate only has values if given 'Closed and 'Locked
data Knockable :: DoorState -> Type where
  KnockClosed :: Knockable 'Closed
  KnockLocked :: Knockable 'Locked

knock :: Knockable s -> Door s -> IO ()
knock _ d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

{-
No way to call above with s ~ 'Opened beuase value of Knockable 'Opened does not exist

works for things done at compile-time

Can make it easier to use use by auto-generating proofs at compile-time:
-}

class Proved p a where
  auto :: p a

instance Proved Knockable 'Closed where
  auto = KnockClosed

instance Proved Knockable 'Locked where
  auto = KnockLocked

{-

ghci> knock auto (mkDoor SClosed "Acacia")
Knock knock on Acacia door!

This typeclass exists in libraries
- type-combinators : called Known
- decidable        : called Auto

Decidable Predicates

above only works if s known at compile-time.

To do at runtime, use property of predicates called “decidability”
Predicate is decidable if, for any input type, can say if predicate is satisfiable or not.

-- defined in singletons
-- | The data type with no values
data Void

-- defined in singletons
-- | 'a' cannot exist.  Commonly also called `Not`
-- type Refuted a = a -> Void (defined in singletons)

-- defined in singletons
-- data Decision a = Proved a                  -- ^ a value of a exists
--                 | Disproved (Refuted a)     -- ^ a value of a cannot exist

usage

decidePred
  :: Sing x               -- ^ given a type
  -> Decision (P x)       -- ^ return a decision
decidePred = _

Decision a like Maybe a, except instead of Nothing, return proof the predicate not true.

a -> Void
- represents fact it is impossible to construct a value of type a.
-}

isKnockable :: Sing s -> Decision (Knockable s)
isKnockable = \case
  SOpened -> Disproved $ \case {}    -- s ~ 'Opened
  SClosed -> Proved KnockClosed      -- s ~ 'Closed
  SLocked -> Proved KnockLocked      -- s ~ 'Locked

{-
straightforward for SClosed and SLocked branches

But SOpened branch cannot produce a type Knockable 'Opened.
So disprove it: by providing a function of type Refuted (Knockable 'Opened)
                            or type Knockable 'Opened -> Void
e.g.,

disproveOpened :: Knockable 'Opened -> Void
disproveOpened k = case k of {}             -- empty pattern match

How does this work?

There is NO VALID PATTERN MATCH, therefore : \case {}
Works because disproveOpened is a complete pattern match, and therefore total.

Use decidable to for Door whose status not know until runtime:
-}

knockSomeDoor
  :: SomeDoor     -- ^ status not known until you pattern match at runtime
  -> IO ()
knockSomeDoor (MkSomeDoor s d) = case isKnockable s of
  Proved k    -> knock k d
  Disproved _ -> putStrLn "No knocking allowed!"

{-
typeclass for decidable predicates in "decidable" package called Decidable
- less useful than one provable predicates

Decision data definition : why care if NOT true?

use Disproved in practice (in knockSomeDoor) but threw away counter-proof
- some situations use contents of Disproved

important to use -Werror=incomplete-patterns when writing dependently typed proofs

Perspective on Proofs

knock :: Knockable s -> Door s -> IO ()
- Knocking requires both a Door s and a proof that the door’s status is Knockable”.

Can think of proofs as “compiler tricks” : exist to appease the compiler.
- and they are erased (never exist at runtime) in Agda, Coq, Idris

GHC 8.6 does not implement proof erasure at the time of this post.

The Role of Singletons

Proofs do not play a role at run-time, but must generating/deciding them
requires pattern match on them at run-time, therefore singletons

isKnockable :: Sing a -> Decision (Knockable a)
- Sing enables isKnockable to pattern match 'a'a to proof

Singletons lib provides tools for working with proofs and decisions.
- Data.Singletons.Decide

re-exports propositional equality:

data (:~:) :: k -> k -> Type where
    Refl :: a :~: a

Only constructor is when left is equal to right side.
Can use Refl with type application syntax, e.g., Refl @'Blah, to be clear

Also has “kindclass” SDecide : decision functions for the (a :~:) predicate:

class SDecide k where
  (%~) :: Sing (a :: k)
       -> Sing (b :: k)
       -> Decision (a :~: b)

example usage:

(STrue %~) :: Sing b -> Decision ('True :~: b)

check if b is equal to 'True. (SDecide like a type-level Eq typeclass for “type equality”.)

------------------------------------------------------------------------------
Type Level Functions

different method for restricting how functions can be gcdetails_elapsed_ns

define type : expresses knockable-or-not-knockable, as a value:
-}

$(singletons [d|
  data Pass = Obstruct | Allow
  |])

-- type-level function (implemented as type family) from DoorState to a Pass
type family StatePass (s :: DoorState) :: Pass where
  StatePass 'Opened = 'Allow
  StatePass 'Closed = 'Obstruct
  StatePass 'Locked = 'Obstruct

{-
type families : type-level fun : take types as arg; return types

ghci> :kind! StatePass 'Opened
'Allow
ghci> :kind! StatePass 'Closed
'Obstruct

:kind  : the kind of a type expression
:kind! : evaluate type families in type expressions

type families cannot be partially applied (“unsaturated”).
-}

-- note: GHC saying constraint redundant (but it is NOT)
knockP :: (StatePass s ~ 'Obstruct) => Door s -> IO ()
knockP d = putStrLn $ "Knock knock on " ++ doorMaterial d ++ " door!"

{-
a ~ b : constraint for type equality.

constraint requires StatePass s to be equal to (or unify with) 'Obstruct

let door1 = mkDoor SClosed "Oak"
let door2 = mkDoor SOpened "Spruce"
knockP door1
-- Knock knock on Oak door!
knockP door2
--     • Couldn't match type ‘'Allow’ with ‘'Obstruct’
--             arising from a use of ‘knock’

Deciding at Runtime

if s known at compile-time,can call knockP without manual proofs.

if s not known until runtime : prove to compiler that Passable s is 'Allow.

pass s (singleton representing a type) to StatePass (the type family).
then match on the resulting type
- function that takes a singleton of s; returns a singleton of StatePass s
- mirror of type-level function at value level

-- already defined above
type family StatePass (s :: DoorState) :: Pass where
    StatePass 'Opened = 'Allow
    StatePass 'Closed = 'Obstruct
    StatePass 'Locked = 'Obstruct
-}

sStatePass :: Sing s -> Sing (StatePass s)
sStatePass = \case
    SOpened -> SAllow
    SClosed -> SObstruct
    SLocked -> SObstruct

-- GHC will reject any def that is not structurally identical to the type family it’s mirroring.

knockSomeDoorP
  :: SomeDoor     -- ^ status not known until you pattern match at runtime
  -> IO ()
knockSomeDoorP (MkSomeDoor s d) = case sStatePass s of
  SObstruct -> knockP d                        -- StatePass s ~ 'Obstruct
  SAllow    -> putStrLn "No knocking allowed!" -- StatePass s ~ 'Allow

{-
-- ---------------------------------------------------------------------------
Singletons Library to the Rescue

At the high level, we defined a “function” on types (StatePass), using type families.

And, just like we have to define singletons (SOpened, SClosed, etc.) at the value level to mirror what is happening at the type level, we also have to define singleton functions (sStatePass) at the value level to mirror what is happening at the type level.

Defining singletons for our types is a tedious and mechanical process. Defining singletonized functions for our type families is also similarly tedious and mechanical. This is where the singletons library comes in: it provides us Template Haskell tools to automatically define type families and their associated singleton functions:

$(singletons [d|
  statePass :: DoorState -> Pass
  statePass Opened = Allow
  statePass Closed = Obstruct
  statePass Locked = Obstruct
  |])
The above declaration would normally declare only the value-level function statePass with the type DoorSate -> Pass.

However, with singleton’s template haskell, this also generates:6

The type family StatePass (s :: DoorState) :: Pass, like we defined above
The singleton function sStatePass, with the type Sing s -> Sing (StatePass s), like we defined above.
The naming convention for functions with non-symbol names takes a function like myFunction and generates the type family MyFunction and the singleton function sMyFunction.

The naming convention for functions with symbolic names (operators) takes an operator like ++ and generates the type family ++ (keeping the identical name) and the singleton function %++.7

A Comparisontop
We went over two methods of using phantom types with the singleton library and dependent types to restrict how certain functions can be called, on a more non-trivial level.

Our first method was leveraging “dependently typed proofs”. These are useful because they are constructed to exploit the “structure” of the types you create. Essentially, we create a data type (predicate) in a way so that it is impossible to create an “invalid” proof. And, often, if we write our proofs in a clever enough way, we can actually use and combine proofs to generate new proofs. (More examples in the exercises)

Personally, I find this to be the source of a lot of the “fun” of dependently typed programming — our proofs become first class values, and if we define them in a nice enough way, we can use manipulate them to create new proofs. (A full exploration of this is well beyond the scope of this post)

However, in practice, carefully constructing predicates and proofs (ones more complicated than the one we just looked at) requires some up-front cost in thinking about how to best express your predicate, and is sometimes not straightforward.

I consider the second method, using type-level functions, to be the more “mechanical” way, with less upfront cost in thinking time. For the most part, if you can write a normal term-level function (something that most Haskell programmers are comfortable doing), you can write a type-level function. This is even made simpler with singletons — you can just write your term-level relationship as a normal function, and you can now just directly use your function at the type level.

In fact, consider if there were more than two Pass (maybe allow, obstruct, or partial?). In that case, we can easily restrict a function based on the Pass being equal to any of the three or more by using the ~ constraint. Using the dependently typed proof version, though, we would have to create a new GADT for each situation.

In a way, type-level functions deliver on the promise of blurring the line between type and value. Our term-level functions are now type-level functions! We just need to remember to switch our capitalizations!

But this strength is also its weakness. Remember that the problem of normal term-level functions was that they are potentially “incorrect”, and not directly verifiable. So, if you just lift your potentially incorrect term-level functions to the type level…what you get is potentially incorrect type-level functions! You get the same logic errors. Really, writing type-level functions (unsurprisingly) brings all of the error-proneness of writing at the term-level.

In contrast, if you use dependently typed proofs correctly, these proofs can compose, and GHC can check that these proofs compose correctly, or that the compositions of your proofs are also valid proofs. That’s because this is enforced at the structural level. (We’ll look at some examples in the exercises) GHC can’t do that directly with functions; it can’t check that the composition of functions gives correct answers.

These two approaches aren’t necessarily mutually exclusive, and you often might mix the two. It’s good to understand the trade-offs in up-front cost, expressiveness, and correctness! But, however way you play, the singletons library is here to make our life easier.

Singleton Library Functionstop
As we have seen, working with type-level functions with singletons involves at least two parts — the type family working on the type-level values, and the singleton functions mirroring the type family, working on the term-level singletons.

The singletons library offers template haskell to make working with these things pretty seamless. In fact, a good portion of Prelude and base is promoted and exported by singletons!

You can find most of these in the Data.Singletons.Prelude module namespace. So, with singletons, you get functions like:

fst :: (a, b) -> a

type family Fst (t :: (a, b)) :: a

sFst :: Sing t -> Sing (Fst t)
and

isLeft :: Either a b -> Bool

type family IsLeft (t :: Either a b) :: Bool

sIsLeft :: Sing t -> Sing (IsLeft t)
and

(++) :: [a] -> [a] -> [a]

type family (xs :: [a]) ++ (ys :: [a]) :: [a]

(%++) :: Sing xs -> Sing ys -> Sing (xs ++ ys)
Promoted Typeclassestop
But, how can we promote functions like (==) and max, which are typeclass-polymorphic?

With kindclasses (typeclasses for kinds), of course!

Let’s remember what we need for these promoted functions to work: the type families, and the singleton functions.

The singletons library handles this by providing each of these in a separate typeclass. Let’s look at the humble Eq typeclass as an example:

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
The singletons library promotes this as:

class PEq a where
    type (x :: a) == (y :: a) :: Bool       -- ^ associated type / type family
    type (x :: a) /= (y :: a) :: Bool

class SEq a where
    (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
    (%/=) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x /= y)
The naming convention is to just add P for the “promoted” type family functions, and S for the singleton functions.

In fact, you can even promote your own custom typeclasses:

$(singletons [d|
  class MyClass a where
    myFunc :: a -> a
  |])
This would create:

The typeclass MyClass with method myFunc :: MyClass a => a -> a
The promoted typeclass PMyClass with associated type/type family MyFunc (x :: a) :: a
The singletonized typeclass SMyClass with method sMyFunc :: Sing x -> Sing (MyFunc x).
Automatically Promoting Instancestop
The singletons library is smart enough to automatically promote instances, as well, including derived ones!

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
This automatically also generates PEq and SEq instances for Pass:

ghci> :kind! 'Obstruct == 'Obstruct
'True
ghci> SAllow %== SObstruct
SFalse
But, you can also just write:

$(singletons [d|
  data Pass = Obstruct | Allow
    deriving (Show, Eq, Ord)
  |])
And this works as well!

ghci> :kind! Show_ 'Obstruct      -- is named Show_ to not conflict with prelude Show
"Obstruct"
ghci> sMax SObstruct SAllow
SAllow
Next Stepstop
In this article we tackled the problem of more expressive ways to restrict the ways users can manipulate our data types. We talked about “dependently typed proofs” (a staple tool of dependently typed programming) and about “type level functions” (a familiar friend in a new context), their trade-offs, and how the singletons library provides tools to make working with both easier.

When we first looked at the idea of phantom type parameters, using them to restrict how functions are called was definitely one of the promises I made. By now, this promise has hopefully been fully realized.

However, the other promise we made about the usefulness of phantom type parameters is that we can use them be more expressive in what our functions do. One huge benefit of using phantom types in this way is that we can express how our input values relate to our output values in ways that we couldn’t before. (as a simple example, we had previously written closeDoor :: Door 'Opened -> Door 'Closed, which we know closes a door just by looking at its type)

This goes beyond simple restrictions, and we will begin discussing this in the next post! We’ll explore using type-level functions to express more non-trivial and complex relationships, and also talk about code re-use using higher-order functions via singleton’s defunctionalization system.

That’s it for now — check out the exercises, and feel free to ask any questions in the comments, or in freenode #haskell, where I idle as jle`!

Exercisestop
Here are some exercises to help cement your understanding of the concepts here! Feel free to start from the sample source code; it contains all of the solutions, but you can delete everything after the comment -- Exercises if you wish to start on your own!

Remember to enable -Werror=incomplete-patterns or -Wall to ensure that all of your functions are total! None of these implementations should require any incomplete pattern matches!

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

Special Thankstop
I am very humbled to be supported by an amazing community, who make it possible for me to devote time to researching and writing these posts. Very special thanks to my two supporters at the “Amazing” level on patreon, Sam Stites and Josh Vera! :)

Thanks also to Koz Ross for helping proofread this post!

All of this is ignoring the “bottom” value that is an occupant of every type in Haskell. We can use bottom to subvert pretty much all proofs in Haskell, unfortunately, so the discussion from this point forward assumes we are talking about a subset of Haskell where all values are non-bottom and all functions are total.↩︎

Thanks to Darwin226 on reddit for this tip!↩︎

Sorry to mix up similar metaphors like this! Definitely not intentional :)↩︎

Note, however, that we are a little lucky in our case. In the case of our implementation of knock, we match on a wildcard pattern, so the input proof is never evaluated.↩︎

Really, we could just use Bool instead of defining a Pass type. We’re just going through a new type for the sake of example, and it can be useful because a type like Pass might potentially have even more constructors!↩︎

In the spirit of full disclosure, the Template Haskell also generates some other things (known as defunctionalization symbols), which we will be talking about in the next part of this series.↩︎

Note that this is a change since singletons-2.4. In previous versions, ++ would generate the type family :++ and the singleton function %:++.↩︎

Double negation is not true in general, but it is true in the case that our predicate is decidable. That’s because Decision a is essentially a witness to the excluded middle for that specific predicate, from which double negation can be derived.↩︎

It’s the good ol’ Principle of Explosion↩︎

Hi, thanks for reading! You can reach me via email at justin@jle.im, or at twitter at @mstk! This post and all others are published under the CC-BY-NC-ND 3.0 license. Corrections and edits via pull request are welcome and encouraged at the source repository.

If you feel inclined, or this post was particularly helpful for you, why not consider supporting me on Patreon, or a BTC donation? :)

This entry is a part of a series called "Introduction to Singletons". Find the rest of the entries in this series at its series history.
#dependent types
#functional programming
#haskell
#singletons
#types
@HASKELL
@TUTORIALS
+Introduction to Singletons
submit to reddit
← Lenses embody Products, Prisms embody Sums (Previous)
(Next) Introduction to Singletons (Part 4) →


© 2020 Justin Le (CC-BY-NC-ND 3.0)
TwitterGithubTwitchPatreonGoogle+KeybaseLinkedInBitcoinRSSMailing list
-}
