{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Ch18 where

import           Ch17            (Category (..), Identity (..), (:=>:), (:.:))
------------------------------------------------------------------------------
import           Control.Monad.Free
import           Data.Kind
import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t)

-- Kleisli arrow composition
-- (.) ::            (b ->   c) -> (a ->   b) -> a ->   c
(<=<)  :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
f <=< g = \x -> g x >>= f

------------------------------------------------------------------------------
-- p 257 Adjunctions

-- https://github.com/lambdaconf/book-of-monads/pull/162 - newtype
newtype State s a = State { runState :: s -> (a, s) }

{-
def has two parts:
- type constructor (s ->) : supplies old state
- type constructor (, s)  : couples new state with return value

both are functors

State is composition of those into single functor

ADJOINT : pairs of functors that satisfy a relation between their arrows
- e.g., (s ->) and (, s)
- given two adjoint functors, can build a monad by composing them

chapter introduces adjoint functor and their relationship with monads

same notion forms basis of free constructions (e.g., free monads)
-}

-- p 257 17.1 Adjoint Functors

{-
given categories C and D, and two functors F C -> D, G; G D -> C
there is an adjunction between F and G
written F -| G
whenever there is an isomorphism between the sets of arrows:

   (in D) F C -> D    and    C -> G D (in C)

-- p 258

F is LEFT  adjoint
G is RIGHT adjoint

if F and G are endofunctors
- only one category, so easier
- do not have to specify where objects and arrows come from

adjunctions packages provides Adjunction class
- specializes above def category of Haskell types
- recall  Haskell’s Functor defines endofunctors in that category:
-}

class (Functor f, Functor g) => Adjunction f g where
  leftAdjunct  :: (f a -> b) -> (a -> g b)
  rightAdjunct :: (a -> g b) -> (f a -> b)

{-
Adjunction's methods witness isomorphism between the sets of arrows
- f and g are adjoints if can turn any f a -> b into a -> g b (and vice versa)
- since isomorphic: composition of two methods, in any order, is identity

EXAMPLE between (family of adjunctions, one for t)
- partially applied pairs: (t,) LEFT
- funs, (t ->) RIGHT

    (t,) -| (t ->)

if adjunction exists, then
-}
instance Adjunction ((,) t) ((->) t) where
  leftAdjunct  :: ((t, a) -> b) -> (a -> t -> b)
  leftAdjunct = undefined
  rightAdjunct :: (a -> t -> b) -> ((t, a) -> b)
  rightAdjunct = undefined

-- TODO Exercise 17.1 Write impl of above. Check they form an isomorphism.

{-
EXAMPLE between higher-kinded versions of Void and (): Void1 -| Unit1
- Void1 : functor with no constructors
- Unit1 : functor with one constructor
-}
data Void1 a         deriving Functor
data Unit1 a = Unit1 deriving Functor

{-
proof
- two functions that witness isomorphism between the arrows:
-}
{-# ANN module "HLint: ignore Redundant lambda" #-}
{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Use lambda-case" #-}
instance Adjunction Void1 Unit1 where
  leftAdjunct   :: (Void1 a -> b) -> (a -> Unit1 b)
  leftAdjunct  _ = \_ -> Unit1
  rightAdjunct  :: (a -> Unit1 b) -> (Void1 a -> b)
  rightAdjunct _ = \v -> case v of { } -- absurd pattern : pattern match with no choice
                                       -- since Void1 has no constructors

{-
now check composition in both directions is identity

   (leftAdjunct . rightAdjunct) f           (f :: a -> Unit1 b)
=> leftAdjunct (rightAdjunct f)             ^
=> leftAdjunct (\v -> case v of { })        |
=> \_ -> Unit1 -----------------------------+

   (rightAdjunct . leftAdjunct) g           (g :: Void1 a -> b)
=> rightAdjunct (leftAdjunct g)             ^
=> rightAdjunct (\_ -> Unit1)               |
=> \v -> case v of { } ---------------------+

-- https://github.com/lambdaconf/book-of-monads/pull/163 : readability
looks like isomorphism does not hold
- but f :: a -> Unit1 b :  forces 'f' to return Unit1 regardless of input
- only one way to build 'Unit1 b' : takes no params
- therefore f equivalent to result of right and then left-adjoining it

reason similarly with g
- it receives a value of type Void1 a
- Void1 has no constructors
- only thing it can do is pattern match with no choice

EXAMPLE between (two categories involved are not the same)
- diagonal functor
- pair functor

to describe, need to introduce idea of product of categories
- given categories C and D, the product C x D is category with
- objects as pairs of (C, D)
- arrows are pairs of arrows (f, g), one per category

identities / compositions defined component-wise:

             id_(C,D) == (id_C,id_D)
    (f1,g1) . (f2,g2) == (f1 . f2, g1 . g2)

diagonal functor Diag goes from any category C into the product of C with itself : C x C
action is to duplicate objects and arrows
- object C sent to (C, C)
- arrow  f sent to (f, f)

product functor (,) takes objects in C x C and builds the internal product of them in C
- haskell : takes x and y values puts them into (x, y)
-}

-- | Each function of pair works independently over each argument
--   Known as (***) in Control.Arrow
map :: (a -> b, c -> d) -- Arrow in C x C
    -> (a, c) -> (b, d) -- Arrow in C
map (f, g) = \(x, y) -> (f x, g y)

{-
adjunction is Diag |- (,)

via substitution from original def of adjunction, see the need to provide an isomorphism between:

   (in CxC) Diag C -> D    and    C -> (,)D (in C)

objects in C x C are pairs of objects from C, so can rewrite def with explicit pairs
and expand def Diag C:

         (C,C) -> (D1,D2)  and    C -> (,)(D1,D2)

left works in C x C
so, writing  (C, C) -> (D1, D2) means a pair of arrows (C -> D1, C -> D2)

funs witnessing the isomorphism have the types:

  leftAdjunct  :: (c -> d1, c -> d2) -> (c -> (d1, d2))
  rightAdjunct :: (c -> (d1, d2)) -> (c -> d1, c -> d2)

-- TODO Exercise 17.2 Write impls of above
-- TODO Exercise 17.3 Give details of : Either |- Diag

tempting to think that since Diag |- (,) and Either |- Diag, therefore Either |- (,)
- but NO : from A |- B and B |- C, cannot deduce (in general) A |- C
- because |- relation is not transitive
-}

------------------------------------------------------------------------------
-- p 260 17.2 Monads from Adjunctions

{-
case where the 'b' in

    leftAdjunct :: (f a -> b) -> (a -> g b)

is set to f a

then becomes:


    leftAdjunct :: (f a -> f a) -> (a -> g (f a))
                   ^
                   |
                   identity

:t leftAdjunct Prelude.id
leftAdjunct Prelude.id :: Adjunction f g => a -> g (f a)

define as:

  η :: a -> g (f a)
  η = leftAdjunct id

this is a polymorphic function between functors so may express as a natural transformation:
-}
-- https://github.com/lambdaconf/book-of-monads/pull/164 -- Id to Identity
η :: Identity :=>: (g :.: f)
η = undefined

-- TODO Exercise 17.4 using rightAdjunct build natural transformation in converse direction
ε :: (f :.: g) :=>: Identity
ε = undefined

{-
for adjunction f |- g
- η is unit
- ε is counit

can define an adjunction using two different sets of data:
1. an isomorphism between f a -> b and a -> g b
2. two natural transformations Identity :=>: (g :.: f) and (f :.: g) :=>: Identity

already shown going from (1) to (2)

prove converse : define leftAdjunct and rightAdjunct in terms of η and ε

leftAdjunct case
- need to turn f a -> b into a -> g b, giving the skeleton for

    leftAdjunct f = \a -> ... -- need to build something of type 'g b'

a has type a, so can apply unit to it

    leftAdjunct f = \a -> ... (η a)
    leftAdjunct f = \a -> ... ((g :.: f) a)
    leftAdjunct f = \a -> ... (g (f a))

f is f a -> b
so can use it under g to get value of type g b ; via fmap (g is functor)

    leftAdjunct f = \a -> fmap f (η a)

-- https://github.com/lambdaconf/book-of-monads/pull/165 -- remove vertical bars |ε|
-- TODO Exercise 17.5 finish proof by defining rightAdjunct using ε


put η and ε on hold

define

μ :: g (f (g (f a))) -> g (f a)
μ = fmap ε

how does this de nition work?
expression is surrounded by fmap to work "inside the functor"
- so arg should be f (g (f a))
                g (f (g (f a))) -> g (f a)
                (g :.: f  :.:  g :.:f ) :=>: (g :.: f)
                (g :.: f) :.: (g :.: f) :=>: (g :.: f)
then rewrite using
     type t = g :.: f

     μ :: (t :.: t) :=>: t

the type of monad 'join'

similar steps give 'return'

     η :: Identity :=> t

SAYS: given adjunction f |- g
- build new monad by taking composition g :.: f
- demonstrates relationship between adjunctions and monads

establishes formal ground for 'State'

adjunction (s,) |- (s ->)
obtain monad by taking type t = (s ->) :.: (s,)
when used generic type arg 'a'

                             ((s ->) :.: (s,)) a
definition of :.:            (s ->) ((s,) a)
reducing innermost type      (s ->) (s, a)
reducing outermost type      s -> (s, a)

exact def of State (except elements in pair are reversed)
-}

------------------------------------------------------------------------------
-- p 262 17.2.1 Comonads : dual of Monad via reversing the arrows

-- section goal : show how exploration of category theory enables discovering new concepts

-- | names from comonad package
class Comonad w where
  extract   :: w a -> a                  -- coreturn
  duplicate :: w a -> w (w a)            -- cojoin
  extend    :: (w a -> b) -> w a -> w b  -- cobind

-- EXAMPLE : non-empty lists : so able to define extract
instance Comonad [] where
  extract        :: [a] -> a
  extract   (x:_) = x
  extract     []  = error "Comonad extract"
  duplicate      :: [a] -> [[a]]
  duplicate   xs  = [xs]
  extend         :: ([a] -> b) -> [a] -> [b]
  extend    f xs  = [f xs]

{-
-- https://github.com/lambdaconf/book-of-monads/pull/166 -- fix grammar

insight
- repeat construction that gave monad for g :.: f coming from an adjunction, f |- g
  but reversing the arrows
- result is comonad :  f :.: g

EXAMPLE : (s,) |- (s ->)

                ((s,) :.: (s ->)) a
                (s,) ((s ->) a)
                (s,) (s -> a)
                (s, s -> a)

the Store comonad (used in lens definitions, to describe automata, etc) usually defined as
-}
data Store s a = Store (s -> a) s

------------------------------------------------------------------------------
-- p 263 17.3 The Kleisli Category

{-
a (co)monad can be build from every adjunction

every monad arises from an adjunction (in different ways)

section explore one via Kleisli category (see Section 5.3.1)

Kleisli category of monad m over category C consists of same objects in C
- but (Kleisli) arrows have form A -> m B
-}

newtype Kleisli m a b = Kleisli (a -> m b)

instance Monad m => Category * (Kleisli m) where
  id :: Kleisli m a a -- a -> m a
  id  = Kleisli return
  Kleisli f . Kleisli g = Kleisli (f <=< g)

{-
-- https://github.com/lambdaconf/book-of-monads/pull/167 -- "former" to "identity"

introduced Kleisli arrows when describing monad laws
can now see the laws are the rules of any category
- e.g., left identity law : id . f == f
  - substitute general names for identity and composition for Kleisli versions:

    id . Kleisli f
    Kleisli return . Kleisli f
    Kleisli (return <=< f)
    Kleisli f

-- https://github.com/lambdaconf/book-of-monads/pull/168 -- grammar
END GOAL : to recover m as an adjunction (via adjunction between C and Kleisli m)

def functors for both directions
1. map C to Kleisli m
   object part : take each A in C to same A in Kleisli m
   arrow  part : take each a -> b to Kleisli m a b
2. maps Kleisli m to C
   object part : takes each A in Kleisli m to m A
   arrow part  : convert each arrow Kleisli m a b to arrow m a -> m b in C

since the functors do not work only in the category of Haskell types
cannot express them as instances of the Functor type class:
-}
bareToKleisli :: Monad m => (a -> b) -> Kleisli m a b
bareToKleisli f = Kleisli $ \x -> return (f x)
          -- or = Kleisli $ return . f

kleisliToBare :: Monad m => Kleisli m a b -> m a -> m b
kleisliToBare (Kleisli f) x = x >>= f
{-
adjunction is

               bareToKleisli |- kleisliToBare

cannot express as instance of Adjunction type class (only allows endofunctors)
given adjunction F |- G, where F D -> E, require

    (in E)   F C -> D     and C -> G D (in D)

case of Kleisli adjunction
- D is C
- E is Kleisli m -- https://github.com/lambdaconf/book-of-monads/pull/169 - remove misleading words

object part of functor F (bareToKleisli) does not change the object untouched

G (kleisliToBare) embeds objects into monad m

shape of the adjunction is an isomorphism between:

                Kleisli m C D and  C -> m D

Kleisli m a b is a -> m b

the adjunction is an isomorphism between arrows of the form
   a -> m b, on the left
and arrows of the form
   a -> m b, on the right

the identity works as the isomorphism needed

the other construction that builds an adjunction from a monad uses the Eilenberg-Moore category
- does not have as many practical uses as Kleisli’s
-}

------------------------------------------------------------------------------
-- p 265 17.4 Free Monads via adjunctions

{-
all free constructions arise similarly to one described for monads (not delving into generic approach)

categories in adjunction
- End(C) : monads as monoids in category of endofunctors of C (ch 16)
- Monad(C)

Monad(C)
- objects : endofunctors from C that can be turned into a monad
  e.g., Maybe (but not ZipList -- has Functor but not Monad instance)
- arrows  : natural transformations that respect monadic structure
  instantiated to Haskell types category, means arrows are polymorphic functions
       m :: f a -> g a
  between two monads that preserve the structure
            m . return == return
       m . f <=< m . g ==  m . (f <=< g)
  these monad morphisms arise from generalizing monoid morphisms
  (i.e., proved f mempty == mempty)
  these morphisms act polymorphically, so can composition;
  identity and combination operations changed into return and Kleisli composition

Monad(C) is a subcateogry of End(C)
- formed by choosing subset of objects and arrows between those objects
- build a forgetful functor that "forgets" additional structure
  imposed by monads as opposed to functors

define functor Forget :: Monadp(C) -> End(C)
- each object in Monad(C) mapped to same object in End(C) (seen as a endofunctor)
- each arrow  in Monad(C) mapped to same arrow  in End(C) (seen as a natural transformation)

forgetful functors arise to change richer structure into simpler structure
- e.g., category of monoids to the category of semigroups : "forget" the identity element

free monad construction is left adjoint of forgetful functor : Free |- Forget

means isomorphism between:

          (in Monad(C)) Free C -> D    and   C -> Forget D (in End(C))

forgetful functor leaves objects untouched, except for removal of monadic structure
- so Forget D can be viewed as D
- arrows in both End(C) and Monad(C) are natural transformations
  written as polymorphic functors in Haskell
- C is object in End(C),   thus an endofunctor
- D is object in Monad(C), thus a monad
-}
{-
-- import Control.Monad.Free above
data Free f a = Free (f (Free f a)) | Pure a -- from ch13
-}

leftAdjunctF  :: (Functor c, Monad d)
              => (forall a. Free c a -> d a)
              -> (forall a. c a -> d a)
leftAdjunctF f = f Prelude.. liftF
rightAdjunctF :: (Functor c, Monad d)
              => (forall a. c a -> d a)
              -> (forall a. Free c a -> d a)
rightAdjunctF = foldFree

{-
rightAdjunct called foldFree in Ch13
- folds interpretation of each op described by c into monad d
  throughout computation described by Free c

-- https://github.com/lambdaconf/book-of-monads/pull/170 -- make concrete
leftAdjunctF : need to to lift single instruction from c into a complete Free c construction

liftF :: Functor c => c a -> Free c a
liftF = Free Prelude.. fmap return

two proofs needed to say that these two ops form an adjunction
1. rightAdjunct produces an arrow in Monadp(q)
   check that op defines a monad morphism
2. check leftAdjunct and rightAdjunct are inverses

category theory gives a sound basis to operations
-}

{-
-- https://github.com/lambdaconf/book-of-monads/issues/171
part V needs a "map"

I completed reading chapters 16 and 17.  To really understand the material I would have to read and try the material multiple times.   I think that is inherent in the nature of the material.

That said, pretty much the entire time, at any particular point in those chapters, I always was at a lose for why I was being shown something at a particular time.

It seems to me that "Part V Diving into Theory" needs an introduction that maps out, in order, all the major points of the coming material explaining what and why they are being shown at a particular time.    In other words: it really needs a top-level map to give the reader a sense of "place" in the big picture when reading.
-}

ts18 :: [Test]
ts18 = U.t "ts18" 'a' 'a'

------------------------------------------------------------------------------
t18 :: IO Counts
t18  =
  runTestTT $ TestList {-$-}
  ts18
