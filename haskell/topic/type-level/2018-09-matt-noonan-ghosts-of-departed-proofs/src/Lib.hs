{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Lib where

import           Data.Coerce
import qualified Data.List          as L
import           Data.List.Extra    (upper)
import qualified Data.List.Utils    as U
import qualified Data.Map.Justified as J
import qualified Data.Map.Strict    as M
import           Data.Ord
import           Prelude            as P hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

------------------------------------------------------------------------------
{-
https://github.com/matt-noonan/gdp-paper/
https://github.com/matt-noonan/gdp
https://github.com/matt-noonan/justified-containers
-}
------------------------------------------------------------------------------
-- 1.

{-
Ghosts of Departed Proofs (Functional Pearl) - GDP
Matt Noonan

a type system provides a mechanism for enforcing program invariants at compile time

e.g.,

ST monad
- enables pure computations to use local mutable state
- phantom type parameter and ank-2 types gives compile-time guarantee
  that that state is invisible from the outside

------------------------------------------------------------------------------
GDP
- Properties and proofs are represented in code.
  - Proofs are entities in host language : can be analyzed
  - propositions are represented by types
  - proof of proposition is a value of that type
- Proofs carried by phantom type parameters
  - phantom type param is mechanism for giving proof to the library API
  - phantom type variables attached to newtype wrappers
  - newtype wrapper erased during compilation
    - no run-time cost
    - proofs do not exists in executable
- Library-controlled APIs to create proofs
  - only library able to create domain proofs/axioms
  - e.g., exporting functions that
    - create values with known properties
    - classify a value into mutually disjoint refinements
    - introduce existentially quantified properties
      - e.g., name in TODO, runSt in TODO, or withMap in TODO
- Library exports combinators for manipulating proofs
  - user can use with evidence at hand to produce a proof of a safety property
  - resulting proof communicated to library

------------------------------------------------------------------------------
note on safe coercions

some examples rely on safe coercions
- type T
    and
  newtype N = N T
    have same runtime rep
- coerce :: Coercible a b => a -> b
    is a zero-cost cast from a to b
-   If N is a newtype of T, then constraints
  Coercible N T
  Coercible T N
    hold in any module where constructor of N is visible

------------------------------------------------------------------------------
2.  case study #1 : sorted lists

User must guarantee.  Not expressed in types:

sortBy  :: (a -> a -> Ordering) -> [a] -> [a]
-- Usage constraint: in `mergeBy comp xs ys`
-- input lists `xs` and `ys` should also be sorted by the same comparator
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]

What if User has a proof that input lists are sorted properly?

2.1 Conjuring a Name

how to express the idea of two comparators being "the same"?
- newtype wrapper with phantom type parameter NAME
- written :  a ~~ n
  - "values of type 'a' with name 'n'
- impl: newtype around a, with phantom type param n
-}

-- module for attaching names to values:

-- module Named (Named, type (~~), name) where
-- - hides constructor of Named
--   ensures that name is the only way to introduce a name for a value

-- import Data.Coerce

-- key feature : exported `name` funcation expresses "any value can be given a name"
newtype Named name a = Named a
type a ~~ name = Named name a

{-
to emulate existentially-quantified type
- instead of directly returning a value with a name attached
- name says to user "tell me what you want to do with that named value, and I’ll do it for you"
- user passes a computation that is agnostic about the name that will be chosen
- See TODO
-}
-- means: ype of `name`    is     a -> (exists name. (a ~~ name))
-- rank-2 type of name, uses a polymorphic continuation (emulates an existential type)
name :: a -> (forall name. (a ~~ name) -> t) -> t
name x k = k (coerce x)

-- TODO : HC : why not just use coerce directly (instead of via `the`)?
-- module The where

-- to remove names and other phantom data from a value
class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a -- ensures no runtime cost
  the = coerce

-- most instances declared with an empty body
instance The (a ~~ name) a

--------------------------------------------------
{-
2.2 Safe API for sorting/merging

-- module Sorted (Named, SortedBy, sortBy, mergeBy) where
--
-- import           The
-- import           Named

-- import           Data.Coerce
-- import qualified Data.List       as L
-- import qualified Data.List.Utils as U
-}

-- refinement represents the predicate "x has been sorted by the comparator named comp"
-- wrapper’s meaning is the type of sortBy
-- - takes a named comparator and a list
-- - produces a list that has been SortedBy comp
-- do NOT export SortedBy’s constructor
-- - ensures the only way to obtain a value of type `SortedBy comp [a]` is via
-- `sortBy` or `mergeBy`
newtype SortedBy comp a = SortedBy a
instance The (SortedBy comp a) a

-- `the` coerces away the name of the comparator
-- apply sortBy from Data.List
-- introduce `SortedBy comp` predicate by coercing result
-- coercions erased so this is just a call to Data.List.sortBy
sortBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> [a]
  -> SortedBy comp [a]
sortBy comp xs = coerce (L.sortBy (the comp) xs)

-- after erasure, this will be just a call U.mergeBy
-- user must provide
-- - a named comparator
-- - two lists sorted by same comparator
mergeBy
  :: ((a -> a -> Ordering) ~~ comp)
  -> SortedBy comp [a]
  -> SortedBy comp [a]
  -> SortedBy comp [a]
mergeBy comp xs ys =
  coerce (U.mergeBy (the comp) (the xs) (the ys))

-- 2.3 user code

{-
library controls introduction of proofs
user leverages those proofs
e.g.,
user function ensures it can only be called on a sorted list
-}
minimum_O1 :: SortedBy comp [a] -> Maybe a
minimum_O1 xs = case the xs of
  []    -> Nothing
  (x:_) -> Just x

--------------------------------------------------
-- 2.4 danger of naming a ghost

-- why is type of `name` above needed instead of
-- (question is really about whether caller or called chooses particular name):

--  caller provides `a` and `name`
anyName :: a -> (a ~~ name)
anyName = coerce

-- but allows

data Simon

-- import Data.Ord

-- user incorrectly names two *different* functions Simon, breaking guarantees of Sorted module
up, down :: (Int -> Int -> Ordering) ~~ Simon
up           = anyName compare
down         = anyName (comparing Down)
list1,list2 :: SortedBy Simon [Int]
list1        = sortBy up   [1,2,3]
list2        = sortBy down [1,2,3]
merged      :: [Int]
merged       = the (mergeBy up list1 list2) :: [Int] -- [1,2,3,3,2,1]

-- using `name` prevents above
{-
xx = name compare $ \up ->
       name (comparing Down) $ \down ->
         let list1 = sortBy up [1,2,3 :: Int]
             list2 = sortBy down [1,2,3]
          in the (mergeBy up list1 list2) -- <-- compiler complains here:

    - Couldn't match type ‘name1’ with ‘name’
      Expected type: SortedBy name [Int]
        Actual type: SortedBy name1 [Int]
-}

{-
difference between examples
- in 1: user creates a named value
- in 2: user can only consume a named value
  - by providing polymorphic function that can work with any named value
    - uses `name` to apply the consumer to a normal, unnamed value
  - library selects a name that is not inspectable by user
-}

yy :: [Int]
yy  = name compare $ \up' ->
        let l1 = sortBy up' [1,2,3]
            l2 = sortBy up' [1,2,3]
         in the (mergeBy up' l1 l2)

------------------------------------------------------------------------------
-- 3. case study #2 : sharing state threads

{-
The technique of using rank-2 types to create names outside of the user’s control
is inspired by the ST monad and its rank-2 function.

The connection between ST monad and GDP-style names is elaborated here.
The elaboration suggests extensions to the ST API.

The ST API shown below using `St` to disambiguate from Control.Monad.ST.
-- think of the `s` param as representing a name attached to a region of the heap
-- think of `ST s` as acting like a State monad over named regions

    a ∈ s    denote a reference cell of type a in the memory region named s
             In Control.Monad.ST, a ∈ s ==== STRef s a

runSt    :: (forall s.      St s  a)     -> a
newRef   ::            a -> St s (a ∈ s)
readRef  :: (a ∈ s)      -> St s  a
writeRef :: (a ∈ s) -> a -> St s  ()

pseudocode:

data Region  = Region
type St s a  = State (Region ~~ s) a
runSt       :: (forall s. St s a) -> a
runSt action = name Region (evalState action)

treating ST's phantom type as a region name leads to ideas for other primitives
- once regions can be name, create more detailed names to describe the contours of those regions

e.g., add type constructor ∩ : s ∩ s’ names the region at the intersection of s and s’
using that enables new capability
- individual sub-computations may share mutable reference cells with other sub-computations

runSt2 :: (forall s s'. St (s ∩ s') a) -> a
liftL  :: St s  a        -> St (s ∩ s') a
liftR  :: St s' a        -> St (s ∩ s') a
share  :: (a ∈ s)        -> St s (a ∈ (s ∩ s'))
use    :: (a ∈ (s ∩ s')) -> (a ∈ s)
symm   :: (a ∈ (s ∩ s')) -> (a ∈ (s' ∩ s))

-- ST-style pure computation using local mutable references.
-- Although "secret" reference is in scope during the calculation in the "right" region,
-- any attempted access will fail to compile.
stSharingDemo :: Bool
stSharingDemo = runSt2 $ do
  -- In the "left" memory region, create and return
  -- two references; one shared, and one not shared.
  (secret, ref) <- liftL $ do
    unshared <- newRef 42
    shared <- share =<< newRef 17
    return (unshared, shared)
  -- In the "right" memory region, mutate the shared reference.
  -- If we attempt to access the non-shared reference here,
  -- the program will not compile.
  liftR $ do
    let mine = use (symm ref)
    x <- readRef mine
    writeRef mine (x + 1)
  -- Back in the "left" memory region, verify that the unshared reference
  -- still holds its original value.
  liftL $ do
    check <- readRef secret
    return (check == 42)

runSt2 enables user to run a computation that uses two partially-overlapping memory regions.
Within the computation, the user can run sub-computations bound to one or the other memory region.
A sub-computation can move any variable that it owns into the common overlap via share.
-}

------------------------------------------------------------------------------
-- 4 case study #3 : key-value lookups

{-
executions using key-value maps that rely on certain keys being present at critical moments.

use GDP to express "this key must be present in that map"

belows shows a GDP API based on author’s justified-containers package
key features
- predicate `Key ks` : "belongs to the key set named ks"
  - a value of type `Key ks k` is
    - a value of type `k`
    - with a ghost proof that it is present in the key set named `ks`
- predicate `JMap ks` : "has a key set named ks"
  - a valueof type `JMap ks k v` is
    - a `Map k v`
    - with a key set named `ks`
- rank-2 `withMap` (analogous to `name`) : attaches a ghostly key set to a map
  - encodes notion that map has some set of keys (perhaps not known at compile time)
- `member` : checks if a key is present in a map with key set `ks`
  - if so, produces a ghost proof of that fact using `Key ks`
- `lookup` : total because the key carries a ghost proof that it is present in the map
  - so can `v` (instead of `Maybe v`)
-}

newtype JMap ks k v = JMap (M.Map k v) deriving Functor
newtype Key  ks k   = Key k
instance The (JMap ks k v) (M.Map k v)
instance The (Key  ks k) k
member   :: k -> JMap ks k v -> Maybe (Key ks k)
member    = undefined
lookup   :: Key ks k -> JMap ks k v -> v
lookup    = undefined
reinsert :: Key ks k -> v -> JMap ks k v -> JMap ks k v
reinsert  = undefined
{-
withMap  :: M.Map k v -> (forall ks. JMap ks k v -> t) -> t
withMap   = undefined
 - Cannot instantiate unification variable ‘a0’
    with a type involving foralls:
       M.Map k v -> (forall ks. JMap ks k v -> t) -> t
       GHC doesn't yet support impredicative polymorphism
-}

-- usage example

{-
member used to check if key is present
in scope of the Just case, key carries a phantom proof of presence
that proof used as evidence that key is present in other maps (e.q., table’ and table'')
-}

test :: M.Map Integer String
test = M.fromList [ (1, "Hello"), (2, "world!") ]

eg :: IO ()
eg = J.withMap test $ \table ->
       case J.member 1 table of
         Nothing -> putStrLn "Missing key!"
         Just key -> do
           let table'  = J.reinsert key "Howdy" table
               table'' = fmap upper table
           putStrLn ("Value in map 1: " ++ J.lookup key table)
           putStrLn ("Value in map 2: " ++ J.lookup key table')
           putStrLn ("Value in map 3: " ++ J.lookup key table'')
-- Output:
-- Value in map 1: Hello
-- Value in map 2: Howdy
-- Value in map 3: HELLO


--------------------------------------------------
-- 4.1 designing for the user's state of knowledge

{-
compare lookup types

           k ->  Map    k v -> Maybe v
    Key ks k -> JMap ks k v ->       v

they have different expectations about user’s knowledge

if user does not know if key is present, then Maybe-returning is appropriate.
- that incomplete knowledge shown in return type
if user knows key is present then avoid dealing with impossible missing-key state
- to ensure safety, user must communicate evidence to library
-}

--------------------------------------------------
-- application: Well-formed Adjacency Lists
{-
maps where values are reference the keys

adjacency representation for directed graphs
- maps each vertex to its list of immediate neighbors
-}

type Digraph0 v = M.Map v [v]

{-
well-formed Digraphs satisfy property
- every vertex referenced in any neighbor list is also a valid key in the adjacency map

Data.Graph API from containers make user responsible to ensure well-formedness

GDP-style API for maps gives vocabulary for expressing "a well-formed adjacency list"
- a well-formed adjacency map should map each vertex to
  a list of vertices that are keys of that same map.
- user can now enforce well-formednes at compile time
-}
type Digraph vs v = JMap vs v [Key vs v]

--------------------------------------------------
-- 4.3 changing the key set

{-
case : maps that are related but do not have exactly the same key sets

consider `insert`
- usually modifies the key set of a map
- gives knowledge of keys in updated map

e.g., in possession of key and proof it is present
- want to use same key in updated map
- library provides a proof combinator that converts a proof of
  "k is a valid key of m"
     into a proof of
  "k is a valid key of `insert k’ v m`"
-}
{-
inserting
  :: Ord k
  => k -> v -> JMap ks  k v
  -> (forall ks'
      .        JMap ks' k v       -- 1
      -> (Key ks  k -> Key ks' k) -- 2
      ->  Key ks' k               -- 3
      -> t)
  -> t
inserting = undefined -- impredicative
-}

{-
insertion results in map with new key set
- introduce ghost of new key set inside another forall

other parameters being passed to the continuation
- a collection of evidence and proof combinators that user may need to formulate a safety argument
1. updated map : JMap ks’ k v
   phantom type ks’ represents the key set ks, updated with the newly-inserted key
2. function that represents inclusion of ks into ks’
   user can apply this to convert a proof that a certain key belonged to the old map
   (a value of type Key ks k) into a proof that the key also belongs to the new map
   (a value of type Key ks’ k).
3. evidence that inserted key is present in new key set
-}

------------------------------------------------------------------------------
-- 5 case study #4 : arbitrary invariants

{-
above:
- introduced names and predicates to enable user to express correctness proofs
  - several ways to introduce name-like entity : `name`, `runSt2`, `withMap`, `inserting`
- use of proofs carried by phantom type parameters
  - phantom types attached via domain-specific newtype wrappers : `SortedBy`, `∈`, `JMap`

this section shows a uniform mechanism for names/proofs

separate type-level names from constraints placed on named values
- e.g., safe head
    newtype wrapper   :::    (pronounced "such that)

then (a ~~ n ::: p) reads "value of type a, named n, such that condition p holds"
-}
data IsNil  xs
data IsCons xs
{-
head :: ([a] ~~ xs ::: IsCons xs) -> a
head xs = Prelude.head (the xs)

:::    similar to Refined type from refinement : https://nikita-volkov.github.io/refined/
- more power when used with names
- names enable encoding predicates about specific values at type level

--------------------------------------------------
5.1 Logical Combinators for Ghostly Proofs

how does create proofs to inhabit phantom types?
-}

-- single phantom type parameter and ONE non-bottom value
-- type exported, constructor hidden (use `axiom`)
data Proof p = QED

-- can now encode rules of natural deduction as functions that produce terms of type Proof p

-- attach proofs to values
newtype a ::: p = SuchThat a
-- proof of type `Proof p` is attached to
-- a value of type `a` via :::
-- producing a value of type (a ::: p)
--
-- p will usually be a proof about the wrapped value, but that is not required
-- any value can carry any proof
-- the only thing that links value to proof is the use of a common name.
(...) :: a -> Proof p -> (a ::: p)
x ... _proof = coerce x

-- logical constants : empty because types are only used as phantoms
data TRUE
data FALSE
data p && q
data p || q
data p --> q
data Not p
data p == q

-- Natural deduction rules (implementations all -- ignore parameters and return `QED`)
andIntro    :: Proof p -> Proof q -> Proof (p && q)
andIntro     = undefined
andElimL    ::                       Proof (p && q) -> Proof p
andElimL     = undefined
orIntroL    :: Proof p -> Proof q -> Proof (p || q)
orIntroL     = undefined
implIntro   :: (Proof p -> Proof q) -> Proof (p --> q)
implIntro    = undefined
implElim    ::                         Proof (p --> q) -> Proof p -> Proof q
implElim     = undefined
notIntro    :: (Proof p -> Proof FALSE)  -> Proof (Not p)
notIntro     = undefined
contradicts :: Proof p  -> Proof (Not p) -> Proof FALSE
contradicts  = undefined
absurd      :: Proof FALSE -> Proof p
absurd       = undefined
refl        :: Proof (x == x)
refl         = undefined
-- ...

-- exported function that enables library authors to assert axioms about their API
axiom       :: Proof p
axiom        = QED

{-
--------------------------------------------------
5.2 Naming Library Functions

for a library author to export lemma "reversing a list twice gives the original list"
- not sufficient to have a name for the original list
- must also name some of the library’s functions

extension to the Named module
-}

-- module Named

data Defn = Defn -- Type exported, constructor hidden.

-- constraint synonym expected to only be available in module where `f` is defined
type Defining f = (Coercible Defn f, Coercible f Defn)

-- Enable library authors to define introduction rules for -- names that they have defined.
-- coercion is only possible since this function is in the Named module.
defn :: Defining f
     => a
     -> (a ~~ f)
defn = coerce

{-
- library introduces a new name X by defining X as a newtype alias of Defn
- library does not export constructor of X
- then the constraint Defining X only holds in the module where X was defined
- the defn function is used to attach the name X to an arbitrary value
  but only in the module where X is defined
- by exporting `defn` with the `Defining f` constraint,
  the Named module enables library authors to introduce new names and axioms
  while users are restrained

Note :viz-a-viz `inserting`, lemmas about a function now stand on their own
- library can add lemmas about reverse without modifying its signature
- can create lemmas that relate multiple functions:

example:
-}

-- API functions
reverse       :: ([a] ~~ xs) -> ([a] ~~ Rev xs)
reverse xs     = defn (P.reverse (the xs))

length :: ([a] ~~ xs) -> (Int ~~ Length xs)
length xs = defn (P.length (the xs))
{- TODO : compile
zipWith
  :: (a -> b -> c)
  -> ([a] ~~ xs ::: Length xs == n)
  -> ([b] ~~ ys ::: Length ys == n)
  -> [c]
zipWith f xs ys = P.zipWith f (the xs) (the ys)
-}
-- Names for API functions
newtype Length xs = Length Defn
newtype Rev    xs = Rev    Defn

-- Lemmas (all bodies are `axiom`)
revRev        :: Proof (Rev (Rev xs) == xs)
revRev         = axiom
{-
revCons  :: Proof (IsCons xs) -> Proof (IsCons (Rev xs))
revCons   = axiom
-}
revLength :: Proof (Length (Rev xs) == Length xs)
revLength = axiom

data ListCase a xs
  = IsCons (Proof (IsCons xs))
  | IsNil  (Proof (IsCons xs))

classify :: ([a] ~~ xs) -> ListCase a xs
classify xs = case the xs of
  (_:_) -> IsCons axiom
  []    -> IsNil  axiom

-- sample of client code for above
{- TODO : needs zipWith (above)
-- user defined dot product that only can be applied to same-sized lists
dot :: ([Double] ~~ vec1 ::: Length vec1 == n)
    -> ([Double] ~~ vec2 ::: Length vec2 == n)
    -> Double
dot vec1 vec2 = sum (Lib.zipWith (*) vec1 vec2)

-- Compute the dot product of a list with its reverse.
-- user uses evidence to tell compiler that dot product of a list with its reverse is legal
dotRev :: [Double] -> Double
dotRev xs = name xs $ \vec ->
  -- dot is well-typed by expressing a proof that vec and reverse vec have the same length
  dot (vec ...refl) (Lib.reverse vec ...revLength)
-}
-- refl and revLength are axiom schemas
-- unification with the type of dot selects the correct instances

--------------------------------------------------
-- On the safety of defn
{-

regarding "Simon" example above : is `defn` as bad as `any_name`?

YES: but only for the library which must be careful about how Simon is introduced.
Users still unable to name arbitrary values "Simon" using defn,
because they do not possess the necessary Defining Simon constraint.
-}

--------------------------------------------------
-- 5.3 Building Theory Libraries
