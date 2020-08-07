{-# LANGUAGE NoImplicitPrelude #-}

module Lib where

import Protolude hiding (Map)

{-
https://github.com/conal/talk-2014-lambdajam-denotational-design

------------------------------------------------------------------------------
1. Introduction

data type rep collects related pieces of info : making more convenient to use

data abstraction : separation interface/impl
- reveals what users need
- implementor free to improve hidden impl

interface
- names of data types
- names and types of operations that work on them

e.g., finite map:

abstract type Map :: ∗ -> ∗ -> ∗

empty  :: (     ...) => Map k v
insert :: (     ...) => k -> v -> Map k v -> Map k v
lookup :: (Eq k ...) => Map k v -> k -> Maybe v

Map rep and operation impl not revealed, but
- impl reveals too much info  (i.e.,, class constraints)
- signatures provide too little
- users care about what the names "mean"

use DENOTATIONAL SEMANTICS to provide the essence of a type
- meaning of ops defined as a function from meanings of args to meaning result
E.g.,
- meaning of `Map k v` could be partial functions from k to v.
  - empty yields bottom for all inputs
  - insert extends a partial function
  - lookup is function application, yielding bottom where undefined

A type class determines part of the essence, as well as the form, of each of its instances.

What about the rest of the meaning of a type class instance
- the semantic freedom remaining after the types and laws have had their say?

principle for answering that question:
- THE INSTANCE’S MEANING FOLLOWS THE MEANING’S INSTANCE
- meaning of each method application is given by
  application of the same method to the meanings of the arguments.
- The semantic function is a type class morphism
  - i.e., it preserves class structure.

e.g., define Monoid instance for above finite map type
TCM says
- meaning of ∅ on maps must be ∅ for partial functions
- meaning of (⊕) of two maps is the (⊕) of the partial functions denoted by those maps

Sometimes TCM property fails.
- examination of failure leads to simpler/better design for which TCM holds
- In each case, class laws are guaranteed to hold, via morphisms
  - so need not be proved specifically for impls

------------------------------------------------------------------------------
2. Denotational semantics and data types

Specify, for each syntactic category C,
- mathematical model [[C]] of meanings
- a semantic function [[·]]C :: C -> [[C]]

[[·]]C semantic functions must be compositional
i.e., must be defined by (mutual) structural recursion

Notation:
- [[·]]_C c : shortened to : [[c]]C (when unambiguous)
- [[·]]_C   : shortened to : [[·]]  (when unambiguous)

Can also apply denotational semantics to data types.

E.g., data type of finite maps from keys to values.
The type is abstract, available only through an explicit
interface that does not reveal its representation.

Central question:
- "What does a map mean?"
- "A map is a representation of what mathematical object?"
- first answer is partial function from values to keys
- be clear about the sense of "partial" here
  - When map queried for missing key, maybe yield an error (semantically ⊥).
  - or, indication that allows for graceful recovery.
  - so model map’s partiality via a success/failure type : Maybe

[[Map k v]] = k -> Maybe v

The meaning function is defined recursively over the abstract syntactic constructors.
This practice transfers directly to algebraic data types.
Doing something different here
- define meaning of an interface, without reference to representation.

The type constraints in the map interface above contribute only to the impl, not the semantics.

A semantics for Map then consists of a model, as given above,
and an interpretation for each member of the interface.

[[ ·             ]] :: Map k v -> (k -> Maybe v)
[[ empty         ]]  = λk      -> Nothing
[[ insert k' v m ]]  = λk      -> if k ≡ k' then Just v else [[m]] k
[[ lookup m    k ]]  = [[m]] k

More functions on maps defined semantically within this model
- left-biased "union" :

unionL :: (...) ⇒ Map k v → Map k v → Map k v
[[ ma ‘unionL‘ mb ]] = λk → [[ma]] k ‘mbLeft‘ [[mb]] k

mbLeft :: Maybe v → Maybe v → Maybe v
mbLeft (Just v)   _ = Just v
mbLeft Nothing  mb0 = mb0

Other functions can be defined in terms of simpler functions.
- e.g., map defined for single key equivalent to:

singleton :: (...) ⇒ k → v → Map k v
singleton k v ≡ insert k v empty

- so explicit semantics for singleton not necessary (i.e., redundant)

meaning of equality (i.e., (≡))?
- semantic : a ≡ b ⇐⇒ [[a]] ≡ [[b]]

Can use lookup to determine if key is in map's domain
(i.e., has a corresponding value/codomain)

How to provide knowledige of domain without having to query?
- domain :: Map k v → { k }
- [[ domain m ]] = { k | [[m]] k !≡ Nothing }

How to provide size of map’s domain (number of defined keys)?
- size :: Map k v → Integer
[[ size m ]] = | [[ domain m ]] |

Above semantics does not give much hint of how to implement size.

------------------------------------------------------------------------------
3. Simplicity

Simpler designs easier to reason about, leading to deeper reasoning.

Benefits: wider applicability, easier use, and better impl performance.

Can we simplify above model of maps?
It has two parts
- (→)   : essential to what a map is
- Maybe : encoding of partiality

Simplify model by factoring out Maybe.

Think of k → Maybe v not as a partial map from k to v
- but as total map from k to Maybe v
- simplify/generalize to total maps to arbitrary types

abstract type TMap k v -- total map
[[ TMap k v ]] = k → v

- In place of an empty Map, we can have a constant TMap
constant :: (...)     ⇒ v        → TMap k v

- Instead of inserting a key/value pair (possibly overwriting),
  update a key (definitely overwriting).
update   :: (...)     ⇒ k        → v → TMap k v → TMap k v

- Sampling a map to a key always has a defined value.
sample   :: (Eq k...) ⇒ TMap k v → k → v

the simpler semantic model above also simplifies the semantic function:

[[ ·             ]] :: TMap k v → (k → v)
[[ constant v    ]]  = λk → v
[[ update k0 v m ]]  = λk → if k ≡ k 0 then v else [[m]] k
[[ sample m k    ]]  = [[m]] k

Can mimic partial maps in terms of total maps:

type Map k v  = TMap k (Maybe v)
empty         = constant Nothing
insert k0 v m = update k0 (Just v) m
lookup m k    = sample m k

What is meaning of unionL of total maps?
- For Map, unionL had to handle the possibility that both maps assign a value to the same key.
- With TMap, there will always be conflicting bindings.

The conflict resolution strategy for Map is specific to Maybe.
Can simplify by passing a combining function:

unionWith               :: (...) ⇒ (a → b → c) → TMap k a → TMap k b → TMap k c
[[ unionWith f ma mb ]]  = λk → f ([[ma]] k) ([[mb]] k)

This unionWith is simpler (semantically) than unionL on Map.

As the type shows, it is also more general, allowing maps of different value types.
- at a cost: client of unionWith must provide a definition of f.
- can provide easy-of-use version by defining Map k v as TMap k (Maybe v):

unionL :: (...) Map k v → Map k v → Map k v
unionL ≡ unionWith mbLeft

This definition of unionL is semantically equivalent to the one in Section 2.

------------------------------------------------------------------------------
4. Type classes

class Monoid o where
  ∅   :: o            -- mempty
  (⊕) :: o → o → o    -- mappend

required identity and associativity laws:

a ⊕ ∅ ≡ a
∅ ⊕ b ≡ b
a ⊕ (b ⊕ c) ≡ (a ⊕ b) ⊕ c

Is Map a b of partial maps a monoid?
- To answer, find definitions for ∅ and (⊕) that satisfy types/laws

instance Monoid (Map k v) where
  ∅   = empty
  (⊕) = unionL

To show that Map satisfies the monoid laws, the crucial lemmas are
- Nothing is a left and right identity for mbLeft
- mbLeft is associative

                              [[ma ⊕ ∅]]
≡ { (⊕) on Map }              [[ma ‘unionL‘ empty]]
≡ { definition of unionL }    [[unionWith mbLeft ma empty]]
≡ { semantics of unionWith }  λk → [[ma]] k ‘mbLeft‘ [[empty]] k
≡ { semantics of ∅ }          λk → [[ma]] k ‘mbLeft‘ Nothing
≡ { lemma }                   λk → [[ma]] k
≡ { η reduction }             [[ma]]

What about TMap?

Since total maps assign a value for every key,
∅ will have to come up with a value from no information.

Similarly, (⊕) will have to combine the two values it finds,
without being given a specific combination function.

Where to get the empty value and combining function? From another monoid.

instance Monoid v ⇒ Monoid (TMap k v) where
  ∅   = constant ∅
  (⊕) = unionWith (⊕)

This instance satisfies the monoid laws, which is easier to show
than for Map. The simplified reasoning testifies to the value of the
simpler semantic model.

However, there is a subtle problem with these two Monoid
instances. Can you spot it? (Warning: Spoiler ahead.)
-}
