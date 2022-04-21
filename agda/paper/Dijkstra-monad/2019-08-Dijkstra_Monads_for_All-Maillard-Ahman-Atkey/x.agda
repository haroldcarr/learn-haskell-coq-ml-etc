module x where

{-
semantic framework for verifying programs with arbitrary monadic side-effects using Dijkstra monads
- monad-like structures indexed by a specification monad

prove
- any monad morphism between a computational monad and a specification monad
  gives rise to a Dijkstra monad
- provides flexibility for obtaining Dijkstra monads tailored to verification task at hand.

show a large variety of specification monads can be obtained by
applying monad transformers to various base specification monads, including
- predicate transformers
- Hoare-style pre/post-conditions

For defining correct monad transformers, provide language
- inspired by Moggi’s monadic metalanguage [Moggi 1989]
- parameterized by a dependent type theory

develop notion of algebraic operations for Dijkstra monads

investigate ways of accommodating effect handlers

------------------------------------------------------------------------------
1 INTRODUCTION

based on Dijkstra monads
- [Protzenko and Parno 2019; Swamy et al. 2016]

Dijkstra monad 'D A w'
- monad-like structure
- classifies effectful computations returning values in A
- specified by w : W A
- W called a specification monad

Prior work used "Dijkstra monad" for both
- the indexed structure D, and
- the index W [Ahman et al. 2017; Jacobs 2014, 2015;Swamy et al. 2013, 2016]

In this paper "Dijkstra monad" exclusively means
- the indexed structure D and
- the term "specification monad" for index W

A specification monad contains predicate transformers mapping postconditions to preconditions.
- e.g., for computations in state monad St A = S → A × S,
  a specification monad is W StA = (A × S → P) → (S → P)
  mapping postconditions
  - here : predicates on final results and states
  to preconditions
  - predicates on initial states
  (P : the internal type of propositions)

Given an arbitrary monadic effect, how to define a specification monad?

Is there a single specification monad that can be associated to each effect?

If not, what are the various alternatives,and what are the constraints on this association
for obtaining a proper Dijkstra monad?

partial answer provided by "Dijkstra Monads for Free" (DM4Free)
- Ahman et al. [2017]
- from a computational monad defined as a term in a metalanguage called DM,
  a (single) canonical specification monad is automatically derived
  through a syntactic translation.
- works for stateful and exceptional computations
- cannot handle several other effects (e.g., IO), due to syntactic restrictions in DM.

To overcome such limitations, observation
- computational monad in DM is a monad transformer applied to identity monad; and
- specification monad obtained by applying this monad transformer to
  continuation monad ContPA = (A → P) → P
- e.g., state
  - specification monad 'W StA' obtained from
    state monad transformer StT M A = S → M(A×S)
- reinterpretation of DM4Free sheds light on its limitations
  - class of supported computational monads is restricted to those that
    can be decomposed as a monad transformer applied to the identity monad
  - rules out effects such as nondeterminism, IO
    for which no proper monad transformer is known
    [Adámek et al. 2012; Bowler et al. 2013; Hyland et al. 2007].

Further, obtaining both computational and specification monads from same monad transformer
introduces a tight coupling.
- In particular, in DM4Free one cannot associate different specification
  monads with a particular effect
- e.g., exception monad 'Exc A = A + E'
  is associated by DM4Free with the specification monad 'W ExcA = ((A + E) → P) → P'
  by applying the exception monad transformer 'ExcT M A = M(A + E)' to ContP
  - This specification monad requires the postcondition to account for both success/failure cases.
  - often desirable
  - at times more convenient to use simpler specification monad ContP directly
    enabling exceptions to be thrown freely,
    without needing to explicitly allow this in specifications.
- e.g., IO, might want specifications that depend on history of interactions with external world,
   or simpler context-free specifications that are more local.
- In general, should have freedom to choose a specification monad that
  - is expressive enough for the verification task at hand
  - simple enough so verification is manageable

Moreover, even for fixed computational monad and fixed specification monad,
there can be more than one way to associate the two in a Dijkstra monad.
- e.g., specify exceptional computations using ContP,
  - could allow all exceptions to be thrown freely (as above)
    - corresponds to a partial correctness interpretation
  - different choice is to prevent any exceptions from being raised
    - corresponds to a total correctness interpretation
- e.g., specifying nondeterministic computations, two interpretations of ContP
  - demonic : postcondition should hold for all possible results [Dijkstra 1975]
  - angelic : postcondition should hold for at least one possible result [Floyd 1967].

KEY IDEA OF THIS PAPER:
- decouple computational monad and specification monad:
- instead of deriving both from same monad transformer as in DM4Free,
- consider them independently
- only require they are related by a monad morphism,
  - mapping between two monads that respects their monadic structure
  - e.g., monad morphism from nondeterministic computations
    - could map a finite set of possible outcomes to a predicate transformer in (A → P) → P
    - given a finite set R of results in A and a postcondition post : A → P
      there are only two reasonable ways to obtain a single proposition:
      - 1. take the conjunction of post v for every v in R (demonic nondeterminism)
      - 2. the disjunction (angelic nondeterminism)
  - e.g., IO, at least two monad morphisms relating IO monad to two different specification monads
          W Fr and W Hist , where E is the alphabet of IO events:
          W FrX = (X × E ∗ → P) → P ←− IO −→ W HistX = (X × E ∗ → P) → (E ∗ → P)
    - both specification monads take postconditions of same type
      - (predicates on final value and the produced IO events)
    - the produced precondition of 'W HistX' has an additional argument 'E ∗'
      - denotes history of interactions (i.e., IO events) with the external world

PAPER MAKES CONTRIBUTIONS:

- Framework for verifying programs with arbitrary monadic effects using Dijkstra monads.
  - Decouples computational monad from the specification monad
    - thus removing all previous restrictions on supported computational monads
    - enables flexibly choosing spec monad and monad morphism suitable for verification task
      - flexibility enables a range of verification styles for
        nondeterminism, IO, and general recursion (not possible in DM4Free)
  - investigate variety of specification monads obtained by applying monad transformers
    to various base monads, including
    - predicate transformers (e.g., weakest preconditions and strongest postconditions)
    - Hoare-style pre- and postconditions

- definition of Dijkstra monads as a monad-like structure
  indexed by a specification monad ordered by precision
  - show that any monad morphism gives rise to a Dijkstra monad
  - show that from any such Dijkstra monad we can recover the monad morphism
  - more generally : construct an adjunction between Dijkstra monads
    and a generalization of monad morphisms, monadic relations,
    which induces the above-mentioned equivalence.

- recast DM4Free as special case new framework
  - introduce SM, metalanguage for defining correct-by-construction monad transformers
    - inspired by DM and Moggi’s monadic metalanguage,
      but parameterized by an arbitrary dependent type theory
      instead of a set of simple types
  - show that under a natural linearity condition
    SM terms give rise to correct-by-construction monad transformers (satisfying all usual laws)
    as well as canonical monadic relations, defined from a logical relation.
    - enables reaping benefits of DM4Free construction when it works well
      (e.g., state, exceptions),
      and to explicitly provide monad morphisms when it does not (e.g., nondeterminism, IO).

- give an account of Plotkin and Power’s algebraic operations for Dijkstra monads
  - show that a monad morphism equips both its specification monad
    and corresponding Dijkstra monad with algebraic operations
  - start investigation of two approaches to effect handlers
    - specification of operations is induced by the handler
      - enables providing a uniform treatment of DM4Free’s
        hand-rolled examples of exception handling, and
      - subsume prior work on weakest exceptional preconditions
    - approach seems inherently limited to exceptions.
  - The second approach, in which operations have to be given specifications upfront,
    enables accommodating handlers for effects other than exceptions,
    (e.g., general recursion, based on McBride’s free monad technique)

- illustrate framework by applying it to the verification of monadic programs in both Coq and F⋆

Paper structure.
- review use of monads in effectful programming and
  the closest related approaches for reasoning about such programs
- overview of paper approach through examples
- how to obtain a range of specification monads by applying monad transformers
  to base specification monads
- show tight/natural correspondence between Dijkstra monads,
  and monadic relations and monad morphisms
- algebraic operations and effect handlers for Dijkstra monads
- implementations of ideas in F⋆ and Coq
- related work
- future work

Supplementary materials
- verification examples and implementation in F⋆
  https://github.com/FStarLang/FStar/tree/dm4all/examples/dm4all
- verification examples and a formalization in Coq
  https://gitlab.inria.fr/kmaillar/dijkstra-monads-for-all
- appendix with further technical details
  https://arxiv.org/abs/1903.01237

------------------------------------------------------------------------------
2 BACKGROUND: MONADS AND MONADIC REASONING

use of monads in effectful programming
related approaches for verifying monadic programs

--------------------------------------------------
2.1 The Monad Jungle Book

effects captured by algebraic structure : computational monad [Benton et al. 2000; Moggi 1989]

- MA   : computations returning values of type A
- retM : A → MA : lifts a value v : A to a computation
- bindM m f : sequentially composes monadic computations m : MA with f : A → MB
- equations specify
  - retM does not have any computational effect
  - bindM is associative

examples of computational monads

- Exceptions: exceptions of type E  is 'Exc A = A + E'

- State: St A = S → A × S

- Nondeterminism: represented by finite set of possible outcomes NDet A = Pfin (A)
  - Returning a value v is provided by the singleton {v}
  - sequencing m with f is forming the union Uv∈m f v
  - operations
    - pick : NDet B = {true, false}
      - nondeterministically chooses a boolean value
    - fail : NDet 0 = ∅
      - unconditionally fails.

- IO
  - iput type I
  - output type O
  - be represented by IO A = µY .A + (I → Y ) + O × Y
    - describes three possible kinds of computations
      - return a value (A)
        - returning v is constructing a leaf
      - expect to receive an input the containue (I -> Y)
      - output and continue (O × Y )
    - sequencing m with f amounts to tree grafting
      - replacing each leaf with value a in m with the tree f a
  - operations
    - input  :     IO I
    - output : O → IO 1

--------------------------------------------------
2.2 Reasoning About Computational Monads

[Hoare 1969] : in imperative setting
- judgments of this logic are Hoare triples { pre } c { post }
- if 'pre' is satisfied, then after running 'c', 'post' post is satisfied
  if 'c' terminates
- pre and post are predicates over states
- adapted to monadic setting by
  - replacing imperative programs c with monadic computations m : M A
  - first proposed in Hoare Type Theory [Nanevski et al. 2008b]
    - where a Hoare monad of the form 'HST pre A post'
      augments state monad over A with
      - precondition 'pre  : S → P' and
      - postcondition post : A × S → P
    - preconditions are still predicates over initial states
    - postconditions are now predicates over both final states and results
   - successfully extended to a few other effects
     [Delbianco and Nanevski 2013; Nanevski et al. 2008a, 2013],
     but no general story on how to define a Hoare monad or
     even just the shape of pre- and postconditions for an arbitrary effect


[Dijkstra 1975] : Dijkstra’s weakest precondition calculus
- for proving properties of imperative programs
- main insight : can typically compute a weakest precondition wp(c, post) such that
  pre ⇒ wp(c, post) if and only if
  { pre } c { post }
  - this enables partly automating the verification process by reducing it
    to a logical decision problem
- Swamy et al. [2013]
  - adopt Dijkstra’s technique to ML programs with state/exceptions in monadic style
  - Dijkstra monad of form 'DST A wp'
    - wp : predicate transformer that specifies behavior of monadic computation
      - represented as functions
        - given
          - a postcondition on the final state and
          - result value of type A or exception of type E
        - calculate a corresponding precondition on the initial state
     - W^ML = ((A + E) × S → P) → (S → P)
              | postconditions|    preconditions
- Swamy et al. [2016] extend this to programs that combine multiple sub-effects
  - compute more efficient weakest preconditions by using Dijkstra monads that
    precisely capture the actual effects of the code,
    instead of verifying everything using W ML above.
  - e.g.,
    - pure computations using Dijkstra monad whose spec have type:
          W PureA = ContP A = (A → P) → P,
    - stateful (but exception-free) have :
          W StA   = (A × S → P) → (S → P).
- Ahman et al.’s [2017] DM4Free
  - shows these disparate specification monads can be derived
    from computational monads defined in their DM metalanguage

observation underlying these techniques
- predicate transformers have a natural monadic structure
  - e.g., PT types
    - 'W Pure' : continuation monad with answer type P
    - 'W St'   : state monad transformer applied to W Pure
    - 'W ML'   : state and exceptions monad transformers applied to W Pure
- this monadic structure supports writing computations that carry their own specification
- next section show it is also the basis for what we call a specification monad

------------------------------------------------------------------------------
3 A GENTLE INTRODUCTION TO DIJKSTRA MONADS FOR ALL

the kinds of specifications most commonly used in practice form ordered monads

on top of this, define effect observations, as monad morphisms between
a computation and a specification monad

how to use effect observations to obtain Dijkstra monads

how to use Dijkstra monads for program verification

--------------------------------------------------
3.1 Specification Monads

Staring point : predicate transformers form monads
- [Ahman etal. 2017; Jacobs 2014, 2015; Swamy et al. 2013, 2016]
- provides a uniform notion of specifications
- we show this is true not only for weakest precondition transformers,
  but also for strongest postconditions, and
  pairs of pre- and postconditions
  - elements of a specification monad can be used to specify properties of some computation,
     e.g.,
     W Pure : specify pure or nondeterministic computations
     W St   : specify stateful computations

specification monads we consider are ordered
- a monad W is ordered when W A is equipped with
  - a preorder ≤W A for each type A, and bindW is monotonic in both arguments:
      ∀(w1 ≤WA w1′ ). ∀(w2 w2′ : A → W B). (∀x : A. w2 x ≤WB w2′x)
     ⇒ bindW w1 w2 ≤WB bindW w ′ w2′
- order enables specifications to be compared as being either more or less precise
  - e.g., for spec monads W Pure and W St, ordering given by
    w1 ≤ w2 : WPure A ⇔ ∀(p : A → P). w2 p ⇒ w1 p
    w1 ≤ w2 : W StA   ⇔ ∀(p : A × S → P)(s : S). w2 p s ⇒ w1 p s
- For 'W Pure' and WSt' to form ordered monads
  - to restrict attention to monotonic predicate transformers
    i.e., those mapping (pointwise) stronger postconditions to stronger preconditions
  - will be assumed implicitly for all predicate transformers (detailed later)

As explained previously, a way to construct specification monads
is to apply monad transformers to existing specification monads
- e.g., applying ExcT M A = M (A + E) to W Pure we get
        WExc A = ExcT WPure A = ((A + E) → P) → P =~ (A → P) → (E → P) → P
  - WExc is a natural specification monad for programs that can throw exceptions,
    transporting a normal postcondition in A → P and an exceptional postcondition in E → P
    to a precondition in P.

--------------------------------------------------
3.2 Effect Observations [Katsumata 2014]

This section relates computational monads to above specifications.

A computational monad can have effect observations into multiple specification monads,
or multiple effect observations into a single specification monad.

runining example : exceptions computational monad 'Exc'

Effect observations are monad morphisms.

computations throwing exceptions : m : Exc A = A + E

to specify m
- specification monad W ExcA = ((A + E) → P) → P
- map m to predicate transformer θExc(m) = λp. p m : WExc A
  applying the postcondition p to the computation m.

The mapping
- θ Exc : Exc → W Exc
- relating
  - computational monad Exc
  - specification monad W Exc
- parametric in return type A
- verifies two properties with respect to the monadic structures of Exc and W Exc
  - returned value is specified by itself: θExc(retExc v) = θExc(inl v) = λp.p(inl v) = retWExc v
  - θ preserves sequencing of computations:
θExc(bindExc(inl v) f) = θExc(f v)   = bindWExc (retWExc v) (θExc ◦ f) = bindWExc θExc(inl v)(θExc ◦f)
θExc(bindExc(inr e) f) = θExc(inr e) = bindWExc θExc(inr e) (θExc ◦ f)

These properties together prove θExc is a monad morphism.

They enable computing specifications from computations compositionally,
e.g., the specification of bind can be computed from the specifications of its arguments.
Leads to definition:

Definition 1 (Effect observation) : effect observation θ
- is a monad morphism from a computational monad M to a specification monad W
- it is a family of maps θA : M A −→ W A
  - natural in A
  - for any
    - v : A
    - m : M A
    - f : A → M B
  - these equations hold:
    - θA(retM v)     = retW v
    - θB(bindM m f ) = bindW(θA m) (θB ◦ f)

Specification monads are not canonical.

Programs that use the exception monad, might call sub-programs that do not raise exceptions.

To ensure sub-programs are pure, could use previous specification monad and restrict
to postconditions that map exceptions to false (⊥)
- so raising an exception would have an unsatisfiable precondition.

simpler solution : taking specification monad W Pure
- define effect observation θ ⊥ : Exc → W Pure
  - θ ⊥ (inl v) = λp. p v
  - θ ⊥ (inr e) = λp. ⊥
- this effect observation
  - gives total correctness interpretation to exceptions
  - which prevents them from being raised at all
- gives effect observations from Exc to both
  - W Exc
  - W Pure

Effect observations are not canonical.

Looking at effect observation θ ⊥
- arbitrary choice when mapping every exception inr e to ⊥
- mapping inr e to true (⊤) instead also gives an effect observation
  - θ ⊤ : Exc → W Pure
  - assigns a trivial precondition to the throw operation,
    giving a partial correctness interpretation:
    - given m : Exc A and postcondition p : A → P
      - if θ ⊤ (m)(p) is satisfiable and m evaluates to inl v then p v holds
      - but m may also raise any exception instead
        Thus, θ ⊥, θ ⊤ : Exc → W Pure
        are two natural effect observations into the same specification monad.
- generally, can vary choice for each exception
- effect observations θ : Exc → W Pure are in one-to-one correspondence with maps E → P

--------------------------------------------------
3.3 Examples of Effect Observations

Monad transformers.

in general, no canonical effect observation for a computational monad

for monad T (Id) (i.e., application of a monad transformer to identity monad)
- can build a canonical specification monad
  - T (W Pure )
- can build a canonical effect observation
  - by lifting the retWPure : Id → W Pure
    through the T transformer
- main idea of reinterpretation of DM4Free approach [Ahman et al. 2017]
- e.g., for exception monad Exc = ExcT(Id) and spec monad W Exc = ExcT (W Pure )
  - effect observation θExc arises as θExc = ExcT(retWPure ) = λm p. p m
- generally
  - for any monad transformer T (e.g. StT, ExcT, StT ◦ ExcT, ExcT ◦ StT) and
  - any specification monad W (not just W Pure , but also any spec monad from 4.1)
  - have a monad morphism
                 T(retWPure)
    - θT : T(Id) -----------> T (WPure)
    providing effect observations for stateful computations with exceptions,
    or for computations with rollback state


Not all computational monads arise as a monad transformer applied to identity monad.
Following examples illustrate such cases.

Nondeterminism.

computational monad NDet admits effect observations to spec monad W Pure

given a nondeterministic computation m : NDet A
- represented as a finite set of possible outcomes, and
- a postcondition post : A → P
- obtain a set P of propositions by applying post to each element of m

two natural ways to interpret P as a single proposition:
- take conjunction ^p∈P p
  - corresponds to weakest precondition such that any outcome of m satisfies post (demonic)
- take disjunction vp∈P p
  - corresponds to weakest precondition such that at least one outcome of m satisfies post (angelic)

both choices lead to monad morphisms θ∀, θ∃ : NDet → WPure
- check that taking conjunction when P = {p} is a singleton is equivalent to p, and
- conjunction of conjunctions ^a∈A ^p∈Pa p is equivalent to conjunction on union of ranges
  - ^p∈Ua∈A Pa p , and
- similarly for disjunctions

Interactive Input-Output TODO

--------------------------------------------------
3.4 Recovering Dijkstra Monads

Dijkstra monads
- provide practical/automatable verification technique
  - in DTT like F⋆ [Swamy et al. 2016]  where they are a primitive notion, and
  - Coq, where they can be embedded via dependent types

Show Dijkstra monad is obtained from
- a computational monad
- a specification monad, and
- an effect observation relating them

show Dijkstra monad used for verification

TODO

-}
