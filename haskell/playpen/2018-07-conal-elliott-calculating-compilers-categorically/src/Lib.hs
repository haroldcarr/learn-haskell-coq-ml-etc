{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MonoLocalBinds          #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeOperators           #-}

module Lib where

import qualified Prelude
import           Protolude       hiding (first, second, swap)
import           Test.HUnit
import qualified Test.HUnit.Util as U

default (Int)

------------------------------------------------------------------------------
{-# ANN module ("HLint: ignore Avoid lambda" :: Prelude.String) #-}
{-# ANN module ("HLint: ignore Redundant lambda" :: Prelude.String) #-}
------------------------------------------------------------------------------

{-
http://conal.net/papers/calculating-compilers-categorically/ccc.pdf
https://github.com/conal/concat
Calculating compilers categorically
(early draft—comments invited)
Conal Elliott
July 26, 2018

------------------------------------------------------------------------------
Abstract

compiling a programming language to a stack-based VM.
Innovation : factor into two phases:
- translation into standard algebraic vocabulary
  - independent of stack machines
  - justified/implemented in a more general setting
  - converts a typed functional language (here, Haskell) to the vocabulary of categories
- stack-oriented interpretation of that vocabulary
  - captures essential nature of stack-based computation
  - independent of the source language
  - calculate a category of stack computations from a simple specification

Other examples of this technique
- generation parallel implementations on GPUs and FPGAs
- incremental evaluation,
- interval analysis
- automatic differentiation

------------------------------------------------------------------------------
1 Stack functions

stack machine for functional computation is like a function f :: a -> b
but it can also use additional storage to help compute f
as long as it does so in a stack discipline
-}

-- stack is a pair
-- with a on top at the start of the computation
-- f a on top at the end of the computation
-- and z as the rest of the stack at the start and finish.
first0 :: (a -> b) -> forall z. ((a, z) -> (b, z))
first0 f (a, z) = (f a, z)

{-
In-between the start and end, the stack may grow and shrink,
but in the end the only stack change is on top.
Note also that first f can do nothing with z other than preserve it.
-}

-- Notion of stack computation as a data type of "stack functions"
-- Converts a Haskell function to StackFun.
newtype StackFun a b = SF (forall z. (a, z) -> (b, z))
stackFun :: (a -> b) -> StackFun a b
stackFun f = SF (first f)

-- Converse: turn a stack function into a regular function
-- initializing the stack to contain a and ()
-- evaluating the contained stack operations
-- discard the final ()
evalStackFun0 :: StackFun a b -> (a -> b)
evalStackFun0 (SF f) a = b where (b, ()) = f (a, ())

-- in more general terms:
evalStackFun :: StackFun a b -> (a -> b)
evalStackFun (SF f) = rcounit . f . runit

-- categorical interface
class    UnitCat      k   where
  lunit   ::      a  `k` ((), a)
  lcounit :: ((), a) `k`      a
  runit   ::      a  `k` (a, ())
  rcounit :: (a, ()) `k`  a

instance UnitCat     (->) where
  lunit           a   =  ((), a)
  lcounit    ((), a)  =       a
  runit           a   =  (a, ())
  rcounit    (a, ())  =   a

xX :: Int
xX  = 1

-- Lemma 1
-- show that evalStackFun is a left inverse for stackFun,
-- i.e., for all f, evalStackFun (stackFun f) = f .
lemma1 :: [Test]
lemma1  = U.tt "lemma1"
  [ evalStackFun (stackFun f)               xX
  -- { definition of stackFun }
  , evalStackFun (SF (first f))             xX
  -- { second definition of evalStackFun }
  , (rcounit ... first f ... runit)         xX
  -- { definition of (...) on functions }
  , (\a -> rcounit (first f (runit a)))     xX
  -- { definition of rcounit on functions }
  , (\a -> rcounit (first f (a, ())))       xX
  -- { definition of first on functions }
  , (\a -> rcounit (f a, ()))               xX
  -- { definition of rcounit on functions }
  , (\a -> f a)                             xX
  -- { η-reduction }
  , f                                       xX
  ]
  (f xX)
 where
  f a = a * 10

-- lemma 2
-- stackFun is surjective
-- i.e., every h :: StackFun a b has the form SF (first f) for some f :: a -> b
-- https://mail.haskell.org/pipermail/haskell-cafe/2018-July/129648.html

-- lemma 3
-- stackFun is injective
-- i.e., every stackFun f = stackFun f' ==> f = f' for all f, f' :: a -> b

-- Corollary 3.1. evalStackFun is the full (left and right) inverse for stackFun

{-
Proof.
Since stackFun is surjective and injective, it has a full (two-sided) inverse,
which is necessarily unique.
Moreover, whenever a category morphism has both left and right inverses,
those inverses must be equal [nLab, 2009–2018, Lemma 2.1].

** THE DEFINITION OF stackFun ABOVE SERVES AS A SPECIFICATION. **

Instead of starting with a function f as suggested by stackFun
we will start with a recipe for f and systematically construct an analogous recipe for stackFun f
- start with a formulation of f in the vocabulary of categories
- require that stackFun preserves the algebraic structure of the category vocabulary

While inconvenient to program in this vocabulary directly, we can instead
automatically convert from Haskell programs [Elliott, 2017].

This approach to calculating correct implementations has also been
used for automatic differentiation [Elliott, 2018].

BENEFIT:
- only need to implement a few type class instances
  (rather than manipulate a syntactic representation)

------------------------------------------------------------------------------

first requirement : stackFun preserve the structure of Category
- i.e.., it is category homomorphism (also called a "functor")
-}

class Category k where
  id    :: a `k` a
  (...) :: (b `k` c) -> (a `k` b) -> (a `k` c)

instance Category (->) where
  id      = identity
  g ... f = g . f

{-
The structure preservation (homomorphism) properties are:
  id                        = stackFun id
  stackFun g ... stackFun f = stackFun (g . f)

The identity and composition operations on the LHS are for StackFun
the ones on the right are for (->) (i.e., regular functions).
Solving these equations for the LHS operations results in a correct instance of Category
for StackFun.

The id equation is already in solved form, so we can use it directly as an implementation.
But first, simplify the equation:
-}
simplifyId :: [Test]
simplifyId  = U.tt "simplifyId"
  [ evalStackFun  (stackFun id)   xX
  -- { definition of stackFun }
  , evalStackFun (SF (first id))  xX
  -- { property of first and id }
  , evalStackFun (SF        id)   xX
  ]
  (identity xX)

-- The (...) equation
simplifyLHS :: [Test]
simplifyLHS  = U.tt "simplifyLHS"
  [ evalStackFun  (stackFun g  ...  stackFun f)  xX
  -- { definition of stackFun }
  , evalStackFun (SF (first g) ... SF (first f)) xX
  ]
  ((g . f) xX)
 where
  f x = x * 10
  g x = x -  5

simplifyRHS :: [Test]
simplifyRHS  = U.tt "simplifyRHS"
  [ evalStackFun  (stackFun (g ...       f))   xX
  -- { definition of stackFun }
  , evalStackFun (SF (first (g ...       f)))  xX
  -- { property of first and (...) }
  , evalStackFun (SF (first  g ... first f))   xX
  ]
  ((g . f) xX)
 where
  f x = x * 10
  g x = x -  5

-- The simplified specification:
simplifiedLHSSpecification :: [Test]
simplifiedLHSSpecification  = U.tt "simplifiedLHSSpecification"
  [ evalStackFun (SF (first g) ... SF (first f))   xX
  , evalStackFun (SF (first g  ...     first f))   xX
  ]
  ((g . f) xX)
 where
  f x = x * 10
  g x = x -  5

-- Strengthen this equation by generalizing from first g and first f to arbitrary functions
-- (also called "g" and "f" and having the same types as first g and first f)
strengthenedLHSSpec :: [Test]
strengthenedLHSSpec  = U.tt "strengthenedLHSSpec"
  [ evalStackFun (SF  g ... SF f)   xX
  , evalStackFun (SF (g ...    f))  xX
  ]
  (fst ((g . f) (xX, ())))
 where
  f (x,z) = (x*10, z)
  g (x,z) = (x- 5, z)

-- generalized/strengthened condition is in solved form
-- use it and simplified id to define category operations
instance Category StackFun where
  -- the identity       stack function  is the identity       function  on stacks
  id            = SF id
  -- the composition of stack functions is the composition of functions on stacks
  SF g ... SF f = SF (g ... f)

-- other useful categorical classes (handled in the same manner as id above)
class AssociativeCat      k   where
  rassoc :: ((a, b), c)  `k` (a, (b,  c))
  lassoc :: (a, (b,  c)) `k` ((a, b), c)
class BraidedCat k where
  swap   :: (a, b)       `k` (b, a)

instance AssociativeCat (->) where
  rassoc    ((a, b), c)   =  (a, (b,  c))
  lassoc    (a, (b,  c))  =  ((a, b), c)
instance BraidedCat     (->) where
  swap      (a, b)        =  (b, a)

-- The associated homomorphism equations are in solved form and can serve as definitions:
instance AssociativeCat StackFun where
  rassoc = stackFun rassoc
  lassoc = stackFun lassoc
instance BraidedCat StackFun where
  swap   = stackFun swap

{-
------------------------------------------------------------------------------
1.2 Parallel composition (products)

The purpose of a stack is to sequentialize computations.
So far, above is only sequential --- the stack is not used.
Nonsequential computation comes from parallel (aka products) composition,
as embodied in the "cross" operation in the MonoidalP interface:
-}
class MonoidalP k where
  (.*.) :: (a `k` c)   -> (b `k` d) -> ((a, b) `k` (c, d))
  -- (more general version of first0 above)
  first  :: Category k => (a `k` c) -> ((a, b) `k` (c, b))
  first  f = f .*. id

  second :: Category k => (b `k` d) -> ((a, b) `k` (a, d))
  second g = id .*. g

instance MonoidalP (->) where
  (.*.) f g = \(a, b) -> (f a, g b)

-- The following law holds for all monoidal categories [Gibbons, 2002, Section 1.5.1]:
-- (f × g)  ◦ (p  × q) = (f ◦ p)  × (g ◦ q)
-- taking g = id and p = id, and renaming q to "g":
-- ???
-- (f × id) ◦ (id × g) = (f ◦ id) × (id ◦ g)
-- simplify
-- f  x             g  =  f       x       g
monidalLaw1 :: [Test]
monidalLaw1  = U.tt "monidalLaw1"
  [ (first f ... second g)  (1,2)
  , (      f .*.        g)  (1,2)
  ]
  (f 1, g 2)
 where
  f a = a * 10
  g b = b -  5

monidalLaw2 :: [Test]
monidalLaw2  = U.tt "monidalLaw2"
  [ (second g ... first f)  (1,2)
  , (       f .*.       g)  (1,2)
  ]
  (f 1, g 2)
 where
  f a = a * 10
  g b = b -  5

--can define

secondInTermsOfFirst :: [Test]
secondInTermsOfFirst  = U.tt "secondInTermsOfFirst"
  [ second g                    (1,2)
  , (swap ... first g ... swap) (1,2)
  ]
  (1, g 2)
 where
  g b = b - 5

firstInTermsOfSecond :: [Test]
firstInTermsOfSecond  = U.tt "firstInTermsOfSecond"
  [ first f                      (1,2)
  , (swap ... second f ... swap) (1,2)
  ]
  (f 1, 2)
 where
  f a = a * 10

-- Because to these relationships
-- any two of (×), first, and second can be defined in terms of the other.
-- Useful to calculate a definition of first on StackFun, and then define (×):
productInTermsOf :: [Test]
productInTermsOf  = U.tt "productInTermsOf"
  [ (      f .*.                g)          (1,2)
  , (first f ... second         g)          (1,2)
  , (first f ... swap ... first g ... swap) (1,2)
  ]
  (f 1, g 2)
 where
  f a = a * 10
  g b = b -  5

{-
Therefore only need to define first.
Can be done by solving the corresponding homomorphism property, i.e.,
   first (stackFun  f)  = stackFun  (first f)

Equivalently (filling in the definition of stackFun),
   first (SF (first f)) = SF (first (first f))

want to simplify the first (first f)

examine the types involved:

f               ::      a          ->  c
first        f  ::      a × b      ->  c × b
first (first f) :: ∀z. (a × b) × z -> (c × b) × z

To reshape this computation into a stack function, temporarily move b aside by re-associating:
    first (first f)
= { definition of first on (->) }
    λ((a, b), z) -> ((f a, b), z)
= { definition of lassoc, rassoc, and first on (->) }
    lassoc ◦ first f ◦ rassoc

the required homomorphism equation for first is therefore equivalent to:
    first (SF (first f)) = SF (lassoc ◦ first f ◦ rassoc)

generalizing from first f, obain the following sufficient condition:
    first (SF        f)  = SF (lassoc ◦       f ◦ rassoc)

Since this generalized equation is in solved form,
it can be used as a definition, expressing second and (×) in terms of it:
-}
instance MonoidalP StackFun where
  f .*. g      = first f ... second g
  first (SF f) = SF (lassoc ... f ... rassoc)
  second g     = swap ... first g ... swap
{-
This sequentialized computation corresponds to right-to-left evaluation of arguments.
left-to-right evaluation can be done by reformulating parallel composition as
    f × g = second g ◦ first f
-}
stackEvolves1 :: [Test]
stackEvolves1  = U.tt "stackEvolves1"
  [               first swap (  (1,  2),   ()  ) == (  (2,    1), ()  )
  ,               rassoc     (  (2,  1),   ()  ) == (   2,   (1 , ()) )
  ,               first g    (   2, (1,    ()) ) == ( g 2,   (1 , ()) )
  ,               lassoc     ( g 2, (1,    ()) ) == ((g 2,    1), ()  )
  ,               first swap ((g 2,  1),   ()  ) == (  (1,  g 2), ()  )
  ,               rassoc     (  (1,  g 2), ()  ) == (   1, (g 2 , ()) )
  ,               first f    (   1, (g 2,  ()) ) == ( f 1, (g 2 , ()) )
  ,               lassoc     ( f 1, (g 2,  ()) ) == ((f 1  ,g 2), ()  )
  --                                                note results are products
  ,                          ((f 1,  g 2), ()  ) == (((0, ())
                                                           ,(20,()))
                                                                , ()  )
  ]
  True
 where
  f x = (x- 1, ())
  g x = (x*10, ())

stackEvolves2 :: [Test]
stackEvolves2  = U.tt "stackEvolves2"
  [               first swap (  (1,   2),  ()  ) == (  (2,    1), ()  )
  ,               rassoc     (  (2,   1),  ()  ) == (   2,   (1 , ()) )
  , evalStackFun (first g)   (   2,  (1,   ()) ) == (  20,   (1 , ()) )
  ,               lassoc     (  20,  (1,   ()) ) == ( (20,    1), ()  )
  ,               first swap ( (20,   1),  ()  ) == (  (1,   20), ()  )
  ,               rassoc     (  (1,  20),  ()  ) == (   1,  (20 , ()) )
  , evalStackFun (first f)   (   1, (20,   ()) ) == (   0,  (20 , ()) )
  ,               lassoc     (   0, (20,   ()) ) == (  (0,   20), ()  )
  ]
  True
 where
  f = stackFun (\x -> x - 1)
  g = stackFun (*10)

stackEvolves3 :: [Test]
stackEvolves3  = U.t "stackEvolves3"
  (evalStackFun
    (lassoc ... first f ... rassoc ... first swap ... lassoc ... first g ... rassoc ... first swap)
    ((1,2), ())
  )
  ((0, 20) ,())
 where
  f = stackFun (\x -> x - 1)
  g = stackFun (*10)

{-
Operationally, first g and first f stand for stack-transformation sub-sequences.
Note the final stack state is equal to first (f × g) ((a, b), z) as required.
We have flattened (under the SF constructor) into purely
sequential compositions of functions of three forms:
- first p for simple functions p
- rassoc
- lassoc
(rassoc/lassoc always come in balanced pairs)

------------------------------------------------------------------------------
1.3 Duplicating and destroying information
-}

class Cartesian k where
  exl :: (a, b) `k` a
  exr :: (a, b) `k` b
  dup ::  a     `k` (a, a)

instance Cartesian (->) where
  exl   = fst
  exr   = snd
  dup x = (x, x)

-- the required homomorphism properties are already in solved form
instance Cartesian StackFun where
  exl = stackFun exl
  exr = stackFun exr
  dup = stackFun dup
{-
These operations are used in the translation from λ-calculus (e.g., Haskell) to categorical form.
The projections exl and exr are used to translate pattern-matching on pairs.
Duplication is used for translation of pair formation and application expressions,
use the "fork" operation [Elliott, 2017, Section 3]:
-}
(.^.) :: (Category k, MonoidalP k, Cartesian k)
      => (a `k` c) -> (a `k` d)
      -> (a `k` (c, d))
f .^. g = (f .*. g) ... dup
{-
------------------------------------------------------------------------------
1.4 Conditional composition (coproducts)

coproducts (sums) : (dual of  MonoidalP and Cartesian)
-}
class MonoidalS k where
  (.+.) :: (a `k` c) -> (b `k` d) -> (Either a b `k` Either c d)

instance MonoidalS (->) where
  f .+. g = \case Left  a -> Left  (f a)
                  Right b -> Right (g b)

class Cocartesian k where
  inl :: a `k` Either a b
  inr :: b `k` Either a b
  jam :: Either a a `k` a

instance Cocartesian (->) where
  inl           = Left
  inr           = Right
  jam (Left  a) = a
  jam (Right a) = a

-- the homomorphism properties are satisfied
instance Cocartesian StackFun where
  inl = stackFun inl
  inr = stackFun inr
  jam = stackFun jam
{-
Just as the (.^.) ("fork") operation for producing products is defined via (×) and dup,
so is the (.|.) ("join") operation ((for consuming coproducts/sums) defined via (+) and jam:
-}
(.|.) :: (Category k, MonoidalS k, Cocartesian k)
      => (a `k` c) -> (b `k` c)
      -> (Either a b `k` c)
f .|. g = jam ... (f .+. g)

-- categorical co/products are related in distributive categories [Gibbons, 2002, Section 1.5.5]
class (Cartesian k, Cocartesian k) => Distributive k where
  distl   :: (a, Either u v) `k` Either (a, u) (a, v)
  distr   :: (Either u v, b) `k` Either (u, b) (v, b)
  undistr :: Either (u, b) (v, b) `k` (Either u v, b)

instance Distributive (->) where
  distl (a, Left  u) = Left  (a, u)
  distl (a, Right v) = Right (a, v)
  distr (Left  u, b) = Left  (u, b)
  distr (Right v, b) = Right (v, b)
  undistr = \case Left  (u, b) -> (Left  u, b)
                  Right (v, b) -> (Right v, b)

-- (.|.) and distl ops are used to translate multi-constructor case expressions to categorical form
-- [Elliott, 2017, Section 8].
instance Distributive StackFun where
  distl   = stackFun distl
  distr   = stackFun distr
  undistr = stackFun undistr

-- define MonoidalS instance for StackFun
-- (using the MonoidalS and Distributive instances for (->))
-- Theorem 4 (Proved in Appendix A.4).
-- Given the instance definition above, stackFun is a MonoidalS homomorphism.
instance MonoidalS StackFun where
  SF f .+. SF g = SF (undistr ... (f .+. g) ... distr)
