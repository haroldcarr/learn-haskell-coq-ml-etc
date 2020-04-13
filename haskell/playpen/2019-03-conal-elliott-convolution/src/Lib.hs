{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib (runTests) where

import           Data.Typeable
import qualified Prelude
import           Protolude       hiding (Endo, Monoid (..), Semiring (..), zero,
                                  (<>))
import           Test.HUnit
import qualified Test.HUnit.Util as U

default (Int)

{-
https://arxiv.org/abs/1903.10677
Generalized Convolution and Efficient Language Recognition
Conal Elliott
2019-03
-}

------------------------------------------------------------------------------
-- Monoids

class Monoid a where
  ε    :: a
  (<>) :: a -> a -> a
infixr 6 <>

instance Monoid [a] where
  ε    = []
  (<>) = (++)

instance Monoid Int where
  ε    = 0
  (<>) = (+)

{-
Def 1. A function h from one monoid to another is called a monoid homomorphism when it satisfies
  h ε        = ε
  h (u <> v) = h u <> h v

Example from Monoid [Int] -> Monoid Int where h == length
-}

t1 :: [Test]
t1  = U.tt "monoid homomorphism : empty"
  [ length (ε  :: [Int])
  , length ([] :: [Int])  -- ε on [a ]
  , 0                     -- length definition
  , ε :: Int ]            -- 0 on N
  0

t2 :: [Test]
t2  = let u = [1]; v = [2,3::Int] in U.tt "monoid homomorphism"
  [ length (u <>        v)
  , length (u ++        v)  -- <> on [a]
  , length  u +  length v   -- length definition and induction
  , length  u <> length v ] -- <> on N
  3

-- another monoid : endofunctions : functions that take a val of type a and return a val of type a

newtype Endo a = Endo (a -> a)

instance Monoid (Endo a) where
  ε                = Endo identity
  Endo g <> Endo f = Endo (g . f)
{-
The identity and associativity monoid laws follow from the identity and associativity category laws,
so we can generalize to endomorphisms, i.e., morphisms from an object to itself in any category.

A generalization of Cayley’s theorem states that every monoid is isomorphic
to a monoid of endofunctions.

That embedding is useful for turning quadratic-time algorithms linear.
-}
toEndo :: Monoid a => a -> Endo a
toEndo         a  = Endo (\z -> a <> z )

fromEndo :: Monoid a => Endo a -> a
fromEndo (Endo f) = f ε

{-
-- The toEndo embedding provides another example of a monoid homomorphism:

  toEndo (ε :: Int)
  Endo (\z -> ε <> z) -- toEndo definition
  Endo (\z -> z)      -- monoid law
  ε                   -- id on Endo a
-}
t3 :: [Test]
t3  = U.tt "Endo monoid homomorphism : type empty"
  [  typeOf (toEndo (ε :: Int)   :: Endo Int)
  ,  typeOf (Endo (\z -> ε <> z) :: Endo Int)
  ,  typeOf (Endo (\z -> z)      :: Endo Int)
  ] (typeOf (ε :: Endo Int))
{-# ANN t3 ("HLint: ignore Use id" :: Prelude.String) #-}
{-
  toEndo (a <> b)
  Endo (\z -> (a <>  b) <> z)                 -- toEndo definition
  Endo (\z ->  a <> (b  <> z))                -- monoid law
  Endo ((\z -> a <> z) . (\z -> b <> z))      -- (◦) definition
  Endo ( \z -> a <> z) <> Endo (\z -> b <> z) -- (<>) on Endo a
  toEndo a <> toEndo b                        -- toEndo definition (twice)
-}
t4 :: [Test]
t4  = let a = [1::Int]; b = [2::Int] in U.tt "Endo monoid homomorphism type"
  [  typeOf (toEndo       (a <>                   b)        :: Endo [Int])
  ,  typeOf (Endo ( \z -> (a <>                   b) <> z)  :: Endo [Int])
  ,  typeOf (Endo ( \z ->  a <>                  (b  <> z)) :: Endo [Int])
  ,  typeOf (Endo ((\z ->  a <> z) .       (\z -> b  <> z)) :: Endo [Int])
  ,  typeOf (Endo ( \z ->  a <> z) <> Endo (\z -> b  <> z)  :: Endo [Int])
  ] (typeOf (toEndo        a <>            toEndo b         :: Endo [Int]))

t3a :: [Test]
t3a  = U.t "Endo monoid homomorphism : empty"
  (fromEndo (toEndo (ε :: Int)))
  0

t4a :: [Test]
t4a  = let u = [1]; v = [2,3::Int] in U.tt "Endo monoid homomorphism"
  [ fromEndo (toEndo (u <> v))
  , fromEndo (toEndo u <> toEndo v) ]
  [1,2,3]
{-
-- Additive Monoids

Monoid <> must be associative, but not necessarily commutative.

Additive monoids must be commutative.  zero/.+. (instead of Monoid's ε/<>)
-}

class Additive b where
  zero  :: b
  (.+.) :: b -> b -> b
infixl 6 .+.

instance Additive Int where
  zero  = 0
  (.+.) = (+)

-- functions with pointwise addition
-- any domain 'a'
-- any additive codomain 'b'
instance Additive b => Additive (a -> b) where
  zero _  = zero
  f .+. g = \a -> f a .+. g a

t5 :: [Test]
t5  = U.tt "Additive Int - associative"
  [   zero .+. (1  .+. (2  .+. 3))
  , ((zero .+.  1) .+.  2) .+. 3::Int ]
  6

t5a :: [Test]
t5a  = U.tt "Additive Int - commutative"
  [ ( zero  .+. (3  .+. 2)) .+. 1
  , ((zero .+.  1) .+.  2)  .+. 3::Int ]
  6

t6 :: [Test]
t6  = U.tt "Additive Int (+) .+. (*)"
  [  (      (+)     .+. (*)  ) 1 2
  -- (\a -> (+) a   .+. (*) a) :: (Additive a, Num a) => a -> a -> a
  ,  (\a -> (+) a   .+. (*) a) 1 2  -- def Additive
  ,  (      (+) 1   .+. (*) 1)   2  -- apply
  ,  (\a -> (+) 1 a .+. (*) 1 a) 2  -- def Additive
  ,         (+) 1 2 .+. (*) 1 2     -- apply
  ,             3   .+.     2       -- apply (+) and (*)
  ]
  (5::Int)                          -- Additive Int
{-
curry is Additive

  curry            0
  curry (λ(x, y) → 0) -- 0 on functions
  λx → λy        → 0  -- curry definition
  λx             → 0  -- 0 on functions
  0                   -- 0 on functions
-}
t7 :: [Test]
t7  = U.tt "Additive curry zero type"
  [  typeOf (curry                  zero  :: Int->Int->Int)
  ,  typeOf (curry (\(_x,    _y) -> zero) :: Int->Int->Int)
  ,  typeOf ((       \_x -> \_y  -> zero) :: Int->Int->Int)
  ,  typeOf ((       \_x ->         zero) :: Int->Int->Int)
  ] (typeOf                        (zero  :: Int->Int->Int))
{-# ANN t7 ("HLint: ignore Collapse lambdas" :: Prelude.String) #-}
{-# ANN t7 ("HLint: ignore Use const" :: Prelude.String) #-}

t7a :: [Test]
t7a  = U.t "Additive curry zero apply"
  (curry zero (1::Int) (2::Int) :: Int)
  zero

instance Additive (a, b) where
  zero        = panic "Additive (a,b)"
  (.+.)       = panic "Additive (a,b)"
instance (Num a, Num b) => Num (a, b) where
  (-)         = panic "Num (a,b)"
  (+)         = panic "Num (a,b)"
  (*)         = panic "Num (a,b)"
  abs         = panic "Num (a,b)"
  signum      = panic "Num (a,b)"
  fromInteger = panic "Num (a,b)"
{-
  curry (f .+. g)
  curry (λ (x , y) → f (x , y) .+. g (x , y)) -- (.+.) on functions
  λ x → λ y → f (x , y) .+. g (x , y)         -- curry definition
  λ x → λ y → curry f x y .+. curry g x y     -- curry definition (twice)
  λ x → curry f x .+. curry g x               -- (.+.) on functions
  curry f .+. curry g                         -- (.+.) on functions
-}
type ACT a b = a -> b -> (a, b) -> (a, b)

t7b :: [Test]
t7b  = let f = (+)::Num a => a -> a -> a; g = (*)::Num a => a -> a -> a
        in U.tt "Additive curry ((+) .+. (*)) type"
  [  typeOf  (curry                     (f        .+.       g)        :: ACT Int Int)
  ,  typeOf  (curry (\(x,    y) ->       f (x, y) .+.       g (x, y)) :: ACT Int Int)
  ,  typeOf         ((\x -> \y  ->       f (x, y) .+.       g (x, y)) :: ACT Int Int)
  ,  typeOf         ((\x -> \y  -> curry f  x  y  .+. curry g  x  y)  :: ACT Int Int)
  ,  typeOf         ((\x ->        curry f  x     .+. curry g  x)     :: ACT Int Int)
  ] (typeOf ((curry                      f        .+. curry g)        :: ACT Int Int))
{-# ANN t7b ("HLint: ignore Collapse lambdas" :: Prelude.String) #-}
{-# ANN t7b ("HLint: ignore Use const" :: Prelude.String) #-}

-- Additive monoids have their form of homomorphism: ...

------------------------------------------------------------------------------
-- Semirings

{-
two monoids on natural numbers : +/0; */1
* distributes over +
x * 0 = 0 -- annihilates

linear endofunctions and various representations (e.g., square matrices) forms a monoid
via addition and via composition, with
- composition distributing over addition (addition cummutes)
- composition with zero yielding zero    (addition cummutes)
- multiplication commutes
- composition does not commutes
-}

class Additive b => Semiring b where
  one   :: b
  (.*.) :: b -> b -> b
infixl 7 .*.

instance Semiring Int where
  one   = 1
  (.*.) = (*)

-- Additive extended with multiplicative monoid, distribution, and annihilation
t7c :: [Test]
t7c  = let p,q,r,w,u,v::Int
           p = 3; q = 4; r = 5; w = 6; u = 7; v = 8
        in U.tt "Semiring laws"
  [          u .*. 0  == 0
  ,          0 .*. v  == 0
  ,          1 .*. v  == v
  ,          u .*. 1  == u
  , (u .*.  v) .*. w  == u .*. (v .*. w )
  ,  p .*. (q  .+. r) == p .*. q .+. p .*. r
  , (p .+.  q) .*. r  == p .*. r .+. q .*. r
  ] True
{-
numbers and various linear endofunction representations form semirings

semiring of booleans, with disjunction as addition and conjunction as multiplication (roles arbitary)
-}
instance Additive Bool where
  zero  = False
  (.+.) = (||)

instance Semiring Bool where
  one   = True
  (.*.) = (&&)
{-
Def 3. A function h from one semiring to another is a semiring homomorphism if it is an additive
       monoid homomorphism (Definition 2) and satisfies
  h 1        = 1
  h (u * v ) = h u * h v
-}
-- a semiring homomorphism
positive :: Int -> Bool
positive n0 = n0 > 0

t8 :: [Test]
t8  = let m = 3; n = 5 in U.tt "semiring homomorphism"
  [ (positive 0 == False) == (positive 0 == zero)
  , (positive 1 == True)  == (positive 1 == one)
  , (positive (m .+. n) == positive m || positive n) == positive m .+. positive n
  , (positive (m .*. n) == positive m && positive n) == positive m .*. positive n
  ] True
{-# ANN t8 ("HLint: ignore Redundant ==" :: Prelude.String) #-}

-- TODO Theorem 2 (Proved in Appendix A.2). Currying and uncurrying are semiring homomorphisms.

------------------------------------------------------------------------------
-- Star Semirings

{-
Semirings enable all *finite* combinations of addition, zero, multiplication, and one.

Useful to form infinite combinations, particularly in the form of Kleene’s “star” (or “closure”)


p ∗ = sigma p^i -- where p^0 = 1, and p^n+1 = p ∗ p^n
        i

Another characterization is as a solution to either of the following semiring equations:

p ∗ = 1 + p ∗ p ∗
p ∗ = 1 + p ∗ ∗ p

which we will take as a laws for a new abstraction, as well as a default recursive implementation:
-}
class Semiring b => StarSemiring b where
  dotStar         :: b -> b
  default dotStar :: b -> b
  dotStar p        = one .+. p .*. dotStar p
{-
alternative impls, e.g., when subtraction and division:  p∗ = (1 − p)^−1

Def 4. A function h from one star semiring to another is a star semiring homomorphism if it is a
semiring homomorphism (Def 3) and satisfies the additional property h (p*) = (h p)*.

closed semiring example
-}
instance StarSemiring Bool where dotStar _ = one -- = 1 ∨ (b ∧ b*)
{-
useful property : recursive affine equations have solutions
Lemma 3. In a star semiring, the affine equation p = b + m ∗ p has solution p = m ∗ ∗ b.
Proof.
        b +  m ∗ (m^*  ∗ b)
 =      b + (m ∗  m^*) ∗ b -- associativity of (∗)
 =  1 ∗ b +  m ∗  m^*  ∗ b -- identity for (∗)
 = (1 +      m ∗  m^*) ∗ b -- distributivity
 =                m^*  ∗ b -- star semiring law
-}

------------------------------------------------------------------------------
-- Semimodules

------------------------------------------------------------------------------
-- Function-like Types and Singletons

------------------------------------------------------------------------------

runTests :: IO Counts
runTests =
  runTestTT $ TestList $
  t1 ++ t2 ++ t3 ++ t3a ++ t4 ++ t4a ++ t5 ++ t5a ++ t6 ++ t7 ++ t7a ++ t7b ++ t7c ++
  t8
