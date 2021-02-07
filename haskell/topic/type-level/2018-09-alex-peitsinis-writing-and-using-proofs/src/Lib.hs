{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Lib where

import Data.Type.Equality
import GHC.Base (Type)

{-
Writing and using proofs in Haskell
September 27,2018

see also: “Proving stuff in Haskell” by Mads Buch
- uses propositional equality to prove a theorem by showing
  that two haskell types are actually the same type
  e.g., a+b=b+a
-}

------------------------------------------------------------------------------
-- Peano numbers

data Nat
  = Z     -- base
  | S Nat -- inductive ("recursive")

{-
values  Z and  S  Z have type : Nat

kinds  'Z and 'S 'Z have kind : Nat (not *)  -- via DataKinds
- they do not have any inhabitants
- can circumvent that restriction (below))

could also write:

data Z
data S a

but ability to unite those two types under a kind

The Nat definition enables writing Nat in GADT and type family declarations

------------------------------------------------------------------------------
Type equality : two types are equal : propositional equality from Data.Type.Equality

data a :~: b where
  Refl :: a :~: a

------------------------------------------------------------------------------
Type-level functions

closed type family
-}

type family a + b where
  a + 'Z   =     a       -- (1)
  a + 'S b = 'S (a + b)  -- (2)

{-
The kind of this type family is deduced to be Nat -> Nat -> Nat

------------------------------------------------------------------------------
a proof
-}

-- type checker uses (1) to infer Z + Z = Z, where a = Z
testEquality1 :: ('Z + 'Z) :~: 'Z
testEquality1  = Refl

-- testEquality 2 and 3 prove PLUS RIGHT IDENTITY

-- type checker uses (1) to infer a + Z = Z, where a = a
testEquality2 :: (a + 'Z) :~: a
testEquality2  = Refl

-- type checker uses (2)
testEquality3 :: (a + 'S b) :~: 'S (a + b)
testEquality3  = Refl

{-
PLUS LEFT IDENTITY needs more work.

prove Z + a = a via induction on a

initial thought:

baseCase :: (Z + Z) :~: Z
baseCase  = Refl

induction :: (Z + S a) :~: S a
induction  = ??? -- need to be able to use one proof in order to prove another one

Data.Type.Equality

-- given Refl proving a is same type as b
-- and something of type r that could use the knowledge that a is b
-- then type checker can infer r is well typed
gcastWith :: (a :~: b) -> (a ~ b => r) -> r
-}

helper :: ('S 'Z + 'S 'Z) :~: 'S ('S 'Z + 'Z) -- this proposition is proved
helper  = Refl

-- above proposition used to prove this proposition
useHelper :: ('S 'Z + 'S 'Z) :~: 'S ('S 'Z)
useHelper  = gcastWith helper Refl

{-

('S 'Z + 'S 'Z) :~: 'S ('S 'Z + 'Z)   -> (Refl  => r) -> 'S ('S 'Z)
                 |                          |             |
                 v                          v             v
             (a :~: b)                -> (a ~ b => r) ->  r

Refl in useHelper says : I have S Z + S Z equal to S (S Z + Z)
- from (1) : S Z + Z is S Z

above is for concrete values

need to prove forall : recurse to base case

need to reflect a type to a value

Idris : {a:Nat} -> (Z + S a) :~: S a

------------------------------------------------------------------------------
Singletons : 1-1 mapping between values and types
-}

data SNat :: Nat -> Type where
  SZ ::           SNat  'Z
  SS :: SNat a -> SNat ('S a)

{-
type of         SZ   is SNat       Z
type of SS (SS (SZ)) is SNat (S (S Z))

------------------------------------------------------------------------------
prove PLUS LEFT IDENTITY using SNat with induction

base case type checks because
- Refl value says (Z + Z) ~ Z -- via  (1)
- 'a' is Z is because 1st parameter is SZ :: SNat Z, so a ~ Z
- similar to Idris: {a:Nat} -> (Z + S a) :~: S a
-}

plusLeftId :: SNat a -> ('Z + a) :~: a
-- base case : a ~ Z
plusLeftId SZ = Refl

{-
induction step : need to now prove for every type S a, assuming proof stands for a

use the singleton for the successor: SS

plusLeftId :: SNat a -> (Z + a) :~: a
plusLeftId SZ     = Refl
plusLeftId (SS n) = Refl

• Could not deduce: ('Z + n) ~ n
  from the context: m ~ 'S n
    bound by a pattern with constructor:
               SS :: forall (m :: Nat). SNat m -> SNat ('S m),
             in an equation for ‘plusLeftId’

need to tell type checker that the equality holds for n

use gcastWith
-}
plusLeftId (SS n) = gcastWith (plusLeftId n) Refl

{-
    (Z + n) :~: n  ->  Refl        -> n
             |           |            |
             v           v            v
    (a      :~: b) -> (a ~ b => r) -> r

Here, Refl is the r, and, according to the error message, we need a ~ (Z + n) and b ~ n, or even better (Z + n) :~: n. But that’s the result type of plusLeftId itself, if we called it with n:
-}

callPlusLeftId ::             'S ('S ('S 'Z))
                          :~: 'S ('S ('S 'Z))
callPlusLeftId  = plusLeftId (SS (SS (SS SZ)))

{-
------------------------------------------------------------------------------
rewrite proof of PLUS RIGHT IDENTITY and axioms automatically got from type family, to use values:
-}

given1        :: SNat a -> (a + 'Z) :~: a
given1 _       = Refl

given2        :: SNat a -> SNat b -> (a + 'S b) :~: 'S (a + b)
given2 _ _     = Refl

plusRightId   :: SNat a -> (a + 'Z) :~: a
plusRightId    = given1

plusRightId'  :: SNat a -> (a + 'Z) :~: a
plusRightId' n = gcastWith (given1 n) Refl

{-
------------------------------------------------------------------------------
Proofs with multiple steps

left identity in mathematical notation

        0 + S(a)
    = S(0 +   a)   -- by (2)
    = S(      a)   -- by the induction hypothesis

in Haskell

         Z + (S n)
    = S (Z +    n) -- using type family definition
    = S         n  -- proved what is inside the S in previous line inductively

------------------------------------------------------------------------------
proof : ASSOCIATIVITY OF ADDITION : (a +  b) + c
                                   = a + (b  + c)
-}
(!+) :: SNat n -> SNat m -> SNat (n + m)
n !+  SZ    =     n
n !+ (SS m) = SS (n !+ m)
{-
BASE CASE, given a and b, c = 0

           (a +  b) + 0
step1:      a +  b        -- by (1) for a + b
step2:      a + (b  + 0)  -- by (1) for b

using x and y to clarify not talking about a and b: just defining steps based on two numbers

step1
- need to use (1) : must pass something of type SNat n where n is type x + y (using singletons !+
step 2
- extract the part of the expression that we want to “transform” to something else
- then use another proof function to drive the process

want type  ((a + b) + Z) :~: (a + (b + Z))
have types ((a + b) + Z) :~: (a + b)
            (a + b)      :~: (a + (b + Z))
i.e., if type x same as type y, and type y same as type z, then type x same as type z
-}

(==>) :: a :~: b -> b :~: c -> a :~: c -- transitive property of :~: (propositional equality).
Refl ==> Refl = Refl

plusAssoc
  :: SNat a
  -> SNat b
  -> SNat c
  -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ =
  let
    step1     :: SNat x -> SNat y -> ((x + y) + 'Z) :~: (x + y)
    step1  x y = gcastWith (given1 (x !+ y)) Refl -- (1)

    step2     :: SNat x -> SNat y ->  (x + y) :~: (x + (y + 'Z))
    step2 _x y = gcastWith (given1       y)  Refl -- (1)
   in step1 a b ==> step2 a b

{-
------------------------------------------------------------------------------
INDUCTIVE CASE, assuming (a + b) + c = a + (b + c)

       (a  +  b) + S(c)
     S((a +   b) +   c)  -- by (2)
      S(a +  (b  +   c)) -- by the induction hypothesis
        a + S(b  +   c)  -- by (2)
        a +  (b  + S(c)) -- by (2)
-}
plusAssoc a b (SS c) =
  let
    step1 :: SNat x -> SNat y -> SNat ('S z) -> ((x + y) + 'S z) :~: 'S ((x + y) + z)
    step1 x y (SS z) = gcastWith (given2 (x !+ y) (SS z)) Refl -- (2)

    step2 :: SNat x -> SNat y -> SNat z -> 'S ((x + y) + z) :~: 'S (x + (y + z))
    step2 x y     z  = gcastWith (plusAssoc x y z)        Refl -- induction

    step3 :: SNat x -> SNat y -> SNat z -> 'S (x + (y + z)) :~: (x + 'S (y + z))
    step3 x y     z  = gcastWith (given2 x (y !+ z))      Refl -- (2)

    step4 :: SNat x -> SNat y -> SNat z -> (x + 'S (y + z)) :~: (x + (y + 'S z))
    step4 _ y     z  = gcastWith (given2 y z)             Refl -- (2)
   in step1 a b (SS c) ==> step2 a b c ==> step3 a b c ==> step4 a b c

------------------------------------------------------------------------------

-- since all of above except are inferred automatically by the + type family
-- only the induction step is needed to help the second step type-check

plusAssocX
  :: SNat a
  -> SNat b
  -> SNat c
  -> ((a + b) + c) :~: (a + (b + c))
plusAssocX _ _  SZ    = Refl
plusAssocX a b (SS c) = gcastWith (plusAssoc a b c) Refl

{-
------------------------------------------------------------------------------
using ScopedTypeVariables
- use a forall in the let to bring fresh variables in scope
- then define the steps inside it
- using same names in the types so that we don’t have to pass variables to each step:
-}

plusAssocY
  :: SNat a
  -> SNat b
  -> SNat c
  -> ((a + b) + c) :~: (a + (b + c))
plusAssocY a b SZ =
  let proof :: forall x y
             . SNat x -> SNat y
            -> ((x +  y) + 'Z)
           :~:  (x + (y  + 'Z))
      proof x y = step1 ==> step2
        where
          step1 :: ((x + y) + 'Z) :~: (x + y)
          step1  = gcastWith (given1 (x !+ y)) Refl

          step2 :: (x + y) :~: (x + (y + 'Z))
          step2  = gcastWith (given1       y)  Refl
  in proof a b
plusAssocY a b (SS c) =
  let proof :: forall x y z
             . SNat x -> SNat y -> SNat z
            -> ((x +  y) + 'S z)
           :~:  (x + (y  + 'S z))
      proof x y z = step1 ==> step2 ==> step3 ==> step4
        where
          step1 :: ((x + y) + 'S z) :~: 'S ((x + y) + z)
          step1  = gcastWith (given2 (x !+ y) (SS z)) Refl

          step2 :: 'S ((x + y) + z) :~: 'S (x + (y + z))
          step2  = gcastWith (plusAssoc x y z)        Refl

          step3 :: 'S (x + (y + z)) :~: (x + 'S (y + z))
          step3  = gcastWith (given2 x (y !+ z))      Refl

          step4 :: (x + 'S (y + z)) :~: (x + (y + 'S z))
          step4  = gcastWith (given2 y z)             Refl
   in proof a b c

{-
------------------------------------------------------------------------------
Commutativity

Here’s the proof (in mathematical notation) for the property of commutativity of addition. What we want to prove is a+b=b+a:

The base case b=0 is the left identity property.

For the second base case b=1:

First we prove it for a=0, which gives 0+1=1+0, the right identity property.
Then we prove inductively, assuming a+1=1+a:
= = = = = = S(a)+1S(a)+S(0)S(S(a)+0)S((a+1)+0)S(a+1)S(1+a)1+S(a)by definition of natural numbersby (2)by definition of natural numbersas proved by the base case for b=0by the induction hypothesisby (2)

For the induction, assuming a+b=b+a:

= = = = = = = a+S(b)a+(b+1)(a+b)+1(b+a)+1b+(a+1)b+(1+a)(b+1)+aS(b)+aby definition of natural numbersby associativityby the induction hypothesisby associativityby the base case for b=1by associativityby definition of natural numbers

The proof is left as an exercise, but using the same pattern as in the associativity proof it should be pretty straightforward. The signature should be:

plusComm :: SNat a -> SNat b -> (a + b) :~: (b + a)
The proof (and even more proofs) can be found in the repo.

Using the proofs
It was a bit difficult to find a use case for the proofs on addition, but I quickly found an issue trying to append two length-indexed vectors. The vector definition and several operations are more or less the same as the ones in the Stitch paper:

data Vec :: Nat -> * -> * where
  V0   :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a

infixr 5 :>
I’m not going to go into detail here, but the most important part is that this vector type has its length in its type as extra information, and we need to reason about the length when manipulating it.

One common example is appending two vectors. It is apparent that if one has length n and the other has length m, then the resulting vector will have length n + m. A naive first attemt would be the following:

append :: Vec n a -> Vec m a -> Vec (n + m) a
append V0      ys = ys
append (x:>xs) ys = undefined -- let's wait for now
But even the base case fails to typecheck:

• Could not deduce: ('Z + m) ~ m
      from the context: n ~ 'Z
The error says that the only concrete information we have is n ~ Z, because the first vector is the empty vector. And because we return ys, which has length m, the resulting type Z + m should be the same as m, but the type checker is not convinced. Let’s notice that what we need is to use the left identity property, and convince the checker:

append :: Vec n a -> Vec m a -> Vec (n + m) a
append V0      ys = gcastWith (plusLeftId ?) ys
append (x:>xs) ys = undefined -- let's wait for now
There is one important part missing: the actual length. All our proofs used singletons to help drive the checking process, but here there is no mention of a singleton. So let’s add it for now, and later we’ll try to make this process implicit. We’ll explicitly pass the lengths of both vectors:

append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0      ys = gcastWith (plusLeftId m) ys
append n  m (x:>xs) ys = undefined -- let's wait for now
OK, this works. Let’s try to do the next case where the first vector is nonempty. We’ll make it fail just to see the error message:

append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0      ys = gcastWith (plusLeftId m) ys
append n  m (x:>xs) ys = x :> append ? m xs ys
Uh-oh. How do we get the length of the first vector after we remove the first element? Luckily, that’s just n - 1. Let’s create a helper function to get the predecessor number of a singleton of Nat:

spred :: SNat (S n) -> SNat n
spred (SS n) = n
This function is total, because there is no need for the case spred SZ. As a matter of fact, it wouldn’t type check because it would mean trying to do SZ :: SNat (S n) which doesn’t make sense. And because the first vector in the second case of append is not empty, we know that n is not SZ but SS ....

append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0      ys = gcastWith (plusLeftId m) ys
append n  m (x:>xs) ys = x :> append (spred n) m xs ys
This time we get this error:

• Could not deduce: ('S n1 + m) ~ 'S (n1 + m)
  from the context: n ~ 'S n1
Where n1 is the type of the result of spred n. Let’s construct a proof for that:

We need to prove S(a)+b=S(a+b). We can do an inductive proof which will be quicker (in the repo I use an inductive proof), but let’s try to do it in one pass:

= = = S(a)+bb+S(a)S(b+a)S(a+b)by commutativityby (2)by commutativity

And in Haskell:

append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0 ys
  = gcastWith (plusIdenL m) ys
append n m (x:>xs) ys
  = gcastWith (proof pn m) $ x :> app (spred n) m xs ys
    where
      pn = spred n
      proof :: forall x y. SNat x -> SNat y -> (S x + y) :~: S (x + y)
      proof x y = step1 ==> step2 ==> step3
        where
          step1 :: (S x + y) :~: (y + S x)
          step1 = gcastWith (plusComm (SS x) y) Refl

          step2 :: (y + S x) :~: S (y + x)
          step2 = gcastWith (given2 y (SS x)) Refl

          step3 :: S (y + x) :~: S (x + y)
          step3 = gcastWith (plusComm y x) Refl
And that’s the proof. Now appending two vectors should work. First, here’s an instance of Show for Vec:

instance (Show a) => Show (Vec n a) where
  show v = "[" ++ go v
    where
      go :: (Show a') => Vec n' a' -> String
      go v = case v of
        V0 -> "]"
        (x :> xs) -> show x ++ sep ++ go xs
          where
            sep = case xs of
              V0 -> ""
              _  -> ", "
And two example vectors:

x = 1 :> 2 :> 3 :> 4 :> V0
lengthX = SS (SS (SS (SS SZ)))

y = 5 :> 6 :> 7 :> 8 :> 9 :> V0
lengthY = SS (SS (SS (SS (SS SZ))))
> append lengthX lengthY x y
[1, 2, 3, 4, 5, 6, 7, 8, 9]
> :t append lengthX lengthY x y
append lengthX lengthY x y
  :: Vec ('S ('S ('S ('S ('S ('S ('S ('S ('S 'Z))))))))) Integer
We can also remove the need to have the length in a variable beforehand. To do that, we have to resort to type classes (again, this is described in the Stitch paper). We will construct a type class with a single method that can magically give an SNat depending on the instance we are using:

class IsNat (n :: Nat) where nat :: SNat n

instance IsNat Z where
  nat = SZ

instance IsNat n => IsNat (S n) where
  nat = SS nat
Then we can just use nat to get the length. The instance of IsNat to use is resolved thanks to the fact that the n in the constraint IsNat is the same n that is the vector length:

vlength :: IsNat n => Vec n a -> SNat n
vlength _ = nat
> append (vlength x) (vlength y) x y
[1, 2, 3, 4, 5, 6, 7, 8, 9]
But I promised that we can have the length passed implicitly. Again, we’ll use the typeclass with some help from TypeApplications and ScopedTypeVariables:

(+++) :: forall n m a. (IsNat n, IsNat m)
      => Vec n a
      -> Vec m a
      -> Vec (n + m) a
(+++) = append (nat @n) (nat @m)
> x +++ y
[1, 2, 3, 4, 5, 6, 7, 8, 9]
-}
