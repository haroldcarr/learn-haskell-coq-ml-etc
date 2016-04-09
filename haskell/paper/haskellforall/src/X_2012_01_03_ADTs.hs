{-
Created       : 2014 Oct 19 (Sun) 21:07:15 by Harold Carr.
Last Modified : 2014 Oct 21 (Tue) 14:50:23 by Harold Carr.

http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers.html
-}

{-# LANGUAGE TupleSections #-}

module X_2012_01_03_ADTs where

{-
ADTs

Why are they called "algebraic"?

can be modeled via addition and multiplication

SUM TYPES

a + b = Either a b

Addition is associative
Eithers are associative

Either (Either a b) c ~ Either a (Either b c)

(a + b) + c = a + (b + c)

~ denotes "essentially the same"

Show equivalent:
-}
forward1 :: Either (Either a b) c -> Either a (Either b c)
forward1  x = case x of
    Left  e -> case e of
        Left  a -> Left a
        Right b -> Right (Left b)
    Right c -> Right (Right c)

reverse1 :: Either a (Either b c) -> Either (Either a b) c
reverse1 x = case x of
    Left  a -> Left (Left a)
    Right e -> case e of
        Left  b -> Left (Right b)
        Right c -> Right c

-- or more tersely:
forward2 :: Either (Either a b) c -> Either a (Either b c)
forward2 = either (either Left (Right . Left)) (Right . Right)

reverse2 :: Either a (Either b c) -> Either (Either a b) c
reverse2 = either (Left . Left) (either (Left . Right) Right)

{-

PRODUCT TYPES

a * b = (a, b)

associative:

((a, b) , c) ~ (a, (b , c))

(a * b) * c  = a * (b * c)
-}

forward3 :: ((a, b), c) -> (a, (b, c))
forward3    ((a, b), c)  = (a, (b, c))

reverse3 :: (a, (b, c)) -> ((a, b), c)
reverse3    (a, (b, c))  = ((a, b), c)

{-
as in algebra, multiplication distributes over addition:

(a , Either b   c) ~ Either (a , b)  (a , c)
 a *       (b + c) =         a * b  + a * c
-}

-- TODO: make type parameters match argument names
forward4 :: (t, Either a b) -> Either (t, a) (t, b)
forward4 (a, e) = either (Left . (a ,)) (Right . (a ,)) e

reverse4 :: (Either (c, b) (c, b1) -> c, Either (a, b2) (a1, b3) -> Either b2 b3)
reverse4 = (either fst fst, either (Left . snd) (Right . snd))

-- analog of 0
data Zero -- data type with no constructor

{-
Either Zero a ~ a
       0  + a = a
-}

forward5 :: Either t t1 -> t1
forward5 (Right a) = a

reverse5 :: b -> Either a b
reverse5 a = Right a -- or reverse = Right

-- forward5 does not need to check alternative case for Left constructor because Zero cannot be created

-- analog of 1 : any type with single empty constructor:

-- type One = ()

{-
(One, a) ~ a
  1 * a  = a

forward6 (One, a) = a
reverse6 a = (One, a)
-}

forward6 :: ((), t) -> t
forward6 ((), a) = a

reverse6 :: t -> ((), t)
reverse6 a = ((), a)

{-
Multiplying by Zero has the expected effect: Neither type can be built:

(Zero, a) ~ Zero
   0 * a  = 0


Can create any Haskell data type using only
- Either
- (,)
- ()
- Zero

data Bool = True | False
     Bool ~    1 + 1                -- i.e. Either () ()
          =    2

data Maybe   = Just a | Nothing
     Maybe a ~      a + 1           -- i.e. Either a ()

data Trool = True_ | False_ | Empty -- "troolean"
     Trool ~     1 +      1 + 1     -- i.e. Either (Either () ()) ()
           = 3

-- or

type Trool = Maybe Bool
     Trool ~ Bool + 1               -- i.e. Either (Either () ()) ()
           ~ 2 + 1
           = 3

data TwoBools = TwoBools Bool Bool
     TwoBools ~ Bool * Bool         -- i.e. (Bool, Bool)
              ~ 2 * 2
              = 4

Each type corresponds to an algebraic number which tells us how many states that type can represent.

algebraic equivalence : if two  types  represent  the  same  number of  states,
can reversibly convert them back and forth.

Multiply two types means multiplying the number  of states they represent.

Add two types means adding the number of states they represent.

rules to translate Haskell data types into algebraic expressions:
- Replace | with +
- Replace a constructor (or tuple) that holds multiple values with a product
- Replace an empty constructor with 1.
- Replace a type with no constructor with 0

e.g.,

data Sum a b c = S1 a | S2 b | S3 c
     Sum a b c ~    a +    b +    c

type Sum2 a b c = Either a (Either b (Either c   Zero)))
     Sum2 a b c =        a +      (b +      (c + 0))
                =        a +       b +       c

data Product a b c d e = P a   b   c   d   e
     Product a b c d e ~   a * b * c * d * e

type Product2 a b c d e = (a , b , c , d , e)
     Product2           ~  a * b * c * d * e

type Product3 a b c d e = (a , (b , (c , (d , (e , ())))))
     Product3           ~  a * (b * (c * (d * (e * 1))))
          = a * b * c * d * e

data Term a b c = T1 a   b | T2 c
     Term a b c ~    a * b +    c

type Term2 a b c = Either (a , b)  c
     Term2 a b c ~         a * b + c

above, type constructors like algebraic functions.
- Sum computes the sum of its three arguments
- Product computes the product of its five arguments

-- RECURSIVE TYPES

data List a = Nil | Cons a  (List a)

     List a ~   1  +      a * List a
            ~   1  +      a *     (1 + a * List a)
            ~   1  +      a *     (1 + a *     (1 + a * List a))
            ~ ...
            ~  (1) +     (a) + (a * a) + (a * a * a) + ...

Says a List is either
- empty (1), or
- (+) has 1 element (a), or
- (+) has 2 elements (a * a), or
- (+) has 3 elements (a * a * a), ...

above: algebraic equivalents to data types

-- FUNCTIONS AS ALGEBRAIC EXPRESSIONS

a -> b = b ^ a

suppose function has a finite domain (i.e. a has n finite states)

encode function by encoding n-tuple of input/output for all possible inputs

e.g., f :: Bool -> Trool
- represented as data type of type (Trool, Trool) : one Trool for each state of Bool
- only 9 unique functions with that type
- algebraic translation agrees

Bool -> Trool ~ Trool^Bool
              ~     3^2
              = 9

data Trool = True_ | False_ | Empty

f :: Bool -> Trool
f x = case x of
    True -> Empty
    False -> False_

f' :: (Trool, Trool)
f'  = (Empty, False_)

f' ~ f

'->' exponentiates types, thus

a -> b -> c ~ (a, b) -> c
    (c^b)^a = c^(a * b)

are equal

also:

forward = uncurry
reverse = curry


-- TYPES WITH INFINITE STATES (e.g., Integer, List a)
- algebra of adding / multiplying infinities

construct any type just from just two base types and three orthogonal functions on types:

-- Base types
data Zero
type One = ()

-- Type Functions
a + b = Either a b
a * b = (a, b)
a -> b = b^a

But the above does not type-check, but does give intuition/motivation of ADTs


example translation

data For = A a | B b | C c

-- gets translated into

newtype For = For { unFor :: Either a (Either b (Either c Zero))) }

constructors that contain several values get translated into a product fold:

data All = A a b c d e

-- gets translated into

newtype All = All { unAll :: (a, (b, (c, (d, (e, ()))))) }

Would be nice if the newtypes could automatically derive
class instances from their components types based on the algebra
(i.e. a more powerful version of GeneralizedNewtypeDeriving).

e.g., type H that was an instance of some interface:

class HasHeight t where getHeight :: t -> Double

instance HasHeight H where getHeight = ...

Then define some other type based off of H:

data Object x y = Person H x | Building H y | Chair H
    deriving (HasHeight)

Then class system does algebra to confirm that it can treat Object as a super-set of H:

Object x y = H *  x + H * y + H
           = H * (x +     y + 1) -- confirmed!

Then automatically derive instance:

instance HasHeight Object where
    getHeight o = case o of
        Good   h _ -> getHeight h
        Better h _ -> getHeight h
        Best   h   -> getHeight h

Unfortunately, Haskell's class system isn't strong enough to express
this kind of behavior, so we are stuck bickering over various record
implementations instead of enjoying a truly algebraic type system.

-}
-- End of file.
