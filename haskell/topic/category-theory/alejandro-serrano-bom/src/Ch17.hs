{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Ch17 where

import           Test.HUnit      (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util as U (t,tt)
------------------------------------------------------------------------------
import           Control.Monad
import           Data.Kind

-- ============================================================================
{-
category theorists define monads as a particular case of of generalized monoids
- Monoid mempty/mappend correspond to Monad return/join
-}

class Monoid1 m where
  mempty1  :: m            -- constant/identity element
  mappend1 :: m -> m -> m  -- combining elements

-- alternative formulation : any value 'a' equivalent to fun :: () -> a

{-# ANN toUnit "HLint: ignore Redundant lambda" #-}
toUnit   :: a -> (() -> a)
toUnit      a =  \() -> a

fromUnit :: (() -> a) -> a
fromUnit    f         =  f ()

v1 :: Int
v1 = 1 :: Int
f1 :: () -> Int
f1 = toUnit v1
tToFromUnit1 :: [Test]
tToFromUnit1 = U.tt "tToFromUnit1"
  [ fromUnit (toUnit (fromUnit f1))
  , fromUnit (toUnit (f1 ()))
  , fromUnit (\() -> f1 ())
  , fromUnit f1
  ]
  v1

tToFromUnit2 :: [Test]
tToFromUnit2 = U.tt "tToFromUnit2"
  [ fromUnit (toUnit v1)
  , fromUnit (\() -> v1)
  , (\() -> v1) ()
  ]
  v1

-- using alternate formulation:

class Monoid2 m where
  mempty2  :: ()     -> m -- from above
  mappend2 :: (m, m) -> m -- tupled args (instead of curried)

{-
monads live in higher world
- some constructions generalized to work over functors
  - i.e., type constructors --- instead of ground types
-}

------------------------------------------------------------------------------
{-
FIRST CONSTRUCTION : FUNCTION ARROW replaced by NATURAL TRANSFORMATIONS (NT)
- NT: funs between functors that work parametrically over any element type:
-}
type f :=>: g = forall a. f a -> g a

-- e.g.,:

safeHead :: [] :=>: Maybe -- expanded: forall a. [a] -> Maybe a
safeHead    [] = Nothing
safeHead (x:_) = Just x

-- length is natural transformation from [a] to "Int" (but Int needs to be wrapped)
newtype Const c a = Const { unConst :: c }
lengthNT :: [] :=>: Const Int
lengthNT    []  = Const 0
lengthNT (_:xs) = Const (1 + unConst (lengthNT xs))

------------------------------------------------------------------------------
{-
SECOND CONSTRUCTION : TUPLING
- in world of ground types, (,) is a type constructor that turns two ground types into another one
- () is a ground type
- (a,()), ((), a) are equivalent : can move from one to other without losing info

need similar construction over functors : functor composition and identity functor
-}

-- | combine two functors into a single functor
newtype (f :.: g) a = Compose (f (g a)) deriving (Eq, Functor, Show)

-- | type constructor that works in same way that () does for tuples
newtype Identity  a = I a               deriving (Eq, Functor, Show)

composeWithIdentity       :: Functor f =>        f :=>: (f :.: Identity)
composeWithIdentity     x  = Compose (fmap I x)
removeIdentity            :: Functor f =>               (f :.: Identity) :=>: f
removeIdentity (Compose x) = fmap (\(I y) -> y) x


tfidcompose1 :: [Test]
tfidcompose1 = U.t "tfidcompose1"
    (composeWithIdentity (Just 'a'))
    (Compose (Just (I 'a')))

-- composition of removeIdentity/composeWithIdentity is equivalent
-- to identity natural transformation regardless of order of composition

idToId :: Identity :=>: Identity
idToId = Prelude.id

tfidcompose2 :: [Test]
tfidcompose2 = U.tt "tfidcompose2"
    [ (removeIdentity      Prelude.. composeWithIdentity)       (I (Just    'a'))
    , idToId                                                    (I (Just    'a'))
    ]
    (I (Just 'a'))

tfidcompose3 :: [Test]
tfidcompose3 = U.tt "tfidcompose2"
    [ (composeWithIdentity Prelude.. removeIdentity)      (Compose (Just (I 'a')))
    ]
    (Compose (Just (I 'a')))

{-
replace Monoid constructions with above
- replacement of function arrows by natural transformations
- tupling by functor composition
-}

mmempty1  :: Monad m => Identity  :=>: m
mmempty1  (I         a) = return a
mmappend1 :: Monad m => (m :.: m) :=>: m
mmappend1 (Compose mma) = join mma

-- Natural transformations are functions parametric on element type
-- Identity  a  : equivalent to :      a
-- (m :.: m) a  : equivalent to : m (m a)

-- | Monad 'return'
mmempty2  :: Monad m => Identity  a -> m a -- i.e. a -> m a
mmempty2  (I       a) = return a
-- | Monad 'join' from Monad
mmappend2 :: Monad m => (m :.: m) a -> m a -- i.e. m (m a) -> m a
mmappend2 (Compose a) = join a

{-
CONCLUSION: Monads are monoids that work on functors instead of ground types

in other words:

  "A monad is just a monoid in the category of endofunctors, what’s the problem?"
-}

-- ============================================================================
-- Categories, Functors, Natural Transformations

{-
category theorists use above in more general sense (e.g., Functor in FP is an endofunctor)


3 fundamental concepts
- CATEGORIES
- FUNCTORS : transformations between categories
- NATURAL TRANSFORMATIONS : relate two different functors

definitions below have two parts
- data
- laws the data must satisfy

category C given by four pieces of data:
1. set of objects O
2. set of morphisms (i.e., arrows) M
   - each arrow has two associated objects from O: domain and codomain : A -f-> B
3. for all object A in the category
   - identity arrow A -> A
     - represented by id_A or 1_A (drop subscript when context clear)
4. given 2 arrows A --f-> B and B --g-> C
   - composition A --g . f-> C

laws
1. Associativity
   - given A --f-> B
           B --g-> C
           C --h-> D
   - h . (g . f) == (h . g) . f
2. Identity
   - given A --f-> B
   - id_B . f == f . id_A

--------------------------------------------------
-- CATEGORIES

Haskell representation of category
- can only give data part of definition
- laws cannot be checked compiler (i.e., need dependent types)
-}

-- type for objects
--             |  type for morphisms
--             v  v
class Category o (m :: o -> o -> *) where
  id  :: m a a
  (.) :: m b c -> m a b -> m a c

{-
example category in which
- objects are Haskell ground types (types with kind *)
- morphisms are functions between them
-}

instance Category * (->) where
  id      x =      x
  (g . f) x = g (f x)

{-
category laws similar to monoid laws, shows:
- category is a generalized monoid
- not all elements may be combined with each other
- or, vice versa
  - monoid can be seen as category 'Single' in which there is only one object, represented as 'One'
-}

-- data Single = One -- redefined below

{-
implies : every arrow is : One -> One ; so all may be freely combined (since (co)domains match)

need one arrow per element in the monoid
domain/codomain must make MonoidArrow the shape required by Category
- only possible instantiation is One
-}

newtype MonoidArrow m (a :: Single) (b :: Single) = MonoidArrow m

{-
must lift monoid ops into the category
- identity arrow corresponds to identity element of monoid
- composition of two arrows is combination of two monoid elements
-}

instance Monoid m => Category Single (MonoidArrow m) where
  id                                = MonoidArrow     mempty
  (MonoidArrow x) . (MonoidArrow y) = MonoidArrow (x `mappend` y)

-- TODO: Exercise 16.3 Prove that the category laws hold whenever 'm' satisfies monoid laws

------------------------------------------------------------------------------
-- p 246 FUNCTORS : next basic concept in category theory

{-
mapping between two categories that respects the structure embodied in them.

functor F from category C to category D given by two pieces of DATA:
- mapping F_O from C objects to D objects
- mapping F_M from C arrows  to D arrows
  - given an arrow A --f-> B in C, domain/codamin must be mapped as
    - F_O(A) --F_M(f)-> F_O(B)
short notation: F A --F f-> F B

LAWS
- respect Identities
  - given object A that is mapped to F A
  - its identity arrow id_A must be mapped to id_F A
- respect Composition
  - mapped arrow F (g . f) == F g . F f
-}

class (Category o1 m1, Category o2 m2)
    => FunctorC o1  (m1 :: o1 -> o1 -> *)
                o2  (m2 :: o2 -> o2 -> *)
                (objMap :: o1 -> o2)       where
  arrowMap :: m1 a b -> m2 (objMap a) (objMap b)

{-
example functor : two monoids defined as categories
- Boolean  monoid with conjunction
- Nat nums monoid with product

Haskell limitations force objMap to be a data type.
- cannot reuse Identity because its kind is * -> *
so:
-}

data Single = One | Other Single

-- type of MonoidArrow args in result do influence the type of elements inside it

instance FunctorC Single (MonoidArrow All)
         Single          (MonoidArrow (Product Integer))
         'Other where
  -- arrowMap :: MonoidArrow All a b
  --          -> MonoidArrow (Product Integer) (Other a) (Other b)
  -- map BOOL to NAT
  arrowMap (MonoidArrow (All True )) = MonoidArrow (Product 1)
  arrowMap (MonoidArrow (All False)) = MonoidArrow (Product 0)

newtype Product a = Product { getProduct :: a }
        deriving (Eq, Ord, Read, Show, Num)
instance Num a => Semigroup (Product a) where
  Product l <> Product r = Product (l * r)
instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (<>)
newtype All = All { getAll :: Bool }
        deriving (Eq, Ord, Read, Show)
instance Semigroup All where
  All True <> All True = All True
  _        <> _        = All False
instance Monoid All where
  mempty  = All True
  mappend = (<>)

{-
need to map all arrows from Boolean monoid to natural numbers monoid
- one arrow corresponds to one element of the monoid
  - so need to map True and False to natural numbers : see "map BOOL to NAT" above

Boolean identity (i.e., True) : corresponds to Product identity (i.e., 1)
-}

{-
subclass of functors needed to define a monad : ENDOFUNCTORS
- functor from category C to category C
-}

type Endofunctor (o :: *) (m :: o -> o -> *) (objMap :: o -> o) = FunctorC o m o m objMap

{-
Functor class in Haskell does not correspond to categorical functor
Haskell Functor instances are endofunctors for the category of types and arrows:
-}

type HaskellFunctor objMap = Endofunctor (*) (->) objMap

{-
example : list constructor
- object mapping : application of list type constructor : A -> [A]

  arrowMap :: (a -> b) -> ([a] -> [b])

exactly type of fmap for lists
-}
------------------------------------------------------------------------------
-- p 248 NATURAL TRANSFORMATIONS : map functors to functors

{-
for functors F and G to take part in a natural transformation: η
- each must transform same underlying categories
  - F and G must take same C category into same D category D

To define a natural transformation, data required is a family of arrows
- one per object X in C, F X -> G X
  -  how to take objects transformed by F to objects transformed by G
- each arrow called a component of η at X, written ηX

family of arrows must satisfy NATURALITY CONDITION
- for every arrow X --f-> Y from C : ηY . F f = G f . ηX

states : does not matter whether we transform objects from F to G
         before or after transforming an arrow

Haskell [] and Maybe functors : endofunctors in category of Haskell types

safeHead natural transformation from [] to Maybe
- describe how to map every [a] to Maybe a
- the components of safeHead at each type a:
safeHead      :: [a] -> Maybe a
safeHead    [] = Nothing
safeHead (x:_) = Just x

need to ensure that safeHead describes a natural transformation

must be the case that for every function X --f->Y

   safeHead_Y . fmap_[] f == fmap_Maybe . safeHead_X

arrow part of functor described by 'fmap'

-- Case of empty list []
-}

f' :: Int -> Int
f' = (+1)
tntEmpty :: [Test]
tntEmpty = U.tt "tntEmpty"
  [ safeHead (fmap f' [])
  , safeHead []
    --
  , fmap f' (safeHead [])
  , fmap f' Nothing
  ]
  Nothing
-- Case of non-empty list (x:xs)
tntNonEmpty :: [Test]
tntNonEmpty = U.tt "tntNonEmpty"
  [ safeHead (fmap f' [1,2])
  , safeHead (f' 1 : fmap f' [2])
    --
  , fmap f' (safeHead [1,2])
  , fmap f' (Just 1)
  ]
  (Just (f' 1))

-- p 249

{-
when restricted to Haskell Functors (endofunctors for category of Haskell types and functions)
- natural transformation always has shape : F a -> G a (for functors F and G)
- this only defines the components of the natural transformation

when implementing those components
- Haskell semantics prevent having different impls depending on 'a'
  - e.g., could not have different code case of [Int] in safeHead
- functions of form F a -> G a are PARAMETRIC over type 'a'

parametricity is a stronger property than naturality
- every function with shape of components of natural transformation defines a natural transformation
— i.e., a free theorem
  - property we know about a function by looking only at its type sig

leads to parametric mappings between functors as Haskell definition of natural transformations:

    type f :=>: g = forall a. f a -> g a (defined above)

like the Functor type class, (:=>:) refers to a specific sort of natural transformation
- e.g., natural transformations between endofunctors for the category of Haskell types and funs

this is enough to understand the construction of monads as monoids.
-}

------------------------------------------------------------------------------
-- p 250 16.3 Monoids in Monoidal Categories
{-
CATEGORIES, FUNCTORS, NATURAL TRANSFORMATIONS : basic parts of category theory

now going to look at specific things needed to describe a monad as a monoid

1st question: what does MONOID mean in context of a category?

Haskell def:

class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m

not amenable for generalizing from category of Haskell types to other categories
- because mempty takes no argument; mappend takes two argument
  - arrows in a category always take exactly one argument

FIX:

categorical point of view for monoid: is thus:

class Monoid m where
  mempty  :: ()     -> m
  mappend :: (m, m) -> m

solved problem of the shape of arrows
- solution assumes category has notion of tuple and notion of unit.
- not all categories have those
- categories that do are called MONOIDAL CATEGORIES

GOAL OF THIS SECTION : describe MONOIDAL CATEGORIES
- then can see a monoid as a generalization of the Monoid type class
  in which tuple and unit  are instantiated to those notions in the corresponding monoidal category

monoidal category C is described by two pieces of DATA
(in addition to elements that make C a regular category)
1. bifunctor biX from CxC  to C
   - functors that take two categories as arguments (instead of single)
     - for Haskell types, bifunctor is tuple constructor (,)
2. An object 1 from C
   - in this section, this object is the type ()

LAWS
1. 1 is identity for the bifunctor b
   - 1 biX X == X biX 1 == X : X is an arbitrary object of C
2. bifunctor must be associative
   Given any three objects, A, B, and C from C
   A biX (B biX C) == (A biX B) biX C
   where (==) means : natural transformation with inverse should exist between all constructions

-- p 251

category may have more than one way to be monoidal
e.g., in category of types, another structure in addition to tuples
- bifunctor (Either) and object (Void) from category of types
-}

data Void

{-
has no constructor
- cannot create a value of type Void
- cannot pattern match on it

to check that Either and Void  fit together
- check Either Void a, Either a Void, and 'a' are isomorphic
-}

removeVoidL :: Either Void a -> a
removeVoidL (Right x) = x
removeVoidL (Left _) = error "impossible"
composeWithVoidL :: a -> Either Void a
composeWithVoidL = Right

{-
TODO: Exercise 16.5 check that Either gives rise to a monoidal category, Void being the identity:
1. For the identity laws, by writing functions from Either a Void to a, and vice versa.
2. For associativity, by writing functions from and to di erent nesting structures

    Either a (Either b c)   and   Either (Either a b) c
-}

-- p 242

{-
summary

notion of monoid only makes sense inside a monoidal category

given the monoidal structure of the bifunctor b with the object 1
a monoid is defined by one object M and two arrows:

    1 --η-> M    and  M biX M --μ-> M

η : unit/identity of the monoid
μ : multiplication of the monoid

can now provide precise definition of Haskell’s Monoid type class in categorical terms
- it is formed by monoids in the monoidal category of Haskell types with respect to tuples.
-}

------------------------------------------------------------------------------
-- p 252 16.4 The Category of Endofunctors

{-
closer to goal of defining monads as monoids
- need to choose the right (monoidal) category to make definition work
  - category of endofunctors : End(C) from any category C

1. objects of category are endofunctors of C (functors that take C to C)
   - type constructors that are instances of Functor type class (e.g., [], Maybe)
   - where C is Haskell types

2. arrows of End(C) are natural transformations between functors
   - functions of form F a -> G a for endofunctors F and G

IDENTITY ARROW for each object
- given each endofunctor F, need natural transformation from F to F
- identity natural transformation : sends each object F X to F X itself

above components satisfy NATURALITY PROPERTY

COMPOSITION of two natural transformations
- components at each object X are composition of components of each transformation

NATURALITY CONDITION is satisfied
-}

{-
TODO
Exercise 16.6 Write out this construction for the case of Haskell types and Functor:
1. The identity natural transformation is a function:

       ident :: Functor f => f :=>: f

2. The composition of two natural transformations is a function:

       compos :: (Functor f, Functor g, Functor h)
              => (g :=>: h) -> (f :=>: g) -> (f :=>: h)
-}

-- p 253

{-
-- https://github.com/lambdaconf/book-of-monads/issues/155 -- former/latter backwards references

next step i: make End(C) into a monoidal category by providing
- bifunctor : way to combine two endofunctors into a new one

    data (f :.: g) a = Compose (f (g a)) deriving Functor

- object  : identity for this composition
  - must be an endofunctor
  — must be one does not add info to the composition

    data Identity a = I a deriving Functor


monad is a monoid in End(C)

to show it coincides with the definition from Chapter 1:

UNIT : 1 --η-> M
- 1 is identity functor; arrows are natural transformations
  - so type of
                 η is Identity :=>: M
  since natural transformation in Haskell represented by a polymorphic function,
  this type is equivalent to:
                 η :: Identity a -> M a
-- https://github.com/lambdaconf/book-of-monads/pull/156 - remove pronoun
- Identity a is simply a fancy wrapper around 'a'

UNIT SUMMARY
- unit of a monoid in the category EndpCq is function
            a -> M a
  for endofunctor M

the return operation of monad M

JOIN
MULTIPLICATION : (M biX M) --μ-> M
- In End(C), bifunctor biX is functor composition where the arrows are natural transformations

                 (M :.: M) :=>: M

unpack (:=>:) to get

           μ :: (M :.: M) a -> M a

(M :.: M) a == M (M a) (like  (f . g) x == f (g x))

so type is

           μ :: M (M a) -> M a

the type of JOIN
-}

------------------------------------------------------------------------------
-- p 254 16.4.1 Functors in End(C)

{-
'hoist' (Ch12 mmorph) : apply a conversion between monadic computations within a transformer:

     hoist0 :: Monad m => (forall a. m a -> n a) -> t m b -> t n b

hoist is member of MFunctor class called MFunctor
- hoist is the arrow part of a functor but in the category End(C)

every polymorphic function between functors is a natural transformation:

    hoist :: Monad m => (m :=>: n) -> (t m :=>: t n)

Compare with

     fmap ::            (a  ->  b) -> (f a ->   f b)

only difference : kind of arrows
- fmap  : normal arrows for fmap
- hoist : natural transformations

SAYS: monad transformer is a variation of the concept of a type constructor,
      where the types are always replaced by monads

notion of a functor in End(C) makes sense : is concept of a monad in End(x) OK?

defined by two operations: η and μ

need identity and composition that work at level of monad transformers:
-}

-- identity and composition work on monad transformers (not work on monads)
-- | given a monad; return same one
newtype IdT          m a = IdT (m a)
-- | given two monad transformers; produce a new one
newtype ComposeT f g m a = ComposeT (f (g m) a)

{-
-- https://github.com/lambdaconf/book-of-monads/pull/160 : fix grammar

monads : arrows are natural transformers : funs polymorphic over type contained in monad

Transformer Morphisms: need similar notioln, but where polymorphism at level of monad being wrapped

-- https://github.com/lambdaconf/book-of-monads/issues/157 - missing 'T' and bad sigs
-- FIXED:
-}

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) } deriving Functor
instance (Functor a, Monad a) => Applicative (MaybeT a) where
  pure                    = MaybeT Prelude.. pure Prelude.. Just
  MaybeT mf <*> MaybeT mx = MaybeT $ do
    mb_f <- mf
    case mb_f of
      Nothing -> return Nothing
      Just f  -> do
        mb_x <- mx
        case mb_x of
          Nothing -> return Nothing
          Just x  -> return (Just (f x))
instance (Applicative m, Monad m) => Monad (MaybeT m) where
  return         = pure
  MaybeT m >>= f = MaybeT $ do
    y <- m
    case y of
      Nothing -> return Nothing
      Just a  -> runMaybeT (f a)

type t :==>: s = forall m a. t m a -> s m a

η :: IdT                    :==>: MaybeT
μ :: ComposeT MaybeT MaybeT :==>: MaybeT
η  = undefined
μ  = undefined

-- replacing definitions and unwrapping gives

-- | lift
η' :: m a -> MaybeT m a
η' = undefined
-- | squash (in mmorph) : flatten two consecutive layers
μ' :: MaybeT (MaybeT m) a -> MaybeT m a
μ' = undefined

-- TODO: Exercise 16.7 Relate the operations described in Chapter 12 to η and μ.

{-
SUMMARY

- basic notions of category theory
- categorical generalization of a monoid : to see that a monad is a monoid
-- https://github.com/lambdaconf/book-of-monads/issues/158 -- functions as monads
- that view shows mmorph functions : monads in another category
-}

------------------------------------------------------------------------------
t17 :: IO Counts
t17  =
  runTestTT $ TestList $
  tToFromUnit1 ++ tToFromUnit2 ++
  tfidcompose1 ++ tfidcompose2 ++ tfidcompose3 ++
  tntEmpty ++ tntNonEmpty
