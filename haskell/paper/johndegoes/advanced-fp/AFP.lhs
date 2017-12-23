> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE KindSignatures            #-}

https://gist.github.com/jdegoes/97459c0045f373f4eaf126998d8f65dc

> module AFP where
>
> import           Control.Arrow                 ((&&&))
> import           Prelude                       hiding (filter, id)
> import           Test.HUnit                    (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util               as U (t, tt)

------------------------------------------------------------------------------
1. Mastering Functions

Function : mapping from one set, called a domain, to another set, called the codomain

both domain and codomain are types

> square :: Int -> Int
> square x = x * x
>
> e001 = U.t "e001" (square 2) 4

------------------------------------------------------------------------------
Higher-Order Functions : functions that accept or return a function

> filter :: (a -> Bool) -> [a] -> [a]
> filter f    []  = []
> filter f (x:xs) | f x       = x : filter f xs
>                 | otherwise =     filter f xs
>
> e002 = U.t "e002" (filter even [1, 2, 3]) [2]

------------------------------------------------------------------------------
Function Combinators: higher-order functions that accept and return functions

> -- | shorthand for function signature
> type To a = Int -> a
>
> -- | combinator that takes two functions and returns a function
> both, both', both'' :: To a -> To b -> To (a, b)
> both   left right = \c -> (left c, right c)
> both'  left right    c =  (left c, right c)
> both'' left right = left Control.Arrow.&&& right
>
> e003 = U.t "e003" (both show (+1) 2) ("2", 3)

------------------------------------------------------------------------------
Polymorphic Functions: universally quantified over one or more type parameters

> id :: a -> a -- implicit forall a.
> id x = x
>
> e004 = U.t "e004" (id  3)   3
> e005 = U.t "e005" (id "3") "3"

------------------------------------------------------------------------------
2. Mastering Types

type : compile-time description of a set of values

values have types : they are a member of the set of values they represent

:t 2
2 :: Num t => t

------------------------------------------------------------------------------
Algebraic Data Types : type formed by composing product and sum types

Product Type : Cartesian cross product on 2 or more types

> type Point2D  = (Int, Int)
> data Point2D' = Point2D' Int Int

Sum types : disjoint union on 2 or more types.

> data AddressType = Home | Business -- enumeration
>
> type Error = String
> type HttpResponse = String
> type RequestResult = Either Error HttpResponse -- one or the other type

------------------------------------------------------------------------------
Universally quantified type : defines category (aka kind) of types that are all parameterized by some arbitrary type

Type Constructors : universally quantified type, used to construct types

> -- | universally quantified over type variable a
> data List a = Nil
>             | Cons a (List a)
>      deriving (Eq, Show)

------------------------------------------------------------------------------
Higher-Kinded Types

Type-Level Functions, e.g.: type constructors viewe as type-level functions: accept types and return types

Kinds : the type of types

kind signature represent "arity"

- *           : kind of types
- * -> *      : type-level functions that accept 1 type and return a type
- * -> * -> * : type-level functions that accept 2 types and return a type

Higher-Order Kinds : type constructors can be higher-order

> -- | https://en.wikipedia.org/wiki/Kind_(type_theory)
> -- kind : (* -> *) -> * -> *
> -- unt : unary data constructor, gets applied to its argument, that must be a type `z`
> --       returns another type
> data App unt z = Z (unt z) deriving (Eq, Show)
>
> e006 = U.t "e006" (Z (Just 3)) (Z (Just 3))

:i App
data App (unt :: * -> *) z = Z (unt z)

:t Z (Just 3)
Z (Just 3) :: Num z => App Maybe z

:t Z (Left 1)
Z (Left 1) :: Num a => App (Either a) z

------------------------------------------------------------------------------
Existentially quantified type : defines type that depends on a definite but unknowable type

useful for hiding type information that is not globally relevant

> -- | universally quantified value and two functions on that operate on that value
> data U a = forall a. U a (a -> Bool) (a -> String)

does not encapsulate value : it shows in types:

> uf1 :: U a -> Bool
> uf1 (U v f1 _) = f1 v
>
> uf2 :: U a -> String
> uf2 (U v _ f2) = f2 v
>
> u    = U 3 even show
> e007 = U.t "e007" (uf1 u)  False -- even 3
> e008 = U.t "e008" (uf2 u) "3"    -- show 3

> -- | existentially qualified (requires LANGUAGE ExistentialQuantification)
> -- hides type parameter (i.e., `a` no longer present on the left-hand side)
> -- (potential confusion: `forall` used in both universal and existential cases)
> data E   = forall a. E a (a -> Bool) (a -> String)

type parameter now hidden from type signatures:

> ef1 :: E -> Bool
> ef1 (E v f1 _) = f1 v
>
> ef2 :: E -> String
> ef2 (E v _ f2) = f2 v
>
> e    = E 3 even show
> e009 = U.t "e009" (ef1 e)  False -- even 3
> e010 = U.t "e010" (ef2 e) "3"    -- show 3

Encapsulated value can only be accessed via functions packaged with the value.
- there is no way to know what that type is — it could be anything

Cannot apply functions:

> -- INVALID
> -- getV (E v _ _) = v
> -- Couldn't match expected type ‘t’ with actual type ‘a’
> --   because type variable ‘a’ would escape its scope

Existential quantification provides the means to implement abstract datatypes
- providing functions over a type while hiding the representation of the type.

Can also do abstract datatypes via Haskell's module system
- hide algebraic datatype constructors while exporting functions over the type

Universal                    Existential
---------                    -----------
type parametrization         type abstraction

parametric polymorphism      encapsulation

user of data specifies type  implementer of data specifies type

forall = "for all"           forall = "for some"

TODO Skolemization

Every existential type can be encoded as a universal type. This process is called skolemization.

case class ListMap[B, A](list: List[B], mapf: B => A)

trait ListMapInspector[A, Z] {
  def apply[B](value: ListMap[B, A]): Z
}

case class AnyListMap[A] {
  def apply[Z](value: ListMapInspector[A, Z]): Z
}
Example: Instead of using ListMap directly, we use AnyListMap, which allows us to inspect a ListMap but only if we can handle any type parameter for B.

------------------------------------------------------------------------------
partially apply a higher-kinded type, yielding another type constructor with fewer type parameters

> type EitherString a = Either String a

------------------------------------------------------------------------------
3. Mastering Type Classes

type class : collection of types and operations defined on them

> class ShowRead a where
>     showSR :: a -> String
>     readSR :: String -> Either String a

type class instance : implementation of a type class for a given set of types

>
> instance ShowRead Int where
>     showSR x = show x
>     readSR x = Right ((read x) :: Int) :: Either String Int -- TODO : partial function

optional: with required but unchecked laws

Right Identity

> e011 = U.t "e011"
>            ((readSR (showSR (1::Int)))::Either String Int)
>            (Right 1)

Left Partial Identity

read(v).map(show(_)).fold(_ => true, _ == v)

------------------------------------------------------------------------------
4. Mastering Functional Patterns

------------------------------------------------------------------------------
Option, Either, Validation : commonly used to describe optionality and partiality

> data Maybe' a = Just' a | Nothing'
>
> data Either' a b = Left' a | Right' b
>
> data Validation a b = Failure a | Success b

------------------------------------------------------------------------------
Semigroup, Monoid

Semigroups : combine (aka "append") two things of same type into another thing of same type (e.g., addition over integers)

> class Semigroup' a where
>     append' :: a -> a -> a
>
> instance Semigroup' Int where
>     append' = (+)

Monoids    : semigroups and a "zero" element (e.g., 0 and addition)

> class Semigroup' a => Monoid' a where
>     zero' :: a
>
> instance Monoid' Int where
>     zero' = 0

Associativity

> e012 = U.t "e012" (append' (1::Int) (append' (2::Int) (3::Int)))
>                   (append' (append' (1::Int) (2::Int)) (3::Int))

Identity

> e013 = U.t "e013" (append' (1::Int) zero') 1

------------------------------------------------------------------------------
Functors : type constructor of kind `f :: * -> *`

most general : `f a` represents description of computation that may halt, run forever, or produce 0 or more a's

> -- | this is a covariant endofunctor
> -- there are other types of functors (e.g., invariant, contravariant)
> class Functor' (f :: * -> *) where
>     fmap' :: (a -> b) -> f a -> f b
>
> instance Functor' List where
>     fmap'   _        Nil  = Nil
>     fmap' fun (Cons x xs) = Cons (fun x) (fmap' fun xs)

Identity

> e014 = U.t "e014" (fmap' id (Cons 1 Nil)) (Cons 1 Nil)

Composition


Natural Transformation : polymorphic function that maps functor F[_] to functor G[_] (morphisms between morphisms)

denoted using `F ~> G`

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[A](fa: F[A]): G[A]
}
type ~> [-F[_], +G[_]] = NaturalTransformation[F, G]
Note: If you have F ~> G, and G ~> H, you can compose them to get F ~> H. In addition, for any F, there is an identity F ~> F.


> e015 = U.t "e015" (fmap' (+1) (fmap' (*10)  (Cons 1 (Cons 2 Nil))))
>                   (fmap' ((+1) .     (*10)) (Cons 1 (Cons 2 Nil)))

> test :: IO Counts
> test =
>     runTestTT $ TestList $
>         e001 ++ e002 ++ e003 ++ e004 ++ e005 ++ e006 ++ e007 ++ e008 ++ e009 ++ e010 ++
>         e011 ++ e012 ++ e013 ++ e014 ++ e015
