{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import           Control.Applicative (Const, WrappedMonad (..))
import           Control.Monad       ((<=<))
import           Data.Kind           (Type)

{-
https://blog.poisson.chat/posts/2019-06-09-free-monads-free-monads.html
Free monads of free monads
Lysxia June 9, 2019

A free monad is a monad.
The free monad functor, the thing that generates free monads, is also a monad.

------------------------------------------------------------------------------
Free monads

Will use the “freer monad”, a.k.a. “operational monad”, for simplicity.
- Not the same as Free from free library
  - requires an Functor constraint on its functions
- this post the difference between that and “freer” as a uninteresting detail.
-}

-- provides raw ingredients of a free monad
data Free :: (Type -> Type) -> Type -> Type where
  Pure   :: a                      -> Free f a
  Impure :: (x -> Free f a) -> f x -> Free f a -- HC : note typical arg order is reversed

{-
steps to show that Free f is a free monad with respect to f:

-- Free f is a monad
instance Monad (Free f)

-- defines “primitive operations” in the monad
call :: f ~> Free f

-- gives “interpretation” of primitive operations,
-- to an interpretation of whole Free f programs (which use these operations).
interpret :: Monad m => (f ~> m) -> (Free f ~> m)

(~>)
- constructs types of indexed functions (i.e., functions between indexed types)
- AKA "natural transformations"
- implies f, g :: k -> Type are functors, and domain k is a category
- but here index kind k can be arbitrary (not only Type)
  - it might not make sense as a category
-}

-- Indexed functions
type f ~> g = forall a. f a -> g a

{-
See also:

Why free monads matter, by Gabriel Gonzalez.
http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

The operational monad tutorial, by Heinrich Apfelmus.
https://apfelmus.nfshost.com/articles/operational-monad.html

Free and freer monads, by Oleg Kiselyov.
http://okmij.org/ftp/Computation/free-monad.html

------------------------------------------------------------------------------
What makes a free monad

the type Free f is a monad that satisfies the monad laws
-}

instance Monad (Free f) where

  return :: a -> Free f a
  return  = Pure

  (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  Pure a      >>= k = k a
  Impure xToFreefa fx >>= k = Impure (k <=< xToFreefa) fx

deriving instance Functor (Free f)
deriving via WrappedMonad (Free f) instance Applicative (Free f)

-- Can be viewed as an interface of “operations”, or “effects”.
-- “call/perform” operations using this
call :: f ~> Free f
call  = Impure Pure

{-
can write programs using call, pure and (>>=)

for any monad m and any function f ~> m,
there is a monad morphism Free f ~> m (i.e., a function which commutes with pure and (>>=)).

it is a fold of recursive type Free f, a catamorphism

the resulting function Free f ~> m is an interpretation of programs in the Free f monad
into another monad m,
generated from an interpretation f ~> m for the individual operations described by f.
-}

interpret :: Monad m => (f ~> m) -> (Free f ~> m)
interpret _ (Pure a)     = pure a
interpret fToM (Impure k fx) = fToM fx >>= (interpret fToM . k)

{-
Free monads have universal property relating call and interpret
- for any monad m and any indexed function h :: f ~> m
  the monad morphism 'interpret h'
  is unique solution of equation
     i . call = h
  with unknown
     i :: Free f ~> m:

------------------------------------------------------------------------------
Monad of free monads

Since Free g is a monad, we can specialize interpret with m = Free g:

interpret :: (f ~> Free g) -> (Free f ~> Free g)

The types of call and interpret are reminiscent of pure and (>>=) (i.e., flip (>>=))

pure :: Monad m => a ->    m a
call ::            f ~> Free f

flip (>>=) :: Monad m => (a ->    m b) -> (   m a ->    m b)
interpret ::             (f ~> Free g) -> (Free f ~> Free g)

Free :: (Type -> Type) -> (Type -> Type)
when viewed as a functor between indexed types, is a monad.
- maps an indexed type to its free monad: call it "the monad of free monads"

------------------------------------------------------------------------------
Indexed types

Free is a monad, but not of the kind represented by standard Monad class.
That class represents monads in the category of types and functions.
Now looking at categories of indexed types and indexed functions.

now leave monads aside for a while to investigate indexed types

An indexed type is a type with a parameter (or index)
i.e., a function k -> Type
      for some index kind k

examples with index kind k = Type
- functors : Maybe, [], (,) a, IO, (->) a
- any Type -> Type, e.g., Endo, or (:~:) a

unindexed types seen as indexed types where k = ()
() -> Type isomorphic to Type (with the Const functor being half of the isomorphism).

Indexed types can have many indices, equivalent to one index which is a tuple.
-}

newtype Uncurry :: (a -> b -> Type) -> ((a, b) -> Type) where
  UncurryOf :: f (Fst ab) (Snd ab) -> Uncurry f ab

data Curry :: ((a, b) -> Type) -> a -> b -> Type where
  CurryOf :: f ab -> Curry f (Fst ab) (Snd ab)

type family Fst (p :: (a, b)) :: a where
  Fst '(x, _) = x

type family Snd (p :: (a, b)) :: b where
  Snd '(_, y) = y

{-
Uncurry converts binary indexed types to unary

:k (,)
(,) :: * -> * -> *

:k Uncurry (,)
Uncurry (,) :: (*, *) -> *

:k Const
Const :: * -> k -> *

:k Uncurry Const
Uncurry Const :: (*, k) -> *


------------------------------------------------------------------------------
Indexed functors

generalize notion of functor

standard Functor class represents functors between types
-}

-- represents functors between indexed types (aka indexed functors)
-- Indexed functors
class Functor1 (w :: (k -> Type) -> (l -> Type)) where
  map1 :: (p ~> q) -> (w p ~> w q)

  -- Laws:
  --   map1 id = id
  --   map1 f . map1 g = map1 (f . g)

{-
(close sibling of Functor from the rank2classes library)

For every index k,
there is a category where objects are indexed types f, g of kind k -> Type,
and arrows are indexed functions f ~> g.

Note Functor1 may represent functors between different
source (k -> Type) and target categories (l -> Type) of indexed types.

This contrasts with Functor which only represents endofunctors on the one category of Type.

type variable naming
- w, v       : of kind (k -> Type) -> (l -> Type)
- f, g, p, q : of kind k -> Type
- use p , q to avoid confusion when comparing indexed and unindexed type signatures
  (e.g., Functor already uses f as instance head)

regular types are indexed types with k = ()

find two types ToFunctor1 and ToFunctor with two instances:

instance Functor  f => Functor1 (ToFunctor1 f)
instance Functor1 w => Functor  (ToFunctor  w)

What are their kinds?

the kind of ToFunctor1 can keep a general k kind variable,
though this indexed functor must have the same domain and codomain.

ToFunctor and ToFunctor1 are type-level functions,
but declare them as their own types instead of type synonyms to work with type classes.
-}

--                    w___________________________      a___
newtype ToFunctor :: ((() -> Type) -> (() -> Type)) -> (Type -> Type) where
  WrapToFunctor :: w (Const a) '() -> ToFunctor w a
-- ToFunctor w a = w (\_ -> a) '()

--                     f___________       p________      a
newtype ToFunctor1 :: (Type -> Type) -> ((k -> Type) -> (k -> Type)) where
  WrapToFunctor1   :: f (p a) -> ToFunctor1 f p a
-- ToFunctor1 f p a = f (p a)

{-
Interestingly, a functor between indexed types is itself an indexed type, with two indices. We can wrap it in Uncurry to get a singly-indexed type:

Uncurry w :: (k -> Type, l) -> Type
Functors, functors between functors, and so on, all of them fit in the same world of (singly-)indexed types with enough Curry.

Indexed recursive types
While we’re not on the topic of monads, Free is basically a variant of Fix, a fixpoint operator for types.

-- Fixed point of f: (Fix f) is isomorphic to (f (Fix f))
newtype Fix (f :: Type -> Type) = Fix (f (Fix f))
Recursive types thus constitute one fundamental aspect of free monads. Two concrete examples of indexed functors appear here; things get pretty abstract afterwards.

Let’s first review what we can do with unindexed recursive types. For example, a type of arithmetic expressions…

data Arith
  = Add Arith Arith
  | Number Int
… can be redefined as a fixed point of a functor ArithF, called its “base functor”.

data ArithF a
  = Add a a
  | Number Int

type Arith = Fix ArithF
One way this encoding is useful is that functors such as ArithF can be composed, forming new recursive types in a modular way; for example, we can thus add a constructor for multiplication to the language ArithF:

data MulF a = Mul a a

-- Arithmetic expressions extended with multiplication
type ArithWithMul = Fix (ArithF :+: MulF)
-- Sum of functors
data (f :+: g) a = L (f a) | R (g a)
This modularity of types extends to functions: we can define the meaning of operations in different functors independently, and compose them into the meaning of a whole language. See also the paper Data Types à la Carte, by Wouter Swierstra.

However, problems arise when the language becomes less “uniform”, which typically happens with the addition of types or binders.

Typed expressions with indexed functors
For example, assume we want to extend our arithmetic language with booleans by declaring constructs for boolean constants and conditionals:

data ConditionalF a
  = Boolean Bool
  | If a a a

-- Untyped Boolean and Arithmetic expressions
type UBArith = Fix (ArithF :+: ConditionalF)
Unfortunately, ill-typed terms plague that type such as True + 3 or if 99 then False else 0.

A conventional solution to make those terms unrepresentable is a GADT indexed by the type of the expression:

-- (Typed) Boolean and Arithmetic expressions
data BArith :: Type -> Type where
  Number :: Int -> BArith Int
  Add :: BArith Int -> BArith Int -> BArith Int
  Boolean :: Bool -> BArith Bool
  If :: BArith Bool -> BArith a -> BArith a -> BArith a
But that type cannot be expressed as the fixed point of a Functor, because the type BArith has a non-uniform structure: its recursive occurences have different indices.

To fix this problem, whereas the encoding of recursive types via Fix abstracted the recursive occurrences of a Type, we can instead abstract recursive occurrences of the type constructor BArith alone, of kind Type -> Type, leaving the type indices untouched.

data BArithF :: (Type -> Type) -> Type -> Type where
  Number :: Int -> BArithF f Int
  Add :: f Int -> f Int -> BArithF f Int
  Boolean :: Bool -> BArithF f Bool
  If :: f Bool -> f a -> f a -> BArithF f a
The indexed type BArith is indeed a fixed point of that indexed functor BArithF, with the following indexed variant of Fix to tie the knot. Unlike Functor1, we are now restricted to endofunctors (“fixed point” doesn’t make sense otherwise).

-- Fixed point of w: (Fix1 w) is isomorphic to (w (Fix1 w))
data Fix1 (w :: (k -> Type) -> (k -> Type)) :: k -> Type where
  WrapFix1 :: w (Fix1 w) a -> Fix1 w a
           -- w (Fix1 w)   ~> Fix1 w
           --   would be equivalent,
           --   but GHC doesn't like this syntax.

type BArith = Fix1 BArithF  -- :: Type -> Type
Comparing the kind of Fix1 above with Fix :: (Type -> Type) -> Type, we can see that we have replaced every Type with k -> Type.

Exercise: write the Functor1 instance for BArithF.

Functor1 instance for BArithF
It looks just like a Functor instance.

instance Functor1 BArithF where
  map1 f = \case
    Number n -> Number n
    Add x y -> Add (f x) (f y)
    Boolean b -> Boolean b
    If x y z  -> If (f x) (f y) (f z)
Statically checked binders: lambda calculus
Similarly, the exemplary language of anonymous functions, the lambda calculus, might look like this in the unindexed world:

data LambdaF a
  = Var Var    -- x       (variable)
  | App a a    -- f e     (application)
  | Lam Var a  -- \x -> e (abstraction)

type Lambda = Fix LambdaF
The issue with that representation is that it doesn’t guarantee variables are properly bound, and it offers no safety regarding variable capture.

Like in the previous section, a nice solution is to keep track the free variables of a term in its type, resulting in a typed De Bruijn representation. Interestingly, this does not require GADTs, only polymorphic recursion. Despite it’s name, that is a rather benign type-system feature, baked in Haskell since about forever.

data Lambda v
  = Var v
  | App (Lambda v) (Lambda v)
  | Lam (Lambda (Shift v))
-- Add one inhabitant to v: "1 + v". This is isomorphic to Maybe.
data Shift v = Here | There v
Exercise: define an indexed functor LambdaF such that Fix1 LambdaF is isomorphic to Lambda.

Definition of the indexed LambdaF
data LambdaF :: (Type -> Type) -> (Type -> Type) where
  Var :: v -> LambdaF f v
  App :: f v -> f v -> LambdaF f v
  Lam :: f (Shift v) -> LambdaF f v

instance Functor1 LambdaF where
  map1 f = \case
    Var x -> Var x
    App u v -> App (f u) (f v)
    Lam u -> Lam (f u)
Indexed monads
We now present the indexed generalization of the Monad class: monads in a category of indexed types, indexed monads.5 We flip (>>=) to avoid an explicit quantification over an index a :: k. At the same time, its nature as a natural transformation becomes more apparent, which also helps to see generalizations to “higher” categories. I like to name it subst, as substitution is a common source of intuition for monads.

-- Indexed monads
class Monad1 (w :: (k -> Type) -> (k -> Type)) where
  pure1 :: p ~> w p
  subst1 :: (p ~> w q) -> (w p ~> w q)

  -- Laws:
  --   subst1 pure1 = id
  --   subst1 k . pure1 = k
  --   subst1 h . subst1 k = subst1 (subst1 h . k)
At the beginning, we witnessed that Free is an indexed monad, which we can now record in the following instance.

instance Monad1 Free where  -- k = Type

  pure1 :: p ~> Free p
  pure1  = call

  subst1 :: (p ~> Free q) -> (Free p ~> Free q)
  subst1 = interpret
By definition, the Free f monad is a free monad (with respect to the indexed type f). What about the indexed monad Free we just defined? What does a free indexed monad even look like?6

Free indexed monads
From Fix to Free
As we’ve seen in the section on indexed recursive types, Fix ArithF is the type of arithmetic expressions, with constructs given by the base functor ArithF. Furthermore, Free is almost like Fix: Free ArithF a is the type of arithmetic expressions with “holes” of type a. If we set a = Void, the empty type, Free ArithF Void (“no holes”) is isomorphic to Fix ArithF. The function (>>=) fills those holes with expressions: that is substitution.

A straightforward definition of Free is thus to extend Fix with a constructor for those holes. The following FFree f a is isomorphic to Fix (f :+: Const a); it is Free in the free library:

-- Free monad (FFree f) with respect to the functor f
data FFree (f :: Type -> Type) (a :: Type)
  = Pure a                  -- Hole
  | Impure (f (FFree f a))  -- Fix-like constructor
Then the first Free definition at the top of this post can be obtained by composing the FFree we just defined with a free functor construction, also known as Coyoneda7: Free f is isomorphic to FFree (Coyoneda f).

Once we have Fix1, an indexed generalization of Fix, we can extend it similarly with a new constructor, to get FFree1 generalizing FFree.

-- Free indexed monad (FFree1 w) with respect to the indexed functor w
data FFree1
       (w :: (k -> Type) -> (k -> Type))
       (f :: k -> Type)
       (a :: k)
  = Pure1 (f a)
  | Impure1 (w (Free1 w f) a)
Here also, we will switch to the “freer” variant to avoid headaches with the Functor1 constraints that FFree1 would incur. This transformation can also be understood in terms of an indexed generalization of Coyoneda.

This is the free indexed monad we will be using in the rest of the post.

-- Free indexed monad (Free1 w) with respect to the indexed type w
data Free1
       (w :: (k -> Type) -> (k -> Type))
       (f :: k -> Type)
       (a :: k) where
  Pure1 :: f a -> Free1 w f a
  Impure1 :: (g ~> Free1 w f) -> w g a -> Free1 w f a
Free indexed monads
To demonstrate that Free1 w is really a free indexed monad, we will follow the recipe given at the beginning for Free f, adapted to indexed monads. As it turns out, the definitions are essentially the same as for Free f, but with more squiggly arrows. Here, the extra polymorphism is handled completely invisibly by Haskell’s type system.

First, Free1 w is an indexed monad.

instance Monad1 (Free1 w) where

  pure1 :: p ~> Free1 w p
  pure1 = Pure1

  subst1 :: (p ~> Free1 w q) -> (Free1 w p ~> Free1 w q)
  subst1 k (Pure1 x) = k x
  subst1 k (Impure1 h u) = Impure1 (subst1 k . h) u
Second, there is a function w ~~> Free1 w, which motivates the introduction of a double tilde-arrow for doubly-indexed functions.

call1 :: w ~~> Free1 w
call1 = Impure1 Pure1
-- Indexed functions with two indices (whereas (~>) is for one).
type v ~~> w = forall f a. v f a -> w f a
Third and last, given an indexed monad v :: (k -> Type) -> (k -> Type), for any function w ~~> v, there is an indexed monad morphism Free1 w ~~> v.

interpret1 :: Monad1 v => (w ~~> v) -> (Free1 w ~~> v)
interpret1 _ (Pure1 x) = pure1 x
interpret1 h (Impure1 k e) = subst1 (interpret1 h . k) (h e)
To recapitulate, we started with Free:

Free f is a (free) monad;
Free is an indexed monad, with call and interpret as its monadic operations.
Then we discovered Free1:

Free1 w is a (free) indexed monad.
There is an obviously missing piece:

Free1 is also an indexed monad, with two indices of kinds k -> Type and k, and with call1 and interpret1 as its monadic operations.
Monad of free indexed monads
We cannot directly make Free1 an instance of Monad1, because it has the wrong kind (their arities are different: Free1 is a ternary type while Monad1 expects a binary type). We must first wrap Free1 to have a compatible kind.

The wrapper is named Unspice, because it “uncurries” the indexed type. Although the wrappers have scary kinds and bloat the code, they are purely a compile-time device to adjust the kinds of types. There is no runtime cost (in principle8), and to understand how the code runs, we simply ignore the wrappers.

newtype Unspice ::
    (((k -> Type) -> (k -> Type)) -> ((k -> Type) -> (k -> Type))) ->
    ((k -> Type, k) -> Type     ) -> ((k -> Type, k) -> Type     ) where
  UnspiceOf :: Uncurry (ww (Curry w)) s -> Unspice ww w s
Unwrapping functions
These wrappers become the identity function at run time, so we can ignore them in order to understand the behavior of functions.

fromUnspice :: Unspice ww w ab -> ww (Curry w) (Fst ab) (Snd ab)
fromUnspice (UnspiceOf (UncurryOf u)) = u

toUnspice :: ww (Curry w) (Fst ab) (Snd ab) -> Unspice ww w ab
toUnspice = UnspiceOf . UncurryOf
The wrapped Free1 is thus named Free1'. At a high level, we can think of it as identical to Free1.

-- Equivalent to Free1
type Free1' = Unspice Free1
The kind of this wrapped Free1 is of the form (h -> Type) -> (h -> Type), as Monad1 expects; to be more specific, h = (k -> Type, k). Like the Free indexed monad, pure1 and subst1 are actually defined as call1 and interpret1, if we ignore the wrappers.

instance Monad1 Free1' where
  pure1 :: p ~> Free1' p
  pure1 = toUnspice . call1 . CurryOf
  -- pure1 = call1  -- ignoring wrappers

  subst1 :: forall p q.
    (p ~> Free1' q) -> (Free1' p ~> Free1' q)
  subst1 f = toUnspice . interpret1 f' . fromUnspice where
  -- subst1 = interpret1  -- ignoring wrappers

    f' :: Curry p ~~> Free1 (Curry q)
    f' (CurryOf x) = fromUnspice (f x)
    -- f' = f  -- ignoring wrappers
In the definition of subst1, we pull out f', a wrapped version of f. Because it is a polymorphic function consuming a GADT (Curry), it requires an explicit type signature, which refers to the type variables p and q bound at the top, thanks to ScopedTypeVariables. The function f' could also be inlined as an anonymous function, which wouldn’t need such a signature, though it seems messier in a different aspect.

That instance shows that Free1 is an indexed monad, while its image consists of free indexed monads: Free1 is the indexed monad of free indexed monads.

Free monads of free monads
This all started with the observation that a free monad functor (Free and Free1) is a monad, thus dubbed monad of free monads. Let us tackle the final question on everyone’s mind: is that monad a free monad, so that we can talk of the free monad of free monads?

In other words, we are looking for some indexed type Key (a key to free monads) such that Free1 Key is isomorphic to Free or Free1.

Sadly, there doesn’t seem to be a solution, though I don’t have a solid proof right now. It might be possible in a fancier type system with equation-carrying types, and by that I mean higher-inductive types or quotient types, which are still subjects of active research.

Nevertheless, we can still try to construct an indexed type Free1 Key with not all but most of the practical features of a monad of free monads.

Free monads for semantics
The Key we’re looking for is meant to be the interface of operations, or effects, of the free indexed monad Free1 Key, but what operations?

Earlier, we established that Free1 and Free1 w are both indexed monads. What about Free1 w f, that is applied twice? In general, there is nothing we can say about it; it doesn’t even have the kind which Monad expects (Free1 w f :: k -> Type). Therefore, if Free1 Key f is anything like the free monad Free f, its monadic structure must come from Key.

This suggests to define (=<<) and pure as effects of the free indexed monad Free1 Key. We thus define Key, using its first type parameter f :: Type -> Type to refer to the type of computations handled by the higher-order effect (=<<).

data Key :: (Type -> Type) -> (Type -> Type) where
  (:=<<:) :: (a -> f b) -> f a -> Key f b
          -- (a -> m b) -> m a ->     m b
  PureK :: a -> Key f a
Free monad of free pseudo-monads
The type Free1 Key is actually isomorphic to the following recursive type, with constructors for pure, (>>=), and call. Although it tries hard to look like a monad, it satisfies none of the three monad laws9: it is a pseudo-monad. We can thus recognize Free1 Key f as a free pseudo-monad.

data Free1K f a where
  (:=<<:.) :: (a -> Free1K f b) -> Free1K f a -> Free1K f b
  Pure1K :: a -> Free1K f a
  Call1K :: f a -> Free1K f a
However at the upper level, Free1 Key (without the f) is still a perfectly healthy free indexed monad: it is the free indexed monad of free pseudo-monads. To summarize:

Free1 Key is a free indexed monad (with respect to Key);
Free1 Key f is a free pseudo-monad (with respect to f).
We can again follow the recipe of free monads for Free1 Key f, ensuring that it is indeed a free pseudo-monad. Although the laws are not satisfied, the implementation still has some interesting details to show.

First, Free1 Key f is a pseudo-monad.

Since the Monad operations are actually provided as effects by Key, both return and (>>=) are defined using Free1’s Impure1 constructor.

instance Monad (Free1 Key f) where
  return x = Impure1 id (PureK x)
  u >>= k  = Impure1 id (k :=<<: u)

-- Deriving Functor and Applicative
deriving via WrappedMonad (Free1 Key f)
         instance Functor (Free1 Key f)
deriving via WrappedMonad (Free1 Key f)
         instance Applicative (Free1 Key f)
Second, there is a function f ~> Free1 Key f, to perform f effects in the pseudo-monad Free1 Key f.

The funny thing is that, while return and (>>=) were defined above as effects (Impure1), call is now defined as a pure action (Pure1). Actually, this story started by identifying call as pure1, so this makes sense, totally.

call1K :: f ~> Free1 Key f
call1K = Pure1
Third, for every function f ~> m, there is a pseudo-monad morphism Free1 Key f ~> m.

The simplest implementation is an explicitly recursive fold of the Free1 Key f tree. Replace (:=<<:) with =<<, PureK with pure, and type-tetris the rest until it typechecks.

interpret1K :: Monad m => (f ~> m) -> (Free1 Key f ~> m)
interpret1K h (Pure1 e) = h e
interpret1K h (Impure1 k (l :=<<: u))
  = (interpret1K h . k . l) =<< interpret1K h (k u)
interpret1K _ (Impure1 _ (PureK x)) = pure x
But we already have a way to fold Free1, that’s interpret1. It would be a shame not to use it. Who cares whether that’s a sane thing to do?

Recursion schemes for recursion schemes
We need an indexed monad as a target for the fold interpret1. It is possible to meticulously handcraft such an indexed monad, but here’s a hint out of left field: we have already seen a couple of indexed monads.

instance Monad1  Free
instance Monad1 (Free1 w)
instance Monad1  Free1'
In fact, Free is a pretty good candidate.10 To see why, specialize interpret1 with what we now have:

interpret1 :: Monad1 v => (w   ~~> v   ) -> (Free1 w   ~~> v   )
interpret1 ::             (Key ~~> Free) -> (Free1 Key ~~> Free)
If we supply a suitable Key ~~> Free function, we get to fold any Free1 Key f into Free f, which we already know how to interpret into another monad m, given f ~> m.

So we need to find Key ~~> Free. The type Key defines monadic composition as an “uninterpreted effect”, and Free is a monad, so we can hope this makes sense. We pattern-match on the Key and type-tetris our way out of each case. The function is named legitimize because it interprets the pseudo-monad Free1 Key into the monad Free.

           -- Equivalent signatures:
           -- Free1 Key f a  -> Free f a
           -- Free1 Key f    ~> Free f
legitimize :: Free1 Key     ~~> Free
legitimize = interpret1 \case
  k :=<<: u -> Impure (call . k) u
  PureK x -> Pure x
The PureK case looks reasonable, but (:=<<:) is intriguing.

In this context we have k :: a -> g b and u :: g a, for some abstract type g (and a, b), and we need to construct Free g b. This g comes from the universal quantification in Key ~~> Free in the argument of interpret1, that’s why g is abstract. The only sensible action for us is to compose two call (the first one is inlined and becomes Impure) to u and k respectively. It is an oddly simple Free g program to write.

-- The (:=<<:) branch of legitimize
Impure (call . k) u
  = do x <- call u
       call (k x)
Since interpret1 only knows about the Monad1 instance of Free, we can expect it to call subst1, i.e., interpret, on that small program of two call, to interpret g into an actual monad. Intuition about parametricity and abstract nonsense tell me it’s probably doing the right thing, but it would be very interesting to work through the proof of correctness.

We finally obtain our alternative implementation of the Free1 Key f interpreter, refactoring the main recursive fold into interpret1 and interpret.

interpret1K' :: Monad m => (f ~> m) -> (Free1 Key f ~> m)
interpret1K' h = interpret h . legitimize
legitimize thus takes us from Free1 Key to Free, and then interpret from Free f (a different monad from Free!) to m.

Free monad of free indexed pseudo-monads
Now let’s repeat the process for Free1. We will approximate Free1 as Free1 Key1 for some type Key1, so that:

Free1 Key1 is a free indexed monad (with respect to Key1);
Free1 Key1 w is a free indexed pseudo-monad (with respect to w).
The code will be mostly the same as that of Free1 Key we just saw, but with many wrappers to curry and uncurry indexed types. Here, typed holes were invaluable to remain sane.

The functor Key1 is the indexed analog of Key: every Type becomes k -> Type, and we insert type indices where necessary.

data Key1 ::
    ((k -> Type) -> (k -> Type)) ->
    ((k -> Type) -> (k -> Type)) where
  (:=<<:!) :: (p ~> w q) ->  w p b -> Key1 w q b
           -- (p ~> w q) -> (w p   ~> Key1 w q  )
  PureK1 :: p a -> Key1 w p a
         -- p   ~> Key1 w p
We must then solve a kind mismatch, for similar reasons to those motivating Unspice. The first parameter of Free1 must have a kind of shape (h -> Type) -> (h -> Type), which is binary (h -> Type and h), while Key1 is a ternary indexed type. We have to Unspice (“uncurry”) the indexed type Key1, so that we can apply Free1 to it, with h = (k -> Type, k).

Free1 (Unspice Key1) ::
  ((k -> Type, k) -> Type) -> ((k -> Type, k) -> Type)
Then, we must curry that back, with the Spice wrapper below, so that Spice (Free1 (Unspice Key1)) has the same kind as Key1, which is the kind one would expect of a monad of free indexed monads.

newtype Spice ::
    (((k -> Type, k) -> Type   ) -> ((k -> Type, k) -> Type)   ) ->
    ((k -> Type) -> (k -> Type)) -> ((k -> Type) -> (k -> Type)) where
  SpiceOf :: Curry (ww (Uncurry w)) f a -> Spice ww w f a
Wrappers for Spice.
fromSpice :: Spice ww w f a -> Curry (ww (Uncurry w)) f a
fromSpice (SpiceOf u) = u

toSpice :: ww (Uncurry w) '(f, a) -> Spice ww w f a
toSpice = SpiceOf . CurryOf
We hide those wrappers with some synonyms. Unspice and Spice only do (un)currying, which doesn’t fundamentally alter the structure of Key and Free1 Key1'. At a high level, think of Key1' and Free1Key1' as identical to Key1 and “Free1 Key1” (which is technically ill-typed without Key1').

type Key1' = Unspice Key1
type Free1Key1' = Spice (Free1 Key1')
Mirroring the situation for Free1 Key, the type Free1Key1' is the free indexed monad of free indexed pseudo-monads. It is isomorphic to this standalone recursive type, analog of Free1K:

data Free1K1 ::
    ((k -> Type) -> (k -> Type)) ->
    ((k -> Type) -> (k -> Type)) where
  Call1K1 :: w f a -> Free1K1 w f a
  (:=<<:!.) :: (g ~> Free1K1 w f) -> Free1K1 w g a -> Free1K1 w f a
  Pure1K1 :: f a -> Free1K1 w f a
Let’s run the free monad recipe again, for the fourth and last time, to verify that Free1Key' w is a free indexed pseudo-monad.

First, Free1Key1' w is an indexed pseudo-monad.

instance Monad1 (Free1Key1' w) where

  pure1 :: p ~> Free1Key1' w p
  pure1 x = toSpice (Impure1 id (toUnspice (PureK1 x)))
  -- pure1 x = Impure1 id (PureK1 x)  -- ignoring wrappers

  subst1 :: forall p q.
    (             p ~> Free1Key1' w q) ->
    (Free1Key1' w p ~> Free1Key1' w q)
  subst1 k (SpiceOf (CurryOf u))
    = toSpice (Impure1 id (toUnspice
        ((fromSpice . k) :=<<:! CurryOf u)))
  -- subst1 k u = Impure1 id (k :=<<:! u)  -- ignoring wrappers
Second, there is a function w ~~> Free1Key1' w.

call1K1 :: w ~~> Free1Key1' w
call1K1 = toSpice . Pure1 . UncurryOf
-- call1K1 = Pure1  -- ignoring wrappers
Third, there is an interpreter into any indexed monad v. We apply the idea from the previous section, to first interpret Free1 Key1' into Free1' (the indexed analog of Free), and then use Free1'’s interpreter to reach v.

-- From the free indexed pseudo-monad Free1 Key1'
-- to   the free indexed        monad Free1'
legitimize1 :: Free1 Key1' ~~> Free1'
legitimize1
  = interpret1 (toUnspice . h . fromUnspice)
  where
    h (k :=<<:! u) = Impure1 (call1 . k) u
    h (PureK1 x)   = Pure1 x
Hopefully by now you’re used to the feeling of déjà vu. Everything is the same as in the previous case, sprinkled with squiggly arrows and wrappers.

All recursion is hidden behind folds (interpret1).

In interpret1K1, the legitimize1 function first takes us from Free1 Key1' to Free1', and then interpret1 takes us from Free1 w (a different indexed monad from Free1!) to v.

-- Interpreter of the free indexed monad Free1Key1'
interpret1K1 :: forall w v.
  Monad1 v => (w ~~> v) -> (Free1Key1' w ~~> v)
interpret1K1 h (SpiceOf (CurryOf u))
  = (interpret1 h' . fromUnspice . legitimize1) u

-- interpret1K1 h
--   = interpret1 h . legitimize  -- ignoring wrappers

  where
    h' :: Curry (Uncurry w) ~~> v
    h' (CurryOf (UncurryOf x)) = h x
    -- h' = h  -- ignoring wrappers
There is something funny going on in the specialization of interpret1 used by legitimize1: we are interpreting the free indexed monad Free1 Key1' into Free1, that is the very indexed monad which generates that free indexed monad we started from!

interpret1 :: Monad1 v => (w     ~~> v     ) -> (Free1 w     ~~> v     )
interpret1 ::             (Key1' ~~> Free1') -> (Free1 Key1' ~~> Free1')
The unreasonable effectiveness of types
The code after the introduction of Unspice was largely driven by typed holes.

On the one hand, it is so abstract that it’s hard to follow what is going on in detail. On the other hand, there is much structure to rely on: the types are so precise that there is only one way forward most of the time, and recurrent patterns across the different sections further guide the implementation.

Conclusion
This is the end of today’s story, though the journey continues.

I would certainly like to see how free indexed monads fare compared with other systems of higher-order effects (e.g., Effect Handlers in Scope, by Nicolas Wu et al.).

Those were four takes on “monads of free monads”, and two of them are themselves defined as free monads, although they actually generate pseudo-monads.

The indexed monad of free monads, Free.

Monad (Free f)
Monad1 Free
The indexed monad of free indexed monads, Free1.

Monad1 (Free1 w)
Monad1 Free1'
The free indexed monad of free pseudo-monads, Free1 Key.

Monad (Free1 Key f)
Monad1 (Free1 Key) (specialization of Monad1 (Free1 w))
The free indexed monad of free indexed pseudo-monads, Free1 Key1.

Monad1 (Free1Key1' w)
Monad1 (Free1 Key1) (specialization of Monad1 (Free1 w))
Appendix A: I can handle myself
Another idea to wrap your head around.

At the level of unindexed monads, we can write the following function iter, which repeats an action updating an a state until it finishes with a b.

iter :: Monad m => (a -> m (Either a b)) -> a -> m b
iter run a = run a >>= either (iter run) pure
The same combinator can be implemented for indexed monads.

iter1 :: Monad1 v => (f ~> v (f :+: g)) -> (f ~> v g)
iter1 run a = subst1 (either1 (iter1 run) pure1) (run a)

-- Indexed sum eliminator
either1 :: (f ~> h) -> (g ~> h) -> (f :+: g ~> h)
either1 l _ (L a) = l a
either1 _ r (R b) = r b
But remember that in the indexed world, the monadic bind of Free is actually the interpretation of effects (interpret). So the first argument of iter1 is really an effect handler f ~> Free (f :+: g), which is made to handle its own f effects.

See also Turing Completeness Totally Free (PDF), by Conor McBride.

Appendix B: Beware of bugs in the above code
I have not proved it correct, not even tried it.

Now that you mention it, it might be a good idea to test it at least once.

main :: IO ()
main = do
  -- Test for Free1 Key
  -- Should print [1,2,3,4,5]
  print (interpret1K id do
    tf <- call1K [True, False]
    if tf then
      call1K [1, 2, 3]
    else
      call1K [4, 5 :: Int])

  -- There should also be a test for Free1Key1'...




Or simply “programming” as it is going to be called once everyone programs in Coq… That was a joke, of course. Everyone will prefer Agda.↩︎

To catch a glimpse of the problem, try to write a function of type f ~> Uncurry (Curry f) with a different version of Uncurry or Curry which uses the pair constructor '(,) explicitly instead of Fst and Snd. This affair is also sort of related to the business of codata (blogpost by Javier Casas, linking to a paper by Paul Downen et al.).↩︎

There are different variants of indexed monads. This one first appears in Kleisli Arrows of Outrageous Fortune (PDF), by Conor McBride. With a pinch of singletons, those are nice monads for dependently-typed programming even pre-Dependent Haskell. A possibly more common kind of indexed monads is i -> i -> Type -> Type, discussed in Parameterized Notions of Computation, by Robert Atkey, among many places. For example, it is used by the motor library for type-safe state machines. Graded monads, indexed by monoids, are also closely related.↩︎

The notion of “free” is relative to a notion of “forgotten structure”, for Free, the base category would be that of indexed types with the kind of Free: ((Type -> Type) -> (Type -> Type)).↩︎

See also the blog post Yoneda intuition from humble beginnings (reddit thread).↩︎

Except for Curry because existential newtypes are not yet supported.↩︎

There is actually a less naive Monad instance satisfying two of the laws, following the logic of a good free monad, in a type that’s too big to ever be one.↩︎

Since I couldn’t give that hint to myself, I had to construct the right indexed monad from scratch. I found the free monad, but finally encoded, which doesn’t change much to this story.↩︎

Site proudly generated by Hakyll
-}
