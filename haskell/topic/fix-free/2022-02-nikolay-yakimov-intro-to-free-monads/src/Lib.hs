{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TupleSections      #-}


module Lib where

import           Control.Monad.Free
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict
import           Control.Monad.Trans.Writer.CPS
import           Data.List                        (intercalate)

{-
https://serokell.io/blog/introduction-to-free-monads

Introduction to Free Monads
Nikolay Yakimov
February 1st, 2022

free monads : a way to get a Monad instance for any Functor

useful in cases that dealing with tree-like structures, e.g.,
- AST for an eDSL using do-notation
- different semantics for same monad
- build a decision-tree type structure using do-notation for non-determinism
  (like with lists, but for trees)

above is achievable with regular monads and newtype wrappers
free monads enable reducing boilerplate

------------------------------------------------------------------------------
Free algebraic structures

In abstract algebra, “free X” mean it is in some sense a minimal structure
that satisfies conditions for being X.

“free X” satisfies all laws for X and does not have any other laws describing it.

in this context, “free” means “unrestricted”
- it is not that we can get a structure “for free”,
- but that no other laws are restricting the structure apart from those absolutely necessary.

That said, in most cases, actually do get a structure “for free”

formally
free structure over a set S is
- a set M
- with operations on elements of M, such that:
  - there is an embedding i : S → M
  - M is a minimal closure
    - it only contains all the results of applying i to elements of S, and
    - any results of applying the operations, defined as part of the structure,
      to the elements of M.
    - The only laws that hold for the generated structure are those required to hold.

An embedding is an injective morphism.
- an embedding S → M maps elements of S to elements of M
  such that each element of S is mapped onto a unique element of M (injectivity), and,
  in some sense, this mapping is structure-preserving (i.e. it’s a morphism).
- every element of original set is uniquely representable in the free structure

example : Monoids

Monoid type class
- to construct a neutral element : mempty :: α
- combine two elements           : (<>) :: α -> α -> α

must satisfy the monoid laws:

    (x <> y) <> z = x <> (y <> z) -- associativity
                  mempty <> x = x -- left identity
                  x <> mempty = x -- right identity

A free monoid has to have
- these two operations
- embedding of some underlying set
- satisfy these laws and only them

example : list/sequence of S elements is a free monoid over S

type M = [S]

-- embedding
i :: S -> M
i x = [x]

mempty = []
(<>)   = (++)

example : addition over integers is NOT a free monoid
- implies law of commutativity, which is not a monoid law

example : addition over non-negative integers because, for naturals, commutativity follows from associativity:

n+k = (1+…+1) + (1+…+1) = (1+…+1) + (1+…+1) = k + n
      n times   k times   k times   n times

Does not hold if integers can be of different signs.
assume, e.g., n < 0 < k : impossible to get k+n from n+k by reshuffling parentheses

a free monoid does not do anything interesting
so we can “recover” any other monoid from a free one.

formally, if M is a free monoid over S,
then for any monoid N,
given a map f : S -> N
can extend this mapping to a monoid morphism f' : M -> N

implies that all free monoids over the same set are isomorphic

since addition with naturals is a free monoid over some set S,
can convert this to a list monoid over the same set.

trick is in choice of the set:
if we choose S = {1}
can map addition to concatenation
and natural numbers to lists of corresponding lengths.
Hence, addition with naturals is a free monoid over a singleton set.

Similar to monoids, any set- and group-theoretic construction can, have a free counterpart
e.g., free groups or free rings

------------------------------------------------------------------------------
Monads

compare monads to monoids

class Functor m => Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

monad laws:

    (m >>= g) >>= h = m >>= (\x -> g x >>= h) -- associativity
                         return a >>= h = h a -- left identity
                             m >>= return = m -- right identity

laws similar to monoid laws

Monad definition makes it a hard to see connection,
so rewrite laws using the Kleisli composition:

    (>=>)   :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
    f >=> g  = \x -> f x >>= g


            (f >=> g) >=> h = f >=> (g >=> h) -- associativity
                             return >=> f = f -- left identity
                             f >=> return = f -- right identity

On equivalence

monad laws expressed via Kleisli arrows imply monad laws expressed via >>= and vice versa

This is why a monad is indeed a monoid
- it satisfies the same laws,
- and the “values” are of the type Functor f => a -> f b

Note, his is not the category of endofunctors.
But there exists an isomorphism with the category of endofunctors.

On endofunctors

could define monads via
    join :: m (m a) -> m a
instead of >>=

make it more evident that monads are monoids over endofunctors

intuition gained by looking at free monoids
- return and >=> correspond to mempty and (<>)
- for any Functor f :: Type -> Type,
  there is a corresponding free Monad Free f :: Type -> Type

------------------------------------------------------------------------------
Free monads

try to construct Free f by analogy

lists are free monoids
-}

data List   a = NilL   | Cons  a (List a)
{-
data Free f a = Nil  a | Cons  f (Free f) a -- does not compile
-}
data Fre  f a = Pur  a | Fre  (f (Fre  f a))
{-
Free similar to a list, but more general
- functor is arbitrary
  - potentially making it a tree with branches of type f and leaves of type a
  - leaves are encoded via Pure, branches via Free
  - leaves correspond to pure values, branches correspond to monadic “actions”

IMPORTANT : free monad is similar to a list, but the “continuation” (i.e. Free)
can be a branching structure, depending on the choice of the base functor f.

-- as a GADT
data Free :: (Type -> Type) -> Type -> Type where
  Pure :: a -> Free f a            -- similar to 'return'
  Free :: f (Free f a) -> Free f a -- similar to join :: Monad m => m (m a) -> m a

can alternatively define monads in terms of fmap, return, and join

above provides insight into why such a structure works as an encoding for a free monad.

-- need f to be a Functor to recursively descend the Free branch
instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free g) = Free (fmap f <$> g)

instance Applicative f => Applicative (Free f) where
  pure  = Pure
  Pure f <*> m =        fmap  f   m
  Free f <*> m = Free $ fmap (<*> m) f

instance Applicative f => Monad (Free f) where
  return = Pure
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)

note >>= similar to fmap
If defined Monad instance in terms of join, would look this:

join (Pure x) = x
join (Free x) = Free $ join <$> x

impl not efficient
more efficient is Church-encoded free monads
core idea the same

'free' package : free monads, applicative and alternative functors, cofree comonads, ...

Use free monads to define a computation as a data structure.
Data structure does not define how computation is performed.
Write interpreters

------------------------------------------------------------------------------
State as a free monad
-}

newtype StateF s a = StateF { runStateF :: s -> (a, s) }
  deriving stock Functor

getF :: StateF s s
getF = StateF $ \s -> (s, s)

putF :: s -> StateF s ()
putF s = StateF $ const ((), s)

type StateHC s = Free (StateF s)

instance Applicative (StateF s) where
  pure a   = StateF (a,)
  sf <*> sa = StateF $ \s ->
    let f = fst (runStateF sf s)
        a = fst (runStateF sa s)
     in (f a,s)

{-
lift state operations into monad.
To lift any a into Free f a, apply Pure:

get :: State s s
get = Free $ Pure <$> getF

common pattern, so free lib provides

liftF :: (Functor f, MonadFree f m) => f a -> m a

MonadFree is MTL type class for different encodings of the free monad.
For more flexibility, instead of directly using Free and Pure,
one would generally use wrap (defined in MonadFree) and pure respectively.

-- simplified
liftF :: Functor f => f a -> Free f a
liftF fa = Free $ Pure <$> fa
-}

getHC :: StateHC s s
getHC  = liftF getF

putHC :: s -> StateHC s ()
putHC  = liftF . putF

someComputation :: StateHC Int ()
someComputation = do
  i <- getHC
  putHC $ i + 1
  pure ()

-- 'someComputation' creates a data structure.  It does not do anything.  Need interpreters.

runStateHC :: StateHC s a -> s -> (a, s)
runStateHC (Pure x) s = (x, s)
runStateHC (Free f) s =
  let (m, s') = runStateF f s
   in runStateHC m s'

rs :: ((), Int)
rs = runStateHC someComputation 0

-- Above moves the impl of >>= to runState.

printState :: (Show s, Show a) => StateHC s a -> s -> String
printState (Pure x) s = "pure (" <> show x <> "," <> show s <> ")"
printState (Free m) s =
  let (x, s') = runStateF m s
   in "state change " <> show s <> " -> " <> show s' <> "\n" <> printState x s'

ps :: String
ps = printState someComputation 0

{-
"state change 0 -> 0\nstate change 0 -> 1\npure ((),1)"
- first  : corresponds to get
- second : corresponds to put
- third  : corresponds to pure

SUMMARY
- can take any base functor for some monad and get a Monad instance “for free”.
- need to define the semantics oby writing a interpreters

------------------------------------------------------------------------------
List as a free monad

Not all free monads behave the same as their regular counterparts.
E.g., list monad encodes non-determinism
- list represents all possible outcomes of some event
- then handle those events nee by one

a free monad for lists behavior different
-}

listComputation :: Free [] Int
listComputation = do
  x <- liftF [1, 2, 3]
  y <- liftF [10, 20]
  z <- liftF [100, 200]
  pure $ x+y+z

printFreeList :: Show a => Free [] a -> String
printFreeList (Pure x) = show x
printFreeList (Free f) = "[" <> intercalate "," (printFreeList <$> f) <> "]"

pfl :: String
pfl = printFreeList listComputation

{-
"[[[111,211],[121,221]],[[112,212],[122,222]],[[113,213],[123,223]]]"

unlike the regular list monad
- produces nested lists
- it is a rose tree
- can get regular list monad behavior by concatenating nested lists

can restore any regular monad behavior from a free monad
since it doesn’t define any semantics beyond those necessary for any monad

'free' pkg provides

foldFree :: Monad m => (forall x. f x -> m x) -> Free f a -> m a

Given a natural transformation converting f x into monad m x for any x
can convert Free f a into m a

On natural transformations

natural transformation : structure-preserving transformation between two functors.
Here, naturality condition is enforced by parametricity : function is polymorphic in x.

Returning to our list example, we can get the original list behavior by applying foldFree id:

> foldFree id listComputation
[111,211,121,221,112,212,122,222,113,213,123,223]

------------------------------------------------------------------------------
Free monads for eDSLs

calculator language : reads ints from IO, adds them, prints result

key point : if next action depends on previous, encode as a continuation
- a function accepting some argument and returning the functor parameter

addition : two operands and the continuation
- continuation accepts result of addition and (eventually) returns some result

If a continuation does not accept a value, encode it as its end result
-}

-- t : operation arguments
-- a : overall result
data ASTF t a
  = Add    t t (t -> a)
  | Input      (t -> a)
  | Output t         a
  deriving Functor

{-
Add takes two arguments t and its result is also t.
The continuation returns type a, i.e., more flexibility in continuation.

Could have defined Output t (() -> a), since output does not have a result.
Omitted extraneous argument and simply encode continuation as a value.
-}

type FreeAST t = Free (ASTF t)

-- instance Applicative (ASTF t) where
--   pure      = Input . const
--   --                                 TODO
--   (<*>) (Add l1t l2t l3tab) (Add r1t r2t r3ta) = Add l1t l2t (l3tab r1t . r3ta)
--   (<*>) (Add l1t l2t l3tab) (Input        ita) = Add l1t l2t (l3tab l1t . ita)
--   (<*>) (Add l1t l2t l3tab) (Output t a)       = Add l1t l2t (\_ -> l3tab t a)

--   (<*>) (Input       itab)  (Add r1t r2t rta)  = Add r1t r2t (itab r1t . rta)
--   (<*>) (Input       itab)  (Input       ita)  = Input (\t -> itab t (ita t))
--   (<*>) (Input       itab)  (Output t a)       = Output  t   (itab t a)

--   --           TODO
--   (<*>) (Output  t ab)      (Add r1t r2t rta)  = Add r1t r2t (ab . rta)
--   (<*>) (Output  t ab)      (Input       rta)  = Output  t   (ab (rta t))
--   --           TODO
--   (<*>) (Output lt ab)      (Output t a)       = Output  t   (ab a)

input :: FreeAST t t
input = liftF $ Input id

add :: t -> t -> FreeAST t t
add x y = liftF $ Add x y id

output :: t -> FreeAST t ()
output x = liftF $ Output x ()

program :: FreeAST a ()
program = do
  x <- input
  y <- input
  res <- add x y
  output res

computeAST :: FreeAST Int () -> IO ()
computeAST = foldFree go
 where
  go :: ASTF Int x -> IO x
  go  = \case
    Add    x y next -> pure $ next (x + y)
    Input      next -> next . read <$> getLine
    Output x   next -> do print x; pure next

cpast :: IO ()
cpast  = computeAST program

{-
convert FreeAST String into WriterT String (State Int).
naming variables using a counter in the State
producing printed output using the Writer
argument type t is set to String since terms will be the variable names, not values.
-}

printAST :: FreeAST String () -> String
printAST fast = snd $ evalState (runWriterT $ foldFree go fast) 0
 where
  freshVar :: State Int String
  freshVar = do
    n <- get
    put $ n + 1
    pure $ "x" <> show n
  go :: ASTF String a -> WriterT String (State Int) a
  go  = \case
    Add x y g -> do
      var <- lift freshVar
      tell $
        var <> " <- add " <> x <> " " <> y <> "\n"
      pure $ g var
    Input x -> do
      var <- lift freshVar
      tell $ var <> " <- input\n"
      pure $ x var
    Output s next -> do
      tell $ "output " <> s <> "\n"
      pure next

past :: String
past = printAST program

{-
Up to this point, ASTF constructors have one parameter that had anything to do with the functor.
What happens if there are several?: branching computation
-}
data ASTF2 t a
  = Add2 t t (t -> a)
  | Input2 (t -> a)
  | Output2 t a
  | If2 t a a
  deriving Functor

type FreeAST2 t = Free (ASTF2 t)

if' :: t -> FreeAST2 t a -> FreeAST2 t a -> FreeAST2 t a
if' cond t f = Free $ If2 cond t f

-- instead of liftF, use Free directly : branches already have type FreeAST
-- in general use wrap defined in MonadFree instead of Free directly

computeAST2 :: FreeAST2 Int () -> IO ()
computeAST2 = foldFree go
 where
  go :: ASTF2 Int x -> IO x
  go  = \case
    Add2    x y next -> pure $ next (x + y)
    Input2      next -> next . read <$> getLine
    Output2 x   next -> do print x; pure next
    -- only thing that changed:
    If2 cond t f     -> if cond /= 0 then pure t else pure f
{-
defines semantics of branching, with 0 as false, otherwise true

Since branching operation introduces non-determinism into the computation,
using  WriterT (State Int) does not work.
-}

printAST2 :: FreeAST2 String a -> String
printAST2 fast = snd $ evalState (runWriterT $ go fast) 0
 where
  freshVar :: State Int String
  freshVar = do
    n <- get
    put $ n + 1
    pure $ "x" <> show n
  go :: FreeAST2 String a -> WriterT String (State Int) ()
  go (Pure _) = pure ()
  go (Free f) = case f of
    Add2 x y g -> do
      var <- lift freshVar
      tell $
        var <> " <- add " <> x <> " " <> y <> "\n"
      go $ g var
    Input2 x -> do
      var <- lift freshVar
      tell $ var <> " <- input\n"
      go $ x var
    Output2 s next -> do
      tell $ "output " <> s <> "\n"
      go next
    If2 cond onTrue onFalse -> do
      tell $ "if' " <> cond <> " (do\n"
      _ <- go onTrue
      tell "\n) (do\n"
      _ <- go onFalse
      tell "\n)\n"
{-
Instead of pure, use explicit recursion, and handle the Pure case explicitly.
For simplicity, the return type of go is fixed at ().

surprised by the output
-}

input2 :: FreeAST2 t t
input2 = liftF $ Input2 id

add2 :: t -> t -> FreeAST2 t t
add2 x y = liftF $ Add2 x y id

output2 :: t -> FreeAST2 t ()
output2 x = liftF $ Output2 x ()

someAST :: FreeAST2 a ()
someAST  = do
  x   <- input2
  y   <- input2
  res <- if' x (add2 x y) (pure y)
  output2 res

past2 :: String
past2 = printAST2 someAST

{-
x0 <- input
x1 <- input
if' x0 (do
x2 <- add x0 x1
output x2
) (do
output x1
)

Because when free monad built, continuation is passed to each place where base functor
is recursive in its parameter.  Mean all code after “if’” gets copied to both branches.
No workaround because free monads build trees, not general graphs.

------------------------------------------------------------------------------
Decomposing ASTs

do not have to define whole AST at once
-}

data ArithASTF3 t a
  = Add3 t t (t -> a)
  -- + Sub, Mul, etc
  deriving Functor

data IOASTF3 t a
  = Input3 (t -> a)
  | Output3 t a
  deriving Functor

data ASTF3 t a
  = Arith3 (ArithASTF3 t a)
  | IO3 (IOASTF3 t a)
  deriving Functor

type FreeAST3 t = Free (ASTF3 t)

input3 :: FreeAST3 t t
input3 = liftF $ IO3 $ Input3 id

add3 :: t -> t -> FreeAST3 t t
add3 x y = liftF $ Arith3 $ Add3 x y id

output3 :: t -> FreeAST3 t ()
output3 x = liftF $ IO3 $ Output3 x ()

-- likewise with interpreters

computeAST3 :: FreeAST3 Int () -> IO ()
computeAST3  = foldFree computeASTF3

computeASTF3 :: (Num t, Read t, Show t) => ASTF3 t x -> IO x
computeASTF3  = \case
  Arith3 c -> pure $ computeArith3 c
  IO3    c -> computeIO3 c

computeArith3 :: Num t => ArithASTF3 t a -> a
computeArith3 (Add3 x y next) = next (x + y)

computeIO3 :: (Read t, Show t) => IOASTF3 t a -> IO a
computeIO3  = \case
  Input3    next -> next . read <$> getLine
  Output3 x next -> do print x; pure next

{-
------------------------------------------------------------------------------
Trees as free monads

build a binary search tree from a sorted list
-}

-- base functor for binary tree
-- leaf and/or nil branch encoded as Maybe l
data BinTreeF l a = NodeF l a a
  deriving Functor

type FreeBinTree l = Free (BinTreeF l)

buildBalanced :: [Int] -> FreeBinTree Int (Maybe Int)
buildBalanced  [] = pure Nothing
buildBalanced [x] = pure $ Just x
buildBalanced  xs =
  case splitAt (length xs `div` 2) xs of
    (l,x:r) -> do
      b <- liftF $ NodeF x l r
      buildBalanced b
    (_,_)   -> error "X"
{-
Multiple functor variables per constructor encode non-determinism.
Passing left/right halves to NodeF, then bind it to name b
This splits omputation into two branches.

But no simple way to work with a tree wrapped in Free.
Convert into a regular binary tree.
-}

data BinTree l = Nil | Leaf l | Branch l (BinTree l) (BinTree l)
  deriving Show

-- Pure values correspond to leaves.
-- Free values correspond to branches.
convert :: FreeBinTree a (Maybe a) -> BinTree a
convert (Pure Nothing)  = Nil
convert (Pure (Just x)) = Leaf x
convert (Free f)        = let NodeF x l r = convert <$> f in Branch x l r

cv :: BinTree Int
cv  = convert $ buildBalanced [0..10]

{-
convert $ buildBalanced [0..10]
5 (2 (1 0
        Nil)
     (4 3
        Nil))
  (8 (7 6
        Nil)
     (10 9
         Nil))

------------------------------------------------------------------------------
Conclusions

Free monads
- “free” because they do not impose additional constraints
  beyond those required by the definition of a monad
- They are a particular type of a free algebraic structure, similar to free monoids
- build tree-like structures
- can interpret those structures
- any regular Haskell monad can be implemented as a free monad with a corresponding interpreter.
- a free monad can be converted to any other monad via a natural transformation.
- use-case : ASTs for eDSLs
- can use anywhere where a tree could be used

Freer : to get a monad, do not even need a functor (more free than free).
See Free and Freer Monads: Putting Monads Back into Closet
https://okmij.org/ftp/Computation/free-monad.html

code on https://github.com/lierdakil/free-monad-examples

------------------------------------------------------------------------------
Exercises

* Implement Reader and Writer using Free.
Template to get started:

import Control.Monad.Free

newtype ReaderF r a = ReaderF { runReaderF :: r -> a }
  deriving Functor

type Reader ...

ask :: Reader r r
ask = undefined

runReader :: Reader r a -> r -> a
runReader = undefined

newtype WriterF w a = WriterF { runWriterF :: (w, a) }
  deriving Functor

type Writer ...

tell :: w -> Writer w ()
tell = undefined

listen :: Monoid w => Writer w a -> Writer w (a, w)
listen = undefined

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass = undefined

runWriter :: Monoid w => Writer w a -> (a, w)
runWriter = undefined

* Using a free monad, define a monad for a String-keyed, String-valued key-value store.
The store must support two commands, assuming the store monad is called Store:

type Key = String
type Value = String

getValue :: Key -> Store (Maybe Value)
putValue :: Key -> Value -> Store ()

* For an interpreter, implement a natural transformation from StoreF to IO,
using IORef [(String, String)] as a backing store.
Feel free to use Map or HashMap if you want to.

* Notice that BinTree defined above is not a Monad.
However, if leaves and branches could have different types, it would be.
Now consider the following type:

data BinTree l a
  = Leaf a | Branch l (BinTree l a) (BinTree l a)
  deriving (Show, Functor)

It is a monad.

* Implement a conventional Monad instance,
  and implement convert :: FreeBinTree l a -> BinTree l a
  using foldFree
  and a natural transformation BinTreeF l a -> BinTree l a.
-}
