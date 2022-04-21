{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE GADTs          #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Use <=<" #-}
{-# LANGUAGE KindSignatures #-}

module Lib where

import           Control.Applicative ()
import           Data.Kind

{-
https://okmij.org/ftp/Computation/free-monad.html
Free and Freer Monads: Putting Monads Back into Closet
November 6, 2015
oleg-at-okmij.org

------------------------------------------------------------------------------
Introduction

Writing Monad/Applicative/Functor instances and ensuring monad laws
hold are time consuming and avoidable boilerplate.

Better to think what an effect does rather than on how the plumbing works.

Free/Freer monads obviate boilerplate.
Instead, write definitional interpreters for effects.


Explanations of Free monad  draw insights from monoids, universal algebra and category theory.

Where does Freer monad fit in?
Does it let us think clearer about effects?
Is it also free?
How come it has not been anticipated in all those categorical explanations?

This article shows concrete examples rather than abstract algebra.

References
- https://okmij.org/ftp/Computation/FreeState.hs
  - complete code for this article
- https://okmij.org/ftp/Haskell/extensible/
  - Freer Monads and Extensible Effects

------------------------------------------------------------------------------
Classical monads

running example is the State monad
-}

-- The operations get and put, with their implied laws,
-- and the interpreter runState are the essential parts;
-- they are what makes State a mutable-state computation.

-- monad operations
get :: State s s
get  = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

-- interpreter
runState :: State s a -> s -> (a,s)
runState  = unState

-- To use these operations need to write the following instances.

newtype State s a = State { unState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State m) = State $ \s -> let (v,s') = m s in (f v,s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)
  State f <*> State x =
    State $ \s -> let (vf,s1) = f s
                      (vx,s2) = x s1
                   in (vf vx, s2)

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  State m >>= k = State $ \s -> let (v,s') = m s in unState (k v) s'

{-
A lot of boilerplate.

Instead of the above, the most general monad, functor and applicative instances
can be defined once and for all to obviate needing to write instances and their laws again.

-- use/test
-}

ts1 :: State Int Int
ts1 = do
  put 10
  get

ts1r :: Bool
ts1r = ((10,10) ==) $ runState ts1 0


ts2 :: State Int Int
ts2 = do
  put 10
  x <- get
  put 20
  y <- get
  pure (x + y)

ts2r :: Bool
ts2r = ((30,20) ==) $ runState ts2 0

{-
------------------------------------------------------------------------------
Free Monad

Free monad gets rid of boilerplate.
"Free" refers to the category theory construction of the left adjoint of a forgetful operation.

'State s' is a functor, an applicative and a monad.

Ignore the Applicative and Monad instances for 'State s'.
Turns out, nothing is lost: can still use 'State s' in programs, via Free monad construction:
-}

-- 'f' is a functor, and say 'm' is 'Free f'
data Free (f :: Type -> Type) (a :: Type) where
 -- Pure   ::           a  -> m      a -- looks like pure
    Pure   ::           a  -> Free f a
 -- Impure :: f (m      a) -> m      a -- looks like join
    Impure :: f (Free f a) -> Free f a

-- eta turns any functor f into the monad Free f.
-- type constructor f must be a functor
-- - given 'f a' value, can transform any value of type a to a value of type b.
--   i.e., f should support the map operation.
--   i.e., f has to be a member of the type class Functor.
eta :: Functor f => f a -> Free f a
eta  = Impure . fmap Pure

-- Free f is a monad and a functor and an applicative:

instance Functor f => Functor (Free f) where
  fmap f (Pure   x) = Pure   $            f  x
  fmap f (Impure m) = Impure $ fmap (fmap f) m

instance Functor f => Applicative (Free f) where
  pure           = Pure
  Pure   f <*> m =          fmap  f   m
  Impure f <*> m = Impure $ fmap (<*> m) f

instance Functor f => Monad (Free f) where
  return         = Pure
  Pure   a >>= k =                   k  a
  Impure m >>= k = Impure (fmap (>>= k) m)

{-
Free f has all these properties for any functor f.

Once above instances written then no need to write any more monad instances,
for state or any other effect.

Because the free monad satisfies all -- and only all -- monad, applicative and functor laws,
do not have to bother proving the monad laws either.

Back to running example: forget that 'State s' is a monad.
Now make it a monad again, without writing any monad and applicative instances:
-}

type FState s = Free (State s)

getF :: FState s s
getF  = eta get

putF :: s -> FState s ()
putF  = eta . put

runFState :: FState s a -> s -> (a,s)
runFState (Pure   x) s = (x, s)
runFState (Impure m) s = let (m', s') = unState m s in runFState m' s'

-- tests

tsF1 :: FState Int Int
tsF1 = do
  putF 10
  getF

tsF1r :: Bool
tsF1r = ((10,10) ==) $ runFState tsF1 0

tsF2 :: FState Int Int
tsF2 = do
  putF 10
  x <- getF
  putF 20
  y <- getF
  return (x + y)

tsF2r :: Bool
tsF2r = ((30,20) ==) $ runFState tsF2 0

{-
to use the state effect
- define State s
- define its Functor instance
- write get, put, and runFState

The important parts, not boilerplate instances nor laws.

Even the Functor (State s) instance is unnecessary (shown below).

'runFState' is similar to the bind (>>=) operation of the original 'State s' monad.

TODO : write
  join :: State s (State s a) -> State s a
and find even more similarity

It was necessary to implement/execute something like the monadic bind after all.

Free f does not magically combine one effectful computation f a with another a -> f b

It cannot know how since f is arbitrary.

It accumulates all these binding points, for us to deal with, en masse, at the end.
i.e., Free f merely shifts the work from monad bind (>>=) to runFState.

Free f is not useless
- it automatically applies the unit (that is, return-bind) law and
  associates the bind points in linear order,
  so runFState has an easier time.
- takes care of trivialities -- which is what we wanted.

More importantly, Free f helps shift all the work to the interpreter,
so makes defining effects simpler, and simpler to reason about.

Free f enables writing definitional interpreters for effects.
The free monad enables combining monads, solving the problem that
besieged monads since their inception.

------------------------------------------------------------------------------
Freer Monad

We forgot the Monad and Applicative instances of 'State s',
and got them back, through the free monad.

Free monad gave us the monad and applicative instances for free.

Can also forget that 'State s' is a functor.

With fmap no longer available, the free monad construction does not work.

To obtain fmap: if a structure 'g a' does not support 'fmap', get it via
-}

-- Lan g a keeps the arguments of fmap without actually doing any mapping.
-- (An instance of the left Kan extension.)
data Lan g a where
  Lan :: g x -> (x -> a) -> Lan g a

-- The needed Functor instance.
-- Therefore, Free (Lan g) is a monad.
instance Functor (Lan g) where
  fmap f (Lan gx h) = Lan gx (f . h)

lan :: g a -> Lan g a
lan ga = Lan ga id

{-
Can now turn 'g a' without any special properties into a monad, the freer monad.
The following Freer monad is not (yet) extensible and nore optimal.
It is used for the sake of explanation.
See the Haskell Symposium 2015 paper for the extensible and optimal version.

The following desugared Free (Lan g) sufficient for this article:
-}

data FFree g a where
  FPure   ::                      a  -> FFree g a
  FImpure :: g x -> (x -> FFree g a) -> FFree g a

{-
In contrast to Free f, the type constructor g :: * -> * does not have to be a functor.
It can be anything at all.
But FFree g is a functor, an applicative and a monad.
FFree g is the monad by its very construction.
-}

instance Functor (FFree g) where
  fmap f (FPure   x)   = FPure          (f x)
  fmap f (FImpure u q) = FImpure u (fmap f . q)

instance Applicative (FFree g) where
  pure = FPure
  FPure   f   <*> x = fmap f x
  FImpure u q <*> x = FImpure u ((<*> x) . q)

instance Monad (FFree g) where
  return = FPure
  FPure x      >>= k = k x
  FImpure u k' >>= k = FImpure u (k' >>> k)

-- composition of side-effectful functions (Kleisli composition)
(>>>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >>> g = (>>= g) . f

{-
Monads laws hold shown categorically, from the fact that FFree g = Free (Lan f).
Category theory gives general theorems, which apply in very many specific circumstances.
Any structure g a can be turned into a monad, no matter what it is:
-}

etaF :: g a -> FFree g a
etaF fa = FImpure fa FPure

{-
Compared to eta of the free monad,
etaF has no Functor constraint and uses no fmap,
which is the sign of improved efficiency.

To make State s a monad, no longer have to write any instances at all, not even Functor:
-}

type FFState s = FFree (State s)

getFF :: FFState s s
getFF  = etaF get

putFF :: s -> FFState s ()
putFF  = etaF . put

runFFState :: FFState s a -> s -> (a,s)
runFFState (FPure   x)   s = (x, s)
runFFState (FImpure m q) s = let (x, s') = unState m s in runFFState (q x) s'

{-
Above is everything needed to write programs with the state effect.
Shifting of the work from monad bind to the interpreter runFFState has become clearer.
A typical computation, without syntax sugar, looks like

     ((FImpure (put 10) Pure) >>= \_ -> getF) >>= \x -> return x

of the general shape

     (((FImpure eff k1) >>= k2) >>= ...) >>= kn

where
- eff identifies the effect to perform
- k1,...,kn are follow-up computations

The bind operation of FFree g turns the above into
     FImpure eff (k1 >>> k2 >>> ... >>> kn)
which is handed over to runFFState to deal with eff and then to follow up.

Thus FFree g collects all effect follow-ups k1,...,kn into a sort of a heterogeneous list,
with (>>>) playing the role of the list constructor.

(One is reminded of the connection between the free monad and the free monoid, which is a sequence.)

Thinking of k1 >>> k2 >>> ... >>> kn as a list is not merely amusing.
The Haskell 2015 paper has significantly improved the performance of the freer monad
by treating k1 >>> k2 >>> ... >>> kn truly as a heterogeneous data structure, an efficient queue.

Looking back to our State s example,
we see that we may forget not only return and bind but also the fmap operation,
and still recover the state monad as FFree (State s).

FFree g is also free (in the category theory/universal algebra sense),
and can recover the forgotten properties such as fmap or bind.

Compared to Free f,
the freer monad affords to forget more,
and hence has less boilerplate.

As a consequence, it is more efficient.

No longer have to write any basic monad and functor operations and instances in the first place.
The freer monad gives them for free.

-}

tsFF1 :: FFState Int Int
tsFF1 = do
  putFF 10
  getFF

tsFF1r :: Bool
tsFF1r = ((10,10) ==) $ runFFState tsFF1 0


tsFF2 :: FFState Int Int
tsFF2 = do
  putFF 10
  x <- getFF
  putFF 20
  y <- getFF
  return (x+y)

tsFF2r :: Bool
tsFF2r = ((30,20) ==) $ runFFState tsFF2 0

{-
------------------------------------------------------------------------------
Definitional interpreters for effects

FFree g is a monad for any g whatsoever.

This section puts this property of the freer monad to good use.

The implementation of the state effect has been spread around
- the operations put and get
- and the interpreter runFFState.

We now pull all the implementation into one place, into the interpreter:
-}

-- Names the state effect operations and defines their types
-- Could call it the effect signature.
-- It is not a functor and does not have special properties.
data StateEff s x where
  Get ::      StateEff s s
  Put :: s -> StateEff s ()

-- This is a monad.
type EffState s = FFree (StateEff s)


{-
The primitives no longer access or update the global state.
They do not do anything at all.
They request what they want to get done.
-}
getEff :: EffState s s
getEff = etaF Get

putEff :: s -> EffState s ()
putEff = etaF . Put

-- Fulfillment of these requests is the job of the interpreter:
runEffState :: EffState s a -> s -> (a, s)
runEffState (FPure x) s     = (x, s)
runEffState (FImpure m q) s =
  let (x, s') = unEffState m s in runEffState (q x) s'

unEffState :: StateEff s a -> (s -> (a, s))
unEffState  Get s    = (s, s)
unEffState (Put s) _ = ((), s)

-- Tests

tsEff1 :: EffState Int Int
tsEff1 = do
  putEff 10
  getEff

tsEff1r :: Bool
tsEff1r = ((10,10) ==) $ runEffState tsEff1 0


tsEff2 :: EffState Int Int
tsEff2 = do
  putEff 10
  x <- getEff
  putEff 20
  y <- getEff
  return (x + y)

tsEff2r :: Bool
tsEff2r = ((30,20) ==) $ runEffState tsEff2 0

{-
The entire implementation of the effect is in the interpreter, a DEFINITIONAL INTERPRETER.
It gives the meaning to state and the operations to access and update it.

With the implementation in one place, it is easy to see and reason and change what goes on.

It is also easy to swap the implementation: to run same program with different interpreter,
e.g., one that implements state via a global reference cell
      or an exchange of messages with a dedicated process.

See the Haskell Symposium 2013 and 2015 papers and the accompanying code
for ease of defining effect interactions.

References

Heinrich Apfelmus: The Operational Monad Tutorial

The definitional effect interpreter above is the Operational Monad by Heinrich Apfelmus.
Our explanation adds the derivation via the free monad, and hence the easy proof of monad laws.
The main difference from Operational is the extensibility and hence the ability to combine monads,
which is presented in the Haskell 2015 paper.

------------------------------------------------------------------------------
Performance

The free/freer monads above have poor performance.

Speeding up the performance of free monad : subject of Haskell 2015 paper

Jaskelioff&Rivas, ICFP2015 : related but not extensible approach.

The optimized freer monad delivers good performance for programs with multiple effects,
surpassing monad transformers in speed and memory economy.

Even for a single effect, notably State (which GHC optimizes well), the free
monad can be on par with the native state monad, see (Wu&Schrijvers, MPC 2015).

------------------------------------------------------------------------------
Conclusions

Monad instances/laws is boring plumbing.

Concentrate on effects.

The Freer monad frees us from monads.
-}
