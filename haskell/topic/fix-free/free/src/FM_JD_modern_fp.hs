{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitForAll       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UndecidableInstances #-}

module FM_JD_modern_fp where

import           Control.Arrow      ((<<<))
import           Control.Monad.Free (Free (Free), liftF)

------------------------------------------------------------------------------
-- http://degoes.net/articles/modern-fp

{-
Free monad :
- a way to put a sequential computation into a data structure
- then inspect/interpret data structure later

Called "free" because monad obtained "automatically" (for free) for any higher-kinded * -> * type
- similar to how a list is a free monoid for any kind *

One way to view Free:

  A description of a program
that will halt, run forever, or
    produce a value `a`
            |
           / \
          /   \
         /     \
        Free f a
             ^ ^
             |  \
             |   \ Value produced by program
        Operational
          Algebra

- Free f a : description of a program
-      f   : set of operations the program can be reduced to (aka "algebra")
-        a : value produced by program (unless it halts or runs forever)

Free monads
- model programs
  - not as sequence of machine code (the IO approach)
  - but as a sequence of algebraic operations that describe semantics of program
- can be introspected (one step at a time), interpreted, and transformed


algebras compose
- given algebras f and g : composite algebra Coproduct f g for suitable definition of Coproduct
- interpreters compose - both horizontally and vertically

- if f and g can be interpreted separately into some h
  then coproduct of their algebras can be interpreted into h
- if interpret f into g, and g into h, then can interpret f into h (involves a higher-order bind)

Free monads       : embody essence of sequential computation
Free applicatives : embody essence of parallel   computation

Model programs with both sequential and parallel computation via operations having well-defined semantics.

*Interpretation of Free Structures*

Interpretation :  natural transformation between functors:

type Natural f g = forall a. f a -> g a

interpret Free f a to Free g a with a natural transformation between f and g (among other ways)
- requires g be at least as capable as f (IO is infinitely powerful so you can interpret anything to IO)
- if g is a Monad, then can collapse Free g a into g a.

In most examples of free monads, algebras are interpreted directly into effectful monads like IO.
- technically superior to defining a whole program in IO
  - gain ability to isolate and reason about effects, modularize interpretation, ...
- but more powerful techniques shown below

To fix this bad program:

saveFile :: Path -> Bytes -> IO Unit
saveFile p f = do
    log ("Saving file" ++ show (name p) ++ " to " ++ show (parentDir p))
    r <- httpPost ("cloudfiles.fooservice.com/" ++ (show p)) f
    if httpOK r
        then log ("Successfully saved file " ++ show p)
        else let msg = "Failed to save file " ++ show p
             in log msg *> throwException (error msg)

Define a composable algebra:
-}

type Path   = String
type Bytes  = String
type List a = [a]
data Unit   = Unit

data CloudFilesF a = SaveFile  Path Bytes a
                   | ListFiles Path (List Path -> a)
                   deriving Functor

-- DSL using alebra for interacting with the API:

type CloudFilesAPI a = Free CloudFilesF a

saveFile :: Path -> Bytes -> CloudFilesAPI Unit
saveFile path bytes = liftF (SaveFile path bytes Unit)

listFiles :: Path -> CloudFilesAPI (List Path)
listFiles path = liftF (ListFiles path id)

{-
DSL defines semantics of API (could also define laws for operations)
- does not describe how to provide the service

REST API
- could express semantics of CloudFilesF in terms of another REST DSL:
-}

data HttpF a
    = GET    Path        (Bytes -> a)
    | PUT    Path  Bytes (Bytes -> a)
    | POST   Path  Bytes (Bytes -> a)
    | DELETE Path        (Bytes -> a)
    deriving Functor

-- Now can "interpret" (aka map or transform) cloud API semantics into algebra of RESTful APIs via `cloudFilesI`

-- BEGIN : THIS SECTION IS WRONG
bytesToHttp :: Bytes -> Free HttpF a
bytesToHttp  = undefined

cloudFilesI :: forall a. Free CloudFilesF a -> Free HttpF a
cloudFilesI  = cf
  where
    cf (Free (SaveFile path bytes cont)) = Free (POST path bytes bytesToHttp)
    cf (Free (ListFiles path lpToA))     = Free (GET  path       bytesToHttp)
    cf _                                 = error "BAD"

ex :: Free CloudFilesF Unit
ex = do
    (f:fs) <- listFiles "/User/carr"
    saveFile f "content"

httpI :: forall a. Free HttpF a -> [String]
httpI  = h []
  where
    h stack  (Free (POST path bytes b2h)) = [bytes]
    h (f:fs) (Free (GET  path cont))      = [path]
    h xs     _                            = xs
-- END : THIS SECTION IS WRONG
{-
App can use high-level, domain-focused algebra CloudFilesF
- will be dynamically interpreted into the low-level, protocol-focused algebra HttpF

Now, rather than tangle logging into domain or protocol logic, define an algebra:
-}

data Level = Debug

data LogF a
    = Log Level String a
    deriving Functor

-- another interpreter
logCloudFilesI                 :: forall a. CloudFilesF a -> Free LogF Unit
logCloudFilesI (SaveFile p _ _) = liftF $ Log Debug ("Saving file to "   ++ show p) Unit
logCloudFilesI (ListFiles p _)  = liftF $ Log Debug ("Listing files at " ++ show p) Unit

-- TODO : depends on Coproduct at end of file
-- compose interpreters
-- toLeft  = liftF . left
-- toRight = liftF . right
-- loggingCloudFilesI :: forall a. CloudFilesF a -> Free (Coproduct LogF HttpF) a
-- loggingCloudFilesI op = toLeft (logCloudFilesI op) *> toRight (cloudFilesI op)

-- to "really" run it
-- executor :: forall a. Coproduct LogF HttpF a -> IO a

{-
BENEFITS

- cloud files layer focused on domain, not implementation

- cloud files service -> REST APIs : centralized/isolated from rest of app
  - for testing, cloud files service -> high-level mock service that uses semantics directly

- logging code centralized/isolated and untangled from business logic and REST APIs
  - logging can be structured/uniform rather than random/scattered across program
  - need not log to a file: supply different interpreters to log to remote API

- modular, composable
  - pick interpreter for program (even based on runtime values) by composing interpreters
-}

------------------------------------------------------------------------------

{-
1. Orthogonal, Composable Algebras

real-world: free algebras are not orthogonal: have overlapping operations
-}

data FileF1 a
    = MakeDir     Path               a
    | Delete1     Path               a
    | Copy        Path Path          a
    | Rename      Path Path          a
    | Move        Path Path          a
    | Ls1         Path (List Path -> a)
    | CreateFile1 Path Bytes         a
    | ReadFile1   Path (Bytes ->     a)
    | AppendFile1 Path Bytes         a
{-
Above ops not primitive (i.e., some can be composed from others).

More orthogonal:
-}

data FileF2 a
    = CreateFile Path               a
    | CreateDir  Path               a
    | AppendFile Path Bytes         a
    | Duplicate  Path Path          a
    | Delete     Path               a
    | Ls         Path (List Path -> a)
    | ReadFile   Path (Bytes ->     a)
    deriving Functor

rename :: Path -> Path -> Free FileF2 Unit
rename from to =
  liftF (Duplicate from to Unit) *>
  liftF (Delete from Unit)

{-
But better to have rename be primitive do avoid renaming 10 GB file  copy/delete of it.

Or, use optimizing interpreter: detect patterns and substitute with faster alternatives.

LIMITATION: free monads depend on runtime values

FREE APPLICATIVES:
- constrained enough to enable this type of optimization statically.

Or extend free structure with atomic sequencing operation that ignores value of left-hand side (e.g. *> or >>).

Both enable interpreters to see far enough ahead to do optimizations.

2. Generalizing Interpreters

Goal: make code less brittle to changes and maximize places they can be used.
-}

type Interpreter f g = forall a. f a -> Free g a

{-
Translates a single op f a into program of ops in g.

But instead of interpreting to a concrete g
- interpret to any g that supports required capabilities
  - via: type-level Haskell, Scala implicits, type classes
- most straightforward : lenses : a Prism

Prism : construct and (when possible) deconstruct sum types.

higher-order prism that can work with functors:
-}

type PrismP a b = (a,b) -- TO GET IT TO COMPILE

type Inject f g = forall a. PrismP (f a) (g a)

{-
Enables constructing an f a from g a.

generalize interpreter:
-}

type Interpreter2 f g' = forall a g. Inject g g' -> f a -> Free g a

{-

says: If prove that any g is at least as powerful as g' (by supplying a Inject),
      then the interpreter (which requires g') can interpret into g.

interpreter is polymorphic in its target algebra
- more cumbersome to define
- more robust to code changes and can be used in more places

3. Generalizing DSLs

This cloud DSL above requires target algebra be CloudFilesF.

Can generalize in same way generalized the interpreters
- requiring target algebra be at least as powerful as CloudFilesF.

(uses PureScript’s first-class records to avoid boilerplate):

data CloudFilesDSL g where
    SaveFile2  :: Path -> Bytes -> Free g Unit
    ListFiles2 :: Path -> Free g (List Path)

cloudFilesDSL :: forall g. Inject g CloudFilesF -> CloudFilesDSL g
cloudFilesDSL p = {
  saveFile  : \path bytes -> liftF $ review p (SaveFile path bytes Unit),
  listFiles : \path       -> liftF $ review p (ListFiles path id) }

Now can use DSL in any target algebra that includes CloudFilesF

Summary

Avoid "purely functional" that sequentially executes "IO" code.  (Task in Scala, Eff in PureScript).

Avoid anti-patterns :
- mixing abstraction levels
- distributing knowledge that should be centralized
- tangling concerns

Describe effects by
- reducing them to orthogonal, composable operations (description of program)
  - constrained algebras : enables reasoning/manipulation
- describing computation with these operations using a computational context like Free. (introspect, transform, interpret)

Do transformation to IO at the edge.
-}

------------------------------------------------------------------------------

-- from https://github.com/purescript/purescript-coproducts/blob/master/src/Data/Functor/Coproduct.purs

-- | `Coproduct f g` is the coproduct of two functors `f` and `g`
newtype Coproduct f g a = Coproduct (Either (f a) (g a))

-- | Unwrap a coproduct
runCoproduct :: forall f g a. Coproduct f g a -> Either (f a) (g a)
runCoproduct (Coproduct x) = x

-- | Left injection
left :: forall f g a. f a -> Coproduct f g a
left = Coproduct <<< Left

-- | Right injection
right :: forall f g a. g a -> Coproduct f g a
right = Coproduct <<< Right

-- | Eliminate a coproduct by providing eliminators for the left and
-- | right components
coproduct :: forall f g a b. (f a -> b) -> (g a -> b) -> Coproduct f g a -> b
coproduct f g = either f g <<< runCoproduct

-- TODO
-- instance functorCoproduct (Functor f, Functor g) => Functor (Coproduct f g) where
--     fmap f = Coproduct <<< coproduct (Left <<< (<$>) f) (Right <<< (<$>) f)
