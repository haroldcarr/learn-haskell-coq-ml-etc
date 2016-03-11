{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module FM_JD_modern_fp where

import           Control.Monad.Free (Free, liftF)

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
    if (httpOK r) then log ("Successfully saved file " ++ show p)
    else let msg = "Failed to save file " ++ show p
    in log msg *> throwException (error msg)

Define a composable algebra:
-}

type Path   = String
type Bytes  = String
type List a = [a]
data Unit   = Unit

data CloudFilesF a
    = SaveFile  Path Bytes a
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

API is REST API
- could express semantics of CloudFilesF in terms of another REST DSL:
-}

data HttpF a
    = GET    Path        (Bytes -> a)
    | PUT    Path  Bytes (Bytes -> a)
    | POST   Path  Bytes (Bytes -> a)
    | DELETE Path        (Bytes -> a)
    deriving Functor

-- Now can "interpret" (aka map or transform) cloud API semantics into algebra of RESTful APIs.

cloudFilesI :: forall a. CloudFilesF a -> Free HttpF a
cloudFilesI  = undefined

{-
App can use high-level, domain-focused algebra CloudFilesF
- will be dynamically interpreted into the low-level, protocol-focused algebra HttpF

Now, rather than tangle logging into domain or protocol logic, define an algebra:
-}

data Level = Debug

data LogF a
    = Log Level String a
    deriving Functor

type Coproduct = Either
toLeft  :: a -> Coproduct a b
toLeft   = Left
toRight :: b -> Coproduct a b
toRight  = Right

-- another interpreter
logCloudFilesI :: forall a. CloudFilesF a -> Free LogF Unit
logCloudFilesI (SaveFile p _ _) = liftF $ Log Debug ("Saving file to "   ++ show p) Unit
logCloudFilesI (ListFiles p _)  = liftF $ Log Debug ("Listing files at " ++ show p) Unit

-- Interpreters compose
-- loggingCloudFilesI :: forall a. CloudFilesF a -> Free (Coproduct (LogF String) (HttpF String)) a
-- loggingCloudFilesI op = toLeft (logCloudFilesI op) *> toRight (cloudFilesI op)

