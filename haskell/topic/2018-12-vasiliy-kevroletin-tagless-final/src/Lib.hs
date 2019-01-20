{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib where

import qualified Prelude
import           Protolude

-- https://serokell.io/blog/2018/12/07/tagless-final

{-

MTL-style programming
- ignore concrete monad transformers
- concentrate on type classes, leaving
  - functions declared using type constraints instead of concrete types:

  getUser :: (MonadReader r m, Has DatabaseConfig r, MonadIO m) => Name -> m User

  - instantiation of polymorphic function to a concrete type (aka interpreter) happens
    somewhere "in the end":

  liftIO $ runReader (getUser (Name "Pedro")) env

that is tagless final style:
- write code using overloaded functions
- run code using a suitable implementations (aka interpreters)

tagless final : extensibility in two dimensions
- see The Expression Problem Revisited : http://www.kframework.org/images/3/3f/ExpProblem.pdf

extensibility of:

  wimble :: (MonadReader Env m, MonadState State m) => m ()

- 1st dimension of extension: a new interpreter
  - run using implementation of MonadReader and MonadState
- 2nd dimension of extension: new set of operations
  - use wimble in function that uses MonadReader, MonadState and MonadWriter
    - i.e. old and new operations

learning resources show two approaches to using tagless final:
- define operations abstracted over a monad
  - can then use do notation
- define an Abstract Syntax Tree using overloaded functions
  - can pretty print, inspect, optimize

the two approaches are the same

Application Monad : organize effectful application code using monads using tagless final

example : read/delete user from a DB
-}

newtype Name = Name Text
data User = User { name :: Name, age :: Int }

-- application monad
-- Tagless final approach, define set of overloaded functions, implement later.
-- (or use generic functions from MTL instead)
class Monad m => MonadDatabase m where
  getUser    :: Name -> m User
  deleteUser :: User -> m ()

-- abstract test function can be executed with different implementations of MonadDatabase
test0 :: MonadDatabase m => m ()
test0 = do
  user <- getUser (Name "Pedro")
  when (age user < 18)
    (deleteUser user)

-- MonadDatabase instance for test function : built on top of MTL transformers

data DatabaseConfig = DatabaseConfig
  deriving Show

newtype AppM a = AppM
  { unAppM :: ReaderT DatabaseConfig IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader DatabaseConfig)

instance MonadDatabase AppM where
  getUser (Name n) = do
    cfg <- ask
    panic $ "getUser " <> n <> " " <> toS (Prelude.show cfg)
  deleteUser (User (Name n) _) = do
    cfg <- ask
    panic $ "deleteUser " <> n <> " " <> toS (Prelude.show cfg)

runAppM :: AppM a -> DatabaseConfig -> IO a
runAppM app = runReaderT (unAppM app)

-- execute test function using AppM implementation
main1 :: IO ()
main1 = do
  cfg <- panic "main"
  runAppM test0 cfg
{-
tagless final : separated def of operations from impl : extensibility

how to choose operations?
- via new typeclass (e.g., MonadDatabase) with application-specific functions, or,
- via MTL typeclasses and define ops on top

how to write the impl?
- are there practical alternatives to MTL transformers?
- he doesn't know.  For application architecture see:

https://pay.reddit.com/r/haskell/comments/8p6rjv/guidelines_for_effect_handling_in_cardano_sl/e09l4ee/
https://github.com/input-output-hk/cardano-sl/blob/1d79a801936edeb4bde7f41187924bc59c7b9b20/docs/cardano-monads.rst
https://github.com/input-output-hk/cardano-sl/blob/f118c77fcd12205fd2e430c12ff1d1dbe6bb4ea9/docs/monads.md
https://jaspervdj.be/posts/2018-03-08-handle-pattern.html
https://tech-blog.capital-match.com/posts/3-anatomy-of-haskell-web-app.html
http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

tagless final using MTL : does it imply MTL's n^2 problem?
- no : transformers have nothing to do with tagless final
- only talking about type constraints and freedom to switch between implementations

two ways to encode an AST: using INITIAL and FINAL encodings


Initial encoding : represent AST using values of a given algebraic data type
- "initial" inspired by category theory
- from observation that inductive data type can be viewed as an "initial algebra"

TAGGED INITIAL ENCODING example

AST that reuses Haskell lambda functions in Lambda def
- approach called higher-order abstract syntax
-}

data ExprTI
  = IntConstTI Int
  | LambdaTI   (ExprTI -> ExprTI)
  | ApplyTI    ExprTI ExprTI
  | AddTI      ExprTI ExprTI

-- well-formed
-- 10 + 20
t1 :: ExprTI
t1 = IntConstTI 10 `AddTI` IntConstTI 20

-- (\x -> 10 + x) 20
t2 :: ExprTI
t2 = ApplyTI (LambdaTI $ \x -> IntConstTI 10 `AddTI` x) (IntConstTI 20)

-- but allows malformed

-- Trying to call integer constant as a function
e1 :: ExprTI
e1 = ApplyTI (IntConstTI 10) (IntConstTI 10)

-- Trying to add lambda functions
e2 :: ExprTI
e2 = AddTI f f where f = LambdaTI identity

-- represent resulting values and Maybe Result for errors
data Result
  = IntResult    Int
  | LambdaResult (ExprTI -> ExprTI)

evalti :: ExprTI -> Maybe Result
evalti (IntConstTI x) = Just (IntResult x)
evalti (LambdaTI   f) = Just (LambdaResult f)
evalti (ApplyTI f0 arg) = do
  f1  <- evalti f0
  case f1 of
    LambdaResult f -> evalti (f arg)
    _              -> Nothing
evalti (AddTI l0 r0) = do
  l1 <- evalti l0
  r1 <- evalti r0
  case (l1, r1) of
    (IntResult l, IntResult r) -> Just $ IntResult (l + r)
    _                          -> Nothing
{-
technique called TAGGED because sum types in Haskell are tagged sum types
- represented as (tag, payload)
- tag for pattern-matches : e.g., evalti function uses pattern matching

TAGLESS INITIAL ENCODING

use GADT to add info about values into Expr type
- makes malformed expressions unrepresentable
- no longer need a Result data type
- no runtime type checking (like in above evalti)

In http://okmij.org/ftp/tagless-final/JFP.pdf
- refer to versions of above IntResult and LambdaResult as "tags"
- because GADTs-based approach no tags, they call it "tagless initial"

param `a` holds type to which expression should evaluate to
-}

data ExprTGLI a where
  IntConstTGL :: Int                                 -> ExprTGLI Int
  LambdaTGL   :: (ExprTGLI a -> ExprTGLI b)          -> ExprTGLI (ExprTGLI a -> ExprTGLI b)
  ApplyTGL    :: ExprTGLI (ExprTGLI a -> ExprTGLI b) -> ExprTGLI a -> ExprTGLI b
  AddTGL      :: ExprTGLI Int -> ExprTGLI Int        -> ExprTGLI Int

evaltgli :: ExprTGLI a -> a
evaltgli (IntConstTGL x) = x
evaltgli (LambdaTGL f)   = f
evaltgli (ApplyTGL f x)  = evaltgli (evaltgli f x)
evaltgli (AddTGL l r)    = evaltgli l + evaltgli r

{-
FINAL ENCODING

INITIAL comes from category theory.
FINAL does notation

Oleg shows http://okmij.org/ftp/tagless-final/index.html#in-fin
- final and initial typed tagless reps related by bijection
- means equivalent
  - both are "Initial" from category theory point of view

approach called final (in contrast to initial) because
- represent each object term NOT by its abstract syntax
- but by its denotation in a semantic algebra

tagless final : build expressions using overloaded functions instead of data constructors
-}

class LambdaSYM repr where
  intConst :: Int -> repr Int
  lambda   :: (repr a -> repr b) -> repr (a -> b)
  apply    :: repr (a -> b) -> repr a -> repr b

-- interpreter implementation

newtype R a = R { unR :: a }

instance LambdaSYM R where
  intConst   = R
  lambda f   = R $ \x -> unR (f (R x))
  apply f a  = R $ unR f (unR a)

eval :: R a -> a
eval = unR

-- applying interpreter

testSmall :: LambdaSYM repr => repr Int
testSmall = apply (lambda identity) (intConst 10)

main :: IO ()
main = print (eval testSmall) -- 10

{-
Interesting points:

eval instantiates testSmall to concrete type R Int (aka interpreter)

define other interpreters, e.g., pretty printer
- needs to allocate names for free variables and keep track of allocated names
- will pass an environment similar to a Reader monad

extend language with new operations: Add `add` op
- requires new type class and instance for each interpreter
- functions that use `add` need AddSYM constraint
-}

class AddSYM repr where
  add :: repr Int -> repr Int -> repr Int

instance AddSYM R where
  add a b = R $ unR a + unR b

testAdd :: (LambdaSYM repr, AddSYM repr) => repr Int
testAdd = apply (apply (lambda (\y -> lambda (`add` y)))
                       (intConst 10))
                (intConst 20)

{-
sometimes necessary to extend existing interpreters to write new ones

Introspection. Host vs target language

counterintuitive to expect introspection because combinators are functions

but it is possible to introspect/transform structure of Final Tagless ASTs

A pretty-printer and transformer of ASTs are tagless final interpreters
which keep track of some additional information
and propagate it during interpreting from parents to children

Both are extensible in the same ways as other tagless final interpreters

recall first section : now apply tagless final to defining Application monad
- cannot inspect/transform
-}

class Monad m => HasDatabaseConfig m where
  getDatabaseConfig :: m DatabaseConfig

getUser2 :: (HasDatabaseConfig m, MonadIO m) => Name -> m User
getUser2 = panic "getUser2"

testX :: (HasDatabaseConfig m, MonadIO m) => m Text
testX = do
  user <- getUser2 (Name "Pedro")
  if age user > 3 then pure "Fuzz"
                  else pure "Buzz"
{-
getDatabaseConfig overloaded but other logic expressed with functions that are not overloaded
- cannot statically inspect the resulting monadic value

important point: to do introspection/transformation of ASTs
- must keep track of what’s overloaded and what’s not
- power of tagless final depends on how far you want to go with overloading.

Relation to Free monads

tagless final
- is faster
- extensible
- requires less boilerplate

free monads
- statically introspect : not completely true
  - can execute actions one by one
  - helps to combine monadic values by interleaving actions
    - achieve a similar thing by interpreting into continuation with tagless final
- see http://reasonablypolymorphic.com/blog/prospecting-free-monads/
  - shows difficulties of free monad introspection
- see https://markkarpov.com/post/free-monad-considered-harmful.html
  - shows difficulties associated with free monads : suggests using tagless final

https://www.youtube.com/watch?v=_XoI65Rxmss&t=5s : overview of free monad performance challenges

https://www.reddit.com/r/haskell/comments/7q4sku/are_people_using_freer_monads_or_still_mostly/dsmlnh7 : Kmett's take

Summary of free monads

simple impls causes O(n^2) asymptotic for left-associated monadic binds
- adds one element to a “list” via [1, 2, 3] ++ [4].

using continuations (like DList package) gives O(n) binds
- but makes some ops slow (e.g., combining two sequences of commands by interleaving them)

using something like Seq data structure leads to a good asymptotic behaviour for all ops
- but also gives significant constant overhead

Performance

sometimes using overloaded functions causes GHC
to generate code which uses dictionaries to dispatch calls
- module boundaries might prevent GHC from avoiding dictionaries

To help compiler, read the “Specializing” section of
  https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/#specializing
then use INLINEABLE

  getUser :: (MonadReader r m, Has DatabaseConfig r, MonadIO m) => Name -> m User
  ...
  {-# INLINEABLE  getUser #-}

Limitations

Haskell lacks first-class polymorphism (aka impredicative polymorphism)
- means can’t specialize an existing data type to hold a polymorphic value like this:

  Maybe (LambdaSym repr => repr Int)

follows that can’t interpret such polymorphic value twice

That is an issue when, e.g.,
- parse text file
- get tagless final AST
- want to interpret it twice: eval and pretty print

limited workaround: define newtype wrapper around polymorphic value
- wrapper specifies concrete type constraints
  and hence kills one extensibility dimension of tagless final.

Oleg workaround: a "duplicating" interpreter

to change impls (aka interpreters) at runtime or change only a part of  existing behaviour
- e.g., change data source, leave other app-specific logic unchange
- tagless final supports by implementing an interpreter configurable in runtime
  which uses some sort of a method dictionary
  - see handle pattern : https://jaspervdj.be/posts/2018-03-08-handle-pattern.html


Literature

Oleg Kiselyov’s Finall Tagless course
http://okmij.org/ftp/tagless-final/index.html

Reducing boilerplate in finally tagless style
https://ro-che.info/articles/2016-02-03-finally-tagless-boilerplate

Transforming transformers
https://www.parsonsmatt.org/2018/04/10/transforming_transformers.html

Static Analysis of Free Monads
http://reasonablypolymorphic.com/blog/prospecting-free-monads/

Haskell 2014: Reflection without Remorse
https://www.youtube.com/watch?v=_XoI65Rxmss&t=5s

LambdaConf 2015 - Finally Tagless DSLs and MTL Joseph Abrahamson
https://www.youtube.com/watch?v=JxC1ExlLjgw

Reddit: are people using freer monads or still mostly
https://www.reddit.com/r/haskell/comments/7q4sku/are_people_using_freer_monads_or_still_mostly/dsmlnh7

Free monad or tagless final? How not to commit to a monad too early - Adam Warski
https://www.youtube.com/watch?v=IhVdU4Xiz2U

Free monad considered harmful
https://markkarpov.com/post/free-monad-considered-harmful.html

No More Transformers: High-Performance Effects in Scalaz 8
http://degoes.net/articles/effects-without-transformers

Revisiting Tagless Final Interpreters
https://gist.github.com/OlivierBlanvillain/48bb5c66dbb0557da50465809564ee80

Initial and Final Encodings
https://peddie.github.io/encodings/encodings-text.html

GHC optimization and fusion
https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/#specializing

Combining Deep and Shallow Embedding for EDSL
http://www.cse.chalmers.se/~josefs/publications/TFP12.pdf
-}

