> {-# LANGUAGE FlexibleContexts      #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE GADTs                 #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables   #-}
> {-# LANGUAGE TypeFamilies          #-}
>
> module Lib where
>
> import Data.Function (fix)
> import Data.IntMap   as IM
> import Data.Map      as M
> import Prelude       as P

------------------------------------------------------------------------------
2. Associated types: indexing types by types

ways to express relations on types
- multiparameter type classes express many-to-many relations
- type constructors express functional relations
  - e.g., functional relation expressed by type constructor [] :: * -> *
  - maps type `a` to type `[a]`
  - *type constructor maps to result type without inspecting input args*

Type functions
- topic of this paper
- also establish functional relations between types
- *type function may perform case analysis on its argument types*

Example

relation between
- monad that supports mutable state
- type constructor for reference cells

two examples:

  newIORef   :: a -> IO (IORef a)
  readIORef  :: IORef a -> IO a
  writeIORef :: IORef a -> a -> IO ()

  newSTRef   :: a -> ST s (STRef s a)
  readSTRef  :: STRef s a -> ST s a
  writeSTRef :: STRef s a -> a -> ST s ()

uniform interface using multiparameter type class and functional dependencies:

  class Mutation m r | m -> r where
    newRef   :: a -> m (r a)
    readRef  :: r a-> m a
    writeRef :: r a -> a -> m ()

  instance Mutation IO IORef where
    newRef = newIORef
    ...etc...
  instance Mutation (ST s) (STRef s) where
    newRef = newSTRef
    ...etc...

------------------------------------------------------------------------------
2.1 Declaring an associated type

class Mutation does not really have two type parameters
- it has one, associated with another type that is functionally dependent

Can say this directly with type families:

  class Mutation m where
    type Ref m :: * -> *             -- `Ref` is type function (with a specified kind)
    newRef     :: a -> m (Ref m a)   -- `newRef`, etc., are value function
    readRef    :: Ref m a -> m a
    writeRef   :: Ref m a -> a -> m ()

  instance Mutation IO where
     type Ref IO     = IORef         -- clause for type function, Ref, given IO, returns IORef
     newRef          = newIORef      -- `newIORef`, etc., are witnesses for each value function
     readRef         = readIORef
     writeRef        = writeIORef

  instance Mutation (ST s) where
     type Ref (ST s) = STRef s       -- clause for type function, Ref, given ST s, returns STRef s
  -- newRef         :: Mutation m => a -> m (Ref m a)
     newRef          = newSTRef
  -- readRef        :: Mutation m => Ref m a -> m a
     readRef         = readSTRef
     writeRef        = writeSTRef

`Ref` is a type family (aka associated type of the class Mutation)
- acts as function at type level
- also call `Ref` a type function

applying type function uses same syntax as applying a type constructor
- Ref m a : means to apply type function Ref to m, then apply resulting type constructor to a

------------------------------------------------------------------------------
2.2 Arithmetic

use-case : make type coercions implicit
- write `add a b` even if one is Int and other is Double
  - without explicit `fromIntegral`

> -- | a b are types given to add
> class Add a b where
>   type SumTy a b                     -- associated type : 2 arg type fun maps arg types to result type
>   add :: a -> b -> SumTy a b         -- given values of type `a` and `b` return a value of type returned from `SymTy a b`

> instance Add Integer Double where
>   type SumTy Integer Double = Double
>   add x y = fromIntegral x + y

> instance Add Double Integer where
>   type SumTy Double Integer = Double
>   add x y = x + fromIntegral y

> instance (Num a) => Add a a where
>   type SumTy a a = a                 -- if both args are of same type the return is of same type
>   add x y = x + y

> e1 = add (1   :: Integer) (2.3 :: Double)
> e2 = add (1.2 :: Double)  (3   :: Integer)
> e3 = add (1.2 :: Double)  (3.4 :: Double)

- add scalar to vector represented by list
  - without explicitly to coerce the scalar to the vector type

> -- | a is type ov value to be cons on to list of type b
> class Cons a b where
>   type ResTy a b
>   cons :: a -> [b] -> [ResTy a b]

> instance Cons Integer Double where
>   type ResTy Integer Double = Double
>   cons x ys = fromIntegral x : ys

> e4 = cons (1 :: Integer) ([]::[Double])

------------------------------------------------------------------------------
2.3 Graphs

another kind of associated type
- introduced with `data` keyword

> class Graph g where
>   type Vertex g                                        -- associated type (declared using `type`)
>   data Edge g                                          -- associated type (declared using `data`)
>   src, tgt :: Edge g -> Vertex g
>   outEdges :: g -> Vertex g -> [Edge g]

> newtype G1 = G1 [Edge G1]                              -- graph represented by list of edges
> instance Graph G1 where
>   type Vertex G1 = Int                                 -- vertices represented as ints
>   data Edge   G1 = MkEdge1 (Vertex G1) (Vertex G1)     -- edge has end-points
>   src      = undefined
>   tgt      = undefined
>   outEdges = undefined

> newtype G2 = G2 (Map (Vertex G2) [Vertex G2])          -- graph represented by map from vertice to list of neighbours
> instance Graph G2 where
>   type Vertex G2 = String                              -- vertices represented as strings
>   data Edge   G2 = MkEdge2 Int (Vertex G2) (Vertex G2) -- edges has a weight and end-points
>   src      = undefined
>   tgt      = undefined
>   outEdges = undefined

------------------------------------------------------------------------------
2.4 Associated data types

MkEdge1 and MkEdge2 are data constructors

[] type constructor is injective
- maps different arg types to different result types
- e.g., two list types are the same, then their element types must be the same

injectivity does not generally hold for type functions
- e.g., find vertices adjacent to given vertex in given graph

> neighbours g v = P.map tgt (outEdges g v)

by declaring Edge with data, we specify that Edge is injective, that Edge g1 ∼ Edge g2 implies g1 ∼ g2

------------------------------------------------------------------------------
2.5 Type functions are open

Value-level functions are closed : must be defined all in one place. e.g.,
  length :: [a] -> Int
  length []     = 0
  length (x:xs) = 1 + length xs

not legal to put the two equations in different modules

type functions, like type classes, are open
- can be extended with additional instances at any time
- e.g., define a new type Age, and extend SumTy and add to work over Age by adding instance

> newtype Age = MkAge Int
> instance Add Age Int where
>   type SumTy Age Int = Age
>   add (MkAge a) n = MkAge (a+n)

------------------------------------------------------------------------------
2.6 Type functions may be recursive

Just as instance for Show [a] defined in terms of Show a,
a type function may be defined by structural recursion on input type.

e.g., extend Add class

> instance (Add Integer a) => Add Integer [a] where
>   type SumTy Integer [a] = [SumTy Integer a]
>   add x y = P.map (add x) y

so : SumTy Integer [Double] ∼ [SumTy Integer Double] ∼ [Double]

------------------------------------------------------------------------------
3 Optimised container representations

optimisation technique : represent data of different types differently
- e.g., rather than uniformly as character strings
- e.g., use same array container for Bool array and Int array
  - Bool
    - can be stored more compactly
    - negated elementwise faster when its elements are tightly packed as a bit vector

------------------------------------------------------------------------------
3.1 Type-directed memoization

memoise : improve future performance by recording/reusing past behaviour

A memo table augments concrete rep of function without affecting its abstract interface.

common impl
- add lookup from table on fun entry, update table on exit
- use lazy eval to manage lookup/update

type function impl : type of table determined from arg type to memoised function

> -- | constraint `Memo a` means
> -- the behaviour of a function from an argument type a to a result type w
> -- can be represented as a memo table of type Table a w
> -- where Table is a type function that maps a type to a constructor
> class Memo a where
>   data Table a :: * -> *
>   toTable      :: (a -> w)  -> Table a w
>   fromTable    :: Table a w -> (a -> w)

> -- | memoise funs from Bool by storing its two return values as a lazy pair
> -- lazy pair is the memo table
> instance Memo Bool where
>   data Table Bool w       = TBool w w
>   toTable f               = TBool (f True) (f False)
>   fromTable (TBool x y) b = if b then x else y

> f :: Bool -> Integer
> f True  = product [1..1000] -- factorial
> f False = fib 30
>  where
>   fib 0 = 0
>   fib 1 = 1
>   fib n = fib (n-1) + fib (n-2)

> -- memoise
> g :: Bool -> Integer
> g = fromTable (toTable f)

first time g is applied to True/False, computes first/second component of e lazy pair by applying f to True/False
- remembers it for future reuse

> e5 = (f False + f False)
> e6 = (g False + g False) -- should be same timing a e5 the first time, faster subsequently

can memoise funs from any sum type
- e.g., function from Either a b
- store lazy pair of a memo table from a and a memo table from b
- leverage isomorphism between fun type Either a b -> w and product type (a -> w, b -> w)

> instance (Memo a, Memo b) => Memo (Either a b) where
>   data Table (Either a b) w = TSum (Table a w) (Table b w)
>   toTable f = TSum (toTable (f . Left)) (toTable (f . Right))
>   fromTable (TSum t _) (Left  v) = fromTable t v
>   fromTable (TSum _ t) (Right v) = fromTable t v

we can memoise fun from product type
- e.g., (a,b)
- storing a memo table from a whose entries are memo tables from b
- leverage currying isomorphism between fun types (a,b) -> w and a -> b -> w

> instance (Memo a, Memo b) => Memo (a,b) where
>   newtype Table (a,b) w = TProduct (Table a (Table b w))
>   toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
>   fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y

HC: memoize fun from int to int

> -- | memoise funs from Int by storing return values in lazy list
> -- lazy list is the memo table
> instance Memo Int where
>   data Table Int w      = TList [w]
>   toTable f             = TList (P.map f [0 ..])
>   fromTable (TList m) i = m !! i

 https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html

> fib :: (Int -> Integer) -> Int -> Integer
> fib f 0 = 0
> fib f 1 = 1
> fib f n = f (n - 1) + f (n - 2)

> fm     = fix (fromTable . toTable . fib)
> fixfib = fix fib -- not memoized

------------------------------------------------------------------------------
3.2 Memoisation for recursive types

A list is a combination of a sum, a product, and recursion:

> instance (Memo a) => Memo [a] where
>   -- recursive type function
>   -- a list is either empty or not
>   -- so Table [a] w represented by TList'
>   -- 1st component : result f []
>   -- 2nd component : result of f (x:xs)
>   data Table [a] w = TList' w (Table a (Table [a] w))
>   -- (x:xs) belongs to product type
>   -- so corresponding table maps each x to a table that deals with xs
>   -- this combines memoization of functions from sums and from products
>   toTable f = TList' (f [])
>                      (toTable (\x -> toTable (\xs -> f (x:xs))))
>   fromTable (TList' t _)    []  = t
>   fromTable (TList' _ t) (x:xs) = fromTable (fromTable t x) xs

Laziness takes care of recursion in [a]
- memo table contains (f x) for all x
- memo table can be infinite
- toTable [a] uses toTable for a and toTable for [a] recursively
- each value (f x) is evaluated only if the function is ever applied to that particular x
- each subtable is expanded only if the function is applied to a list with that prefix
- laziness works at two distinct levels

> ffl    []  = []
> ffl (x:xs) = fm x : ffl xs
> e7 = fromTable (toTable ffl)

any data type can be memoized

generally
- define Memo instances for sum types, product types, and fixpoint types
- define a Memo instance by writing isomorphism between new type and construction from sum, product and fixpoint types
- can be defined generically using functional dependencies or type functions

------------------------------------------------------------------------------
3.3 Generic finite maps

finite map : partial function keys to values (made total with Maybe), here represented using memo table

Need to distinguish mapping of key to Nothing from the absence of the mapping for key.

> class KKey k where
>   data KMap k :: * -> *
>   kempty  :: KMap k v
>   klookup :: k -> KMap k v -> Maybe v
>   -- other methods ...

> instance KKey Bool where
>   data KMap Bool elt = MB (Maybe elt) (Maybe elt)
>   kempty = MB Nothing Nothing
>   klookup False (MB mf _) = mf
>   klookup True  (MB _ mt) = mt

> instance (KKey a, KKey b) => KKey (Either a b) where
>   data KMap (Either a b) elt = MS (KMap a elt) (KMap b elt)
>   kempty = MS kempty kempty
>   klookup (Left  k) (MS m _) = klookup k m
>   klookup (Right k) (MS _ m) = klookup k m

> instance (KKey a, KKey b) => KKey (a,b) where
>   data KMap (a,b) elt = MP (KMap a (KMap b elt))
>   kempty = MP kempty
>   klookup (a,b) (MP m) = case klookup a m of
>                            Nothing -> Nothing
>                            Just m' -> klookup b m'


a finite map makes the instance for Int easier than before
- invoke an existing data structure for finite maps keyed by Int:

> instance KKey Int where
>   newtype KMap Int elt = MI (IM.IntMap elt)
>   kempty = MI IM.empty
>   klookup k (MI m) = IM.lookup k m

------------------------------------------------------------------------------
3.4 Session types and their duality

finite map's key and lookup function can be regarded as pair of communicating processes
- the key sends indices to lookup
- lookup responds with element’s value

generalise to relationship between pair of processes that communicate by sending and receiving values in a session

> data Stop = Done
> newtype In  a b = In (a -> IO b)
> data    Out a b = Out a   (IO b)

couple two processes with complementary (aka dual) protocols

> class Session a where
>   -- represents consumers of type a -> IO () by producers of matching Dual a type
>   -- (like Memo represents functions of type a -> w by tables of type Table a w)
>   type Dual a
>   run :: a -> Dual a -> IO ()

> instance (Session b) => Session (In a b) where
>   type Dual  (In  a b) = Out a (Dual b)
>   run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

> instance (Session b) => Session (Out a b) where
>   type Dual  (Out a b) = In  a (Dual b)
>   run (Out a d) (In f) = f a >>= \b -> d >>= \c -> run c b

> instance Session Stop where
>   type Dual Stop = Stop
>   run Done Done  = return ()

types guarantee protocols match

> -- | function-like value
> -- accepts two Ints in succession
> -- prints “Thinking”
> -- responds with sum
> -- `add_server` is a "process"
> -- interface protocol is specified by its type – a "session type"
> add_server :: In Int (In Int (Out Int Stop))
> add_server = In $ \x -> return $ In $ \y -> do
>   putStrLn "Thinking"
>   return $ Out (x + y) (return Done)

> add_client :: Out Int (Out Int (In Int Stop))
> add_client = Out 3 $ return $ Out 4 $ do
>   putStrLn "Waiting"
>   return $ In $ \z -> print z >> return Done

couple (either way):
   > run add_server add_client
   Thinking
   Waiting
   7
   > run add_client add_server
   Thinking
   Waiting
   7

if run given processes that are not dual, type error

> neg_server :: In Int (Out Int Stop)
> neg_server = In $ \x -> do
>   putStrLn "Thinking"
>   return $ Out (-x) (return Done)

   > run add_client neg_server

      Couldn't match type ‘Out Int Stop’ with ‘In Int (Out Int Stop)’
      Expected type: Dual (Out Int (Out Int (In Int Stop)))
        Actual type: In Int (Out Int Stop)

Protocols do not allow past communication to affect the type and direction of future exchanges.
- seems impossible to write protocol that begins by receiving Bool, then does addition if True else negation
- but can express protocol that chooses between addition and negation
- more generally : protocol that chooses among ways to continue
- treat choice as distinct protocol step
- receiver of choice has product type
- sender has sum type

> instance (Session a, Session b) => Session (Either a b) where
>   type Dual (Either a b) = (Dual a, Dual b)
>   run (Left  y) (x,_)    = run y x
>   run (Right y) (_,x)    = run y x

> instance (Session a, Session b) => Session (a, b) where
>   type Dual (a,b)        = Either (Dual a) (Dual b)
>   run (x,_) (Left  y)    = run x y
>   run (_,x) (Right y)    = run x y

> server :: (In Int (Out Int Stop),
>            In Int (In Int (Out Int Stop)))
> server = (neg_server, add_server)

> client :: Either (Out Int (In Int Stop))
>                  (Out Int (Out Int (In Int Stop)))
> client = Right add_client

    > run server client
    > run client server

session type of client hides which choice selected
- may depend on user input at run time, type checker has no way of knowing
- type checker verifies server can handle either choice

above instances only allow finite number of exchanges
- restriction not fundamental
- recursive in protocols can be expressed (e.g., using fixpoint type level)
- can also separate notion of process from that of channel
  and associate protocol with channel rather than process

------------------------------------------------------------------------------
4 Typed sprintf and sscanf

TODO

------------------------------------------------------------------------------
5 Fun with phantom types

type functions above return types that are used in value-level computations.
- necessary to type-check the functions above
- e.g., Ref enables value functions newIORef and newSTRef to be overloaded under the name newRef

this section considers type functions that operate on phantom types

Phantom types enforce distinctions among values with same runtime representation
- e.g., numbers with different units, strings for different XML elements

Functions on phantom types propagate these distinctions through a static approximation of the computation.

Phantom types and functions on them enable reasoning more precisely about program behaviour before running it
- by defining additional type-checking rules that refine Haskell’s built-in ones

------------------------------------------------------------------------------
5.1 Pointer arithmetic and alignment

example of pointer arithmetic/alignment

pointer
- represented by a machine word at run time
- aligned (i.e.,divisible) by integer

goal
- distinguish types of differently aligned pointers (preventing misaligned pointers)

> data Zero
> data Succ n

> type One   = Succ Zero
> type Two   = Succ One
> type Four  = Succ (Succ Two)
> type Six   = Succ (Succ Four)
> type Eight = Succ (Succ Six)

> class Nat n where
>   toInt :: n -> Int
> instance Nat Zero where
>   toInt _ = 0
> instance (Nat n) => Nat (Succ n) where
>   toInt _ = 1 + toInt (undefined :: n)

toInt uses proxy arguments
- toInt never examines its arg
- must take an arg
  - as proxy : specifies which instance to use

   > toInt (undefined :: Two)
   2

represent pointer or offset as machine word at run time
- use phantom type at compile time to track how alignment

> -- newtype so no runtime represention
> -- these phantom-type alignment annotations have no runtime overhead
> newtype Pointer n = MkPointer Int -- value of Pointer n is n-byte-aligned pointer : Pointer Four is 4-byte-aligned pointer
> newtype Offset  n = MkOffset  Int -- value of Offset  n is multiple of n

To keep alignment knowledge sound, constructors MkPointer and MkOffset must not be exported.

Use smart constructors:

> -- | multiple i : i-th multiple of  alignment specified by return type
> -- e.g., multiple 3 :: Offset Four yields MkOffset 12, the 3rd multiple of a Four-byte alignment
> multiple :: forall n. (Nat n) => Int -> Offset n
> multiple i = MkOffset (i * toInt (undefined :: n))

When pointer incremented by offset, resulting pointer aligned by GCD of alignments of original pointer and offset.

> -- | if p :: Pointer Eight and o :: Offset Six, then oadd p o :: Pointer Two
> oadd :: Pointer m -> Offset n -> Pointer (GCD Zero m n)
> oadd (MkPointer x) (MkOffset y) = MkPointer (x + y)


define GCD type function
- three arguments: GCD d m n computes GCD of d+m and d+n

type checker does not check that oadd x y is aligned by GCD
- oadd and multiple are trusted code
- their types  express claims the programmer must guarantee

If fetch32 is operation that works on 4-aligned pointers only, then we can give it the type

    (GCD Zero n Four ~ Four) => Pointer n -> IO ()

fetch32 works on any pointer whose alignment’s GCD with 4 is 4.

Because type function GCD has no value-level operations, define it without a type class:

> type family GCD d m n
> type instance GCD     d     Zero     Zero    = d
> type instance GCD     d    (Succ m) (Succ n) = GCD (Succ    d) m n
> type instance GCD  Zero    (Succ m)  Zero    = Succ m
> type instance GCD (Succ d) (Succ m)  Zero    = GCD (Succ Zero) d m
> type instance GCD  Zero     Zero    (Succ n) = Succ n
> type instance GCD (Succ d)  Zero    (Succ n) = GCD (Succ Zero) d n

------------------------------------------------------------------------------
5.2 Tracking state and control in a parameterised monad

TODO

------------------------------------------------------------------------------
5.3 Keeping the kinds straight

Above is untyped functional programming at the type level.

kind of GCD is

    GCD :: * -> * -> * -> *

compiler will accept GCD Int Zero Bool)

alleviate using Nat n constraint (aka kind predicate):

    newtype Nat n => Pointer n = MkPointer Int

rules out Pointer Bool

want algebraic data kinds when writing type-level functions (like algebraic data types in term-level programs)

Want to ‘lift’ data type declaration
   data N = Zero | Succ N
to the kind level

then GCD could have kind
    GCD :: N -> N -> N -> N

use DataKinds

------------------------------------------------------------------------------
5.4 Type-preserving compilers

index a GADT by a phantom type to ensure only well-typed programs can be represented

> data Exp a where
>   Enum :: Int                     -> Exp Int
>   Eadd :: Exp Int      -> Exp Int -> Exp Int
>   Eapp :: Exp (a -> b) -> Exp a   -> Exp b
>   -- ...

> optimize :: Exp a -> Exp a
> optimize = undefined

> evaluate :: Exp a -> a
> evaluate = undefined

expresses
- optimiser only has to deal with well-typed terms
- optimising term does not change its type
- evaluating term yields a value of correct type

> newtype CpsT a = CpsT a -- HC fake : maps `a` to  CPS-converted version
> cpsConvert :: Exp a -> Exp (CpsT a)
> cpsConvert = undefined

CpsT can be expressed as type-level function
