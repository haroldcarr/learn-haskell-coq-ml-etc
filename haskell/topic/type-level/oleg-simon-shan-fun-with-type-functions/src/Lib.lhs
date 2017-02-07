> {-# LANGUAGE FlexibleContexts      #-}
> {-# LANGUAGE FlexibleInstances     #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeFamilies          #-}
>
> module Lib where
>
> import Data.Function (fix)
> import Data.Map
> import Prelude as P

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
>   getTable     :: Table a w -> [w] -- HC

> -- | memoise funs from Bool by storing its two return values as a lazy pair
> -- lazy pair is the memo table
> instance Memo Bool where
>   data Table Bool w       = TBool w w
>   toTable f               = TBool (f True) (f False)
>   fromTable (TBool x y) b = if b then x else y
>   getTable  (TBool x y)   = [x,y]

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
>   getTable                    _  = undefined

we can memoise fun from product type
- e.g., (a,b)
- storing a memo table from a whose entries are memo tables from b
- leverage currying isomorphism between fun types (a,b) -> w and a -> b -> w

> instance (Memo a, Memo b) => Memo (a,b) where
>   newtype Table (a,b) w = TProduct (Table a (Table b w))
>   toTable f = TProduct (toTable (\x -> toTable (\y -> f (x,y))))
>   fromTable (TProduct t) (x,y) = fromTable (fromTable t x) y
>   getTable                  _  = undefined

HC: memoize fun from int to int

> -- | memoise funs from Int by storing return values in lazy list
> -- lazy list is the memo table
> instance Memo Int where
>   data Table Int w      = TList [w]
>   toTable f             = TList (P.map f [0 ..])
>   fromTable (TList m) i = m !! i
>   getTable  (TList m)   = m

 https://kseo.github.io/posts/2017-01-14-memoization-in-hasekll.html

-- HC TODO : this version is not storing results of recursive calls

> fib :: (Int -> Integer) -> Int -> Integer
> fib f 0 = 0
> fib f 1 = 1
> fib f n = f (n - 1) + f (n - 2)

> fixFib   = fix fib
> fibTable = toTable fixFib
> t = getTable fibTable

> fibMemo :: Int -> Integer
> fibMemo i = fromTable fibTable i

> fibMemo' :: Int -> Integer
> fibMemo' i = fromTable (toTable fibMemo) i

