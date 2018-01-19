> {-# LANGUAGE NoImplicitPrelude #-}
> {-# LANGUAGE OverloadedStrings #-}
>
> module U where
>
> import qualified Control.Concurrent.Async    as A
> import qualified Control.Concurrent.STM.TVar as TV
> import qualified Control.Monad.ST            as ST
> import           Data.IORef
> import qualified Data.Map.Strict             as M
> import           Data.Maybe                  (fromJust)
> import qualified Data.Text                   as T
> import qualified Data.Vector                 as V
> import qualified Data.Vector.Mutable         as VM
> import qualified Prelude
> import           Protolude
> import           Text.Parsec                 as PS
> import           Text.Parsec.Text            as PS
> import           Text.Parser.Token

https://www.snoyman.com/blog/2017/12/what-makes-haskell-unique

What Makes Haskell Unique
December 17, 2017


from conference talk : https://fby.by/
slides:  https://www.snoyman.com/reveal/what-makes-haskell-unique

------------------------------------------------------------------------------
HASKELL FEATURES

- Functional
- Statically typed
- Pure (rare in other langs)
- Lazy (rare in other langs)
- Strongly typed
- Green threads
- Native executables
- Garbage collected
- Immutability

the combination of these features that makes Haskell unique

purity + strong typing + FP
- high level form of expression
- easy to write, read, modify
- efficient

------------------------------------------------------------------------------
ASYNC I/O AND CONCURRENCY

    json1 := httpGet(url1)
    json2 := httpGet(url2)
    useJsonBodies(json1, json2)

- blocking code
- ties up thread waiting for each response

Instead, using async I/O: js callback (hell):

    httpGetA(url1, |json1| =>
      httpGetA(url2, |json2| =>
        useJsonBodies(json1, json2)  )  )

asynchronous version in Haskell

    json1 <- httpGet url1
    json2 <- httpGet url2
    useJsonBodies json1 json2

looks like blocking pseudocode above
GHC runtime converts blocking-style into async calls, scheduling threads, ...

Erlang and Go: both have this too

Concurrency

    (json1, json2) <- concurrently
      (httpGet url1)
      (httpGet url2)
    useJsonBodies json1 json2

- GHC green threads makes forking cheap
- async lib API

Canceling

Instead of using both JSON response bodies, use whichever one comes back first

    eitherJson <- race
      (httpGet url1)
      (httpGet url2)
    case eitherJson of
      Left  json1 -> useJsonBody1 json1
      Right json2 -> useJsonBody2 json2

GHC supports async exceptions
- enables cancel other threads
- used in 'race' impl

thread scheduing, canceling, etc
- not just for I/O
- for CPU-intensive tasks too
- can fork thousands of threads

    let tenSeconds = 10 * 1000 * 1000
    timeout tenSeconds expensiveComputation

Summary: concurrency and async I/O

Advantages
- Cheap threads
- Simple API
- Highly responsive
Disadvantages
- Complicated runtime system
- Need to be aware of async exceptions when writing code

------------------------------------------------------------------------------
IMMUTABILITY AND PURITY

Values : immutable by default
Mutability must be explicitly indicated
Mutating : considered a side effect : tracked by type system

impossible in Haskell

    let mut total = 0
        loop i =
          if i > 1000000
            then total
            else total += i; loop (i + 1)
     in loop 1

haskell

> foo = do
>   total <- newIORef 0
>   let loop i = if i > 1000000 then
>                  readIORef total
>                else do
>                  modifyIORef total (+ i)
>                  loop (i + 1)
>    in loop 1

avoid mutable variables, use functional style

> foo2 =
>   let loop i total = if i > 1000000 then
>                        total
>                      else
>                        loop (i + 1) (total + i)
>   in loop 1 0

why immutable/pure?

Reasoning about code easier without side-effects

> data TestResult = TestResult
>   { name  :: T.Text
>   , score :: Int
>   }
>
> one :: IO ()
> one = do
>   results <- readResultsFromFile "results.txt"
>   printScoreRange results
>   putText $ "First result was by: " <> name (Prelude.head results)
>
> printScoreRange :: [TestResult] -> IO ()
> printScoreRange results = do
>   let results' = sortBy score' results
>   putText $ "Lowest: "  <> show (score (Prelude.head results'))
>   putText $ "Highest: " <> show (score (Prelude.last results'))
>  where
>   score' x y = score x `compare` score y
>
> readResultsFromFile :: FilePath -> IO [TestResult]
> readResultsFromFile _ = undefined


know it's impossible for printScoreRange to modify the results of one

Data races

more powerful than single threaded case : immutability and multithreaded app

> two :: IO ()
> two = do
>   results <- readResultsFromFile "results.txt"
>   A.concurrently_
>     (printFirstResult results)
>     (printScoreRange  results)
>
> printFirstResult results =
>   putText $ "First result was by: " <> name (Prelude.head results)

no need to worry about concurrent accesses to data structures
- impossible for the other threads to alter data

Mutability when needed

more efficient to sort vector using mutable access
- create mutable data structures, and mutate in place
  - breaks all guarantees mentioned above
- create mutable copy of the original data
  - perform mutable algorithm
  - freeze into immutable version

> -- sortMutable :: VM.IOVector a -> ST.ST ST.RealWorld (VM.IOVector a)
> sortMutable = undefined -- normal sorting algorithm

> sortImmutable :: V.Vector a -> V.Vector a
> sortImmutable orig = ST.runST $ do
>   mutable <- undefined -- VM.new (length orig)
>   V.copy mutable orig
>   sortMutable mutable
>   V.freeze mutable

ST : temporary/local mutable effects : none visible outside

Summary: immutability and purity

Advantages
- Easier to reason about code
- Avoid many cases of data races
- Functions are more reliable, returning the same output for the same input
Disadvantages
- Lots of ceremony if you actually want mutation
- Some runtime performance hit for mutable algorithms

------------------------------------------------------------------------------
SOFTWARE TRANSACTIONAL MEMORY

> runServer = undefined
> accounts = undefined
> data Request = Request { fromR :: T.Text, toR :: T.Text, amtR :: Int }
>
> x = runServer $ \request -> atomically $ do
>   let fromVar = fromJust $ M.lookup (fromR request) accounts
>       toVar   = fromJust $ M.lookup (toR   request) accounts
>   origFrom <- TV.readTVar fromVar
>   TV.writeTVar fromVar (origFrom - amtR request)
>   origTo <- TV.readTVar toVar
>   TV.writeTVar toVar (origTo + amtR request)


can be shorter

atomically : ensures complete transaction committed : no partiable
any vars mutated by another thread before TX causes rollback

TVar : transactional variable
- alternative to IORef
- others: channels, MVars

Summary of STM

Advantages
- Makes concurrent data modification much easier
- Bypass many race conditions and deadlocks
Disadvantages
- Depends on purity to work at all
- Not really a disadvantage, you're already stuck with purity in Haskell

------------------------------------------------------------------------------
LAZINESS

    let loop i total =
          if i > 1000000
            then total
            else loop (i + 1) (total + i)
     in loop 1 0

problem : space leaks

    let foo = 1 + 2

foo contains thunk to apply  + to 1 and 2

Each time in loop
- do comparison : i > 1000000
- forced to evaluate comparison : turns 'i' into integer
- but never look at value of total
- so builds huge tree that looks like : "add 1 to the result of add 2 to the result of ...

work around : explicit about which values evaluated

    let loop i !total =     -- bang pattern : evaluate before running rest of function
          if i > 1000000
            then total
            else loop (i + 1) (total + i)
     in loop 1 0

Laziness is awesome

instead of above, do:

    sum [1..1000000]

looks like it needs to allocate entire range then do sum, but, instead
- allocate thunk
- each step in list thunk generates one new integer and a new thunk for the rest of the list

    sum (filter even [1..1000000])

    sum (map (`mod` 13) (filter even [1..1000000]))

Short circuiting for free

foo() && bar()
bar is only called if foo returns true

Not special operators: just use laziness

    False && _ = False
    True  && x = x

    True  || _ = True
    False || x = x

Other downsides

exceptions can be hiding inside any thunk (known as partial values and partial functions)

    head []

use total functions

Summary of laziness

Advantages
- More composable code
- Get efficient results from combining high level functions
- Short-circuiting like && and || is no longer a special case
Disadvantages
- Need to worry about space leaks
- Exceptions can be hiding in many places
- Unfortunately some bad functions like foldl still hanging around

------------------------------------------------------------------------------
OTHERS

PARSER (and other) DSLs
- Operator overloading
- Abstract type classes like Applicative and Alternative a natural fit, e.g.: parseXMLElement <|> parseXMLText.
- Able to reuse huge number of existing library functions, e.g. optional, many
- General purpose do-notation is great

> {-
> type Hour = Integer
> type Minutes = Integer
> type Seconds = Integer
> data Time = Time Hour Minutes Seconds (Maybe AmPm)
> data AmPm = Am | Pm
>
> parseAmPm :: Parser Time
> parseAmPm = Time
>   <$> decimal
>   <*> (":" *> decimal)
>   <*> (":" *> decimal)
>   <*> PS.optional (("AM" $> Am) PS.<|> ("PM" $> Pm))
> -}

