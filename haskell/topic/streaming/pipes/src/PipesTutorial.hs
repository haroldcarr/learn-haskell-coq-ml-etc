{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module PipesTutorial where

import           Control.Monad (unless)
import           Pipes
import qualified Pipes.Prelude as P
import           System.IO     (isEOF)

{-# ANN module "HLint: ignore Reduce duplication" #-}

------------------------------------------------------------------------------
{-
Pipes.Tutorial

Contents

    Introduction
    Producers
    Composability
    Consumers
    Pipes
    ListT
    Tricks
    Conclusion
    Appendix: Types
    Appendix: Time Complexity
    Copyright

------------------------------------------------------------------------------
stream programming : choose two of three features:

    Effects
    Streaming
    Composability

sacrifice Effects
- get pure/lazy lists
- can use composable functions (COMPOSABLE)in constant space (STREAMING)
- but no interleaving of effects

sacrifice Streaming
- get mapM, forM and "ListT done wrong"
- COMPOSABLE and EFFECTFUL
- do not return a result until entire list processed and loaded into memory

sacrifice Composability
- get tightly coupled read, transform, and write loop in IO
- STREAMING and EFFECTFUL
- not modular

pipes gives all three features: effectful, streaming, composable programming

------------------------------------------------------------------------------
Introduction

components communicate using

    yield: Send output data
    await: Receive input data

pipes has four types of components built around these two commands:

    Producers : yield           values : model streaming sources
    Pipes     : yield and await values : model stream transformations
    Consumers :           await values : model streaming sinks
    Effects   :                        : model non-streaming components

connect via:

    Producers : for   handles yields
    Pipes     : (>->) handles yields and awaits
    Consumers : (>~)  handles            awaits
    Effects   : (>>=) handles return values

connecting components change types to reflect in/outputs that are fused away

done connecting things when you get an Effect
- meaning all in/outputs have been handled
- run Effect to begin streaming

------------------------------------------------------------------------------
Producers

Producers
- effectful streams of input
- monad transformer that extends base monad with yield command
- yield enables sending output downstream to anonymous handler

example: incrementally read Strings from stdin and yield them downstream
-}

--         +--------+-- A 'Producer' that yields 'String's
--         |        |
--         |        |      +-- Every monad transformer has a base monad.
--         |        |      |   This time the base monad is 'IO'.
--         |        |      |
--         |        |      |  +-- Every monadic action has a return value.
--         |        |      |  |   This action returns '()' when finished
--         v        v      v  v
stdinLn :: Producer String IO ()
stdinLn = do
    eof <- lift isEOF        -- 'lift' an 'IO' action from base monad into Producer
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop
{-
yield
- emits a value
- suspending current Producer until value is consumed
- if not consumed then yield never returns
- think of yield as

 yield :: Monad m => a -> Producer a m ()
 yield :: Monad m => a -> Pipe   x a m ()

types say : "can use yield within a Producer, but also in other contexts"

--------------------------------------------------
FOR

for loops : consume a Producer

 --                +-- Producer      +-- The body of the   +-- Result
 --                |   to loop       |   loop              |
 --                v   over          v                     v
 --                --------------    ------------------    ----------
 for :: Monad m => Producer a m r -> (a -> Effect     m ()) -> Effect     m r
 for :: Monad m => Producer a m r -> (a -> Producer b m ()) -> Producer b m r

(for producer body) loops over (producer), substituting each yield in (producer) with (body).

- body takes `a`, the Producer's output
- Producer's return value `r` matches return value of Effect result
  - so `for` must loop over entire Producer and not skip anything

type says : "if  1st arg of `for` is a Producers
             and 2nd argt returns an Effect,
             then final result must be an Effect"

a Producer that never yields is also an Effect:

 data X  -- The uninhabited type
 type Effect m r = Producer X m r

This is why for permits two different type signatures

 for :: Monad m => Producer a m r -> (a -> Producer b m ()) -> Producer b m r
 for :: Monad m => Producer a m r -> (a -> Producer X m ()) -> Producer X m r
 for :: Monad m => Producer a m r -> (a -> Effect     m ()) -> Effect     m r

example
-}

loop1 :: Effect IO ()
loop1 = for stdinLn $ \str -> -- read this like: "for str in stdinLn"
    lift $ putStrLn str       -- the body of the 'for' loop

loop2 = for stdinLn (lift . putStrLn)

{-
for loops over stdinLn
replaces each yield in stdinLn with body of loop
equivalent to following code

loop = do                      |  stdinLn = do
    eof <- lift isEOF          |      eof <- lift isEOF
    unless eof $ do            |      unless eof $ do
        str <- lift getLine    |          str <- lift getLine
        (lift . putStrLn) str  |          yield str
        loop                   |          stdinLn

think of yield as creating      a hole
think of for   as a way to fill a hole

loop only lifts actions from base monad and does nothing else.
- property true for all Effects
- Effects are wrappers around actions in base monad
- means can run Effects to remove their lifts and lower them back to computation in  base monad:

 runEffect :: Monad m => Effect m r -> m r
-}
runLoop1 = runEffect loop1
runLoop2 = runEffect loop2
runLoop3 = runEffect $ for stdinLn (lift . putStrLn)
{-
main = do               |  loop = do
    eof <- isEof        |      eof <- lift isEof
    unless eof $ do     |      unless eof $ do
        str <- getLine  |          str <- lift getLine
        putStrLn str    |          (lift . putStrLn) str
        main            |          loop

main like a tighly-coupled hand written

--------------------------------------------------
for to loop over lists

each :: (Monad m, Foldable f) => f a -> Producer a m ()
each as = mapM_ yield as
-}
runForEachList = runEffect $ for (each [1..4::Int])     (lift . print)
runForEachJust = runEffect $ for (each (Just (1::Int))) (lift . print)
{-

------------------------------------------------------------------------------
Composability

the body of a for loop can be a Producer

example : loop body that creates three copies of every value:
-}
triple :: Monad m => a -> Producer a m ()
triple x = do
    yield x
    yield x
    yield x

tripleLoop1 :: Producer String IO ()
tripleLoop1 = for P.stdinLn triple

-- same as:

tripleLoop2 :: Producer String IO ()
tripleLoop2 = for P.stdinLn $ \x -> do
    yield x
    yield x
    yield x

-- tripleLoopN is a Producer that outputs Strings (read from stdin)

runTripleLoop1 = runEffect $ for tripleLoop1 (lift . putStrLn)
runTripleLoop2 = runEffect $ for tripleLoop2 (lift . putStrLn)

-- can write as nested for loop

runTripleLoopNested = runEffect $
    for P.stdinLn $ \str1 ->
        for (triple str1) $ \str2 ->
            lift $ putStrLn str2
{-
nested is special case of equality

 s :: Monad m =>      Producer a m ()  -- i.e. 'P.stdinLn'
 f :: Monad m => a -> Producer b m ()  -- i.e. 'triple'
 g :: Monad m => b -> Producer c m ()  -- i.e. '(lift . putStrLn)'

 for (for s f) g = for s (\x -> for (f x) g)

rationale of equality
- first define (~>) (pronounced "into") operator (point-free version of `for`)

 (~>) :: Monad m
      => (a -> Producer b m ())
      -> (b -> Producer c m ())
      -> (a -> Producer c m ())
 (f ~> g) x = for (f x) g

then substitute above

 f :: Monad m => a -> Producer b m ()
 g :: Monad m => b -> Producer c m ()
 h :: Monad m => c -> Producer d m ()

 -- Associativity
 (f ~> g) ~> h = f ~> (g ~> h)

(~>) properties

-- Left Identity
yield ~> f = f

-- Right Identity
f ~> yield = f

yield and (~>) form a Category, the generator category
- (~>)  : composition operator
- yield : identity

notice : translate left identity law to use for instead of (~>):

for (yield x) f = f x

says : if iterating over a pure single-element Producer,
       then could instead cut out the middle
       and directly apply loop body to that single element

notie : translate the right identity law to use for instead of (~>):

for s yield = s

says : if only re-yield every element of a stream, get back original stream

three "for loop" laws summarize how for loops behave
- means that Producers are composable

using (~>)
-}
runTripleOp = runEffect $ for P.stdinLn (triple ~> lift . putStrLn)
{-

pipes design philosophy

    Define the API in terms of categories
    Specify expected behavior in terms of category laws
    Think compositionally instead of sequentially

------------------------------------------------------------------------------
Consumers

Sometimes you don't want to use a for loop because you don't want to consume every element of a Producer or because you don't want to process every value of a Producer the exact same way.

The most general solution is to externally iterate over the Producer using the next command:

 next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))

Think of next as pattern matching on the head of the Producer. This Either returns a Left if the Producer is done or it returns a Right containing the next value, a, along with the remainder of the Producer.

However, sometimes we can get away with something a little more simple and elegant, like a Consumer, which represents an effectful sink of values. A Consumer is a monad transformer that extends the base monad with a new await command. This await command lets you receive input from an anonymous upstream source.

The following stdoutLn Consumer shows how to incrementally await Strings and print them to standard output, terminating gracefully when receiving a broken pipe error:

import Control.Monad (unless)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G
import Pipes

--          +--------+-- A 'Consumer' that awaits 'String's
--          |        |
--          v        v
stdoutLn :: Consumer String IO ()
stdoutLn = do
    str <- await  -- 'await' a 'String'
    x   <- lift $ try $ putStrLn str
    case x of
        -- Gracefully terminate if we got a broken pipe error
        Left e@(G.IOError { G.ioe_type = t}) ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn

await is the dual of yield: we suspend our Consumer until we receive a new value. If nobody provides a value (which is possible) then await never returns. You can think of await as having the following type:

 await :: Monad m => Consumer a m a

One way to feed a Consumer is to repeatedly feed the same input using (>~) (pronounced "feed"):

 --                 +- Feed       +- Consumer to    +- Returns new
 --                 |  action     |  feed           |  Effect
 --                 v             v                 v  
 --                 ----------    --------------    ----------
 (>~) :: Monad m => Effect m b -> Consumer b m c -> Effect m c

(draw >~ consumer) loops over (consumer), substituting each await in (consumer) with (draw).

So the following code replaces every await in stdoutLn with (lift getLine) and then removes all the lifts:

>>> runEffect $ lift getLine >~ stdoutLn
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
...

You might wonder why (>~) uses an Effect instead of a raw action in the base monad. The reason why is that (>~) actually permits the following more general type:

 (>~) :: Monad m => Consumer a m b -> Consumer b m c -> Consumer a m c

(>~) is the dual of (~>), composing Consumers instead of Producers.

This means that you can feed a Consumer with yet another Consumer so that you can await while you await. For example, we could define the following intermediate Consumer that requests two Strings and returns them concatenated:

doubleUp :: Monad m => Consumer String m String
doubleUp = do
    str1 <- await
    str2 <- await
    return (str1 ++ str2)

-- more concise: doubleUp = (++) <$> await <*> await

We can now insert this in between (lift getLine) and stdoutLn and see what happens:

>>> runEffect $ lift getLine >~ doubleUp >~ stdoutLn
Test<Enter>
ing<Enter>
Testing
ABC<Enter>
DEF<Enter>
ABCDEF
42<Enter>
000<Enter>
42000
...

doubleUp splits every request from stdoutLn into two separate requests and returns back the concatenated result.

We didn't need to parenthesize the above chain of (>~) operators, because (>~) is associative:

-- Associativity
(f >~ g) >~ h = f >~ (g >~ h)

... so we can always omit the parentheses since the meaning is unambiguous:

f >~ g >~ h

Also, (>~) has an identity, which is await!

-- Left identity
await >~ f = f

-- Right Identity
f >~ await = f

In other words, (>~) and await form a Category, too, specifically the iteratee category, and Consumers are also composable.
Pipes

Our previous programs were unsatisfactory because they were biased either towards the Producer end or the Consumer end. As a result, we had to choose between gracefully handling end of input (using stdinLn) or gracefully handling end of output (using stdoutLn), but not both at the same time.

However, we don't need to restrict ourselves to using Producers exclusively or Consumers exclusively. We can connect Producers and Consumers directly together using (>->) (pronounced "pipe"):

 (>->) :: Monad m => Producer a m r -> Consumer a m r -> Effect m r

This returns an Effect which we can run:

-- echo2.hs

import Pipes
import qualified Pipes.Prelude as P  -- Pipes.Prelude also provides 'stdoutLn'

main = runEffect $ P.stdinLn >-> P.stdoutLn

This program is more declarative of our intent: we want to stream values from stdinLn to stdoutLn. The above "pipeline" not only echoes standard input to standard output, but also handles both end of input and broken pipe errors:

$ ./echo2
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
<Ctrl-D>
$

(>->) is "pull-based" meaning that control flow begins at the most downstream component (i.e. stdoutLn in the above example). Any time a component awaits a value it blocks and transfers control upstream and every time a component yields a value it blocks and restores control back downstream, satisfying the await. So in the above example, (>->) matches every await from stdoutLn with a yield from stdinLn.

Streaming stops when either stdinLn terminates (i.e. end of input) or stdoutLn terminates (i.e. broken pipe). This is why (>->) requires that both the Producer and Consumer share the same type of return value: whichever one terminates first provides the return value for the entire Effect.

Let's test this by modifying our Producer and Consumer to each return a diagnostic String:

-- echo3.hs

import Control.Applicative ((<$))  -- (<$) modifies return values
import Pipes
import qualified Pipes.Prelude as P
import System.IO

main = do
    hSetBuffering stdout NoBuffering
    str <- runEffect $
        ("End of input!" <$ P.stdinLn) >-> ("Broken pipe!" <$ P.stdoutLn)
    hPutStrLn stderr str

This lets us diagnose whether the Producer or Consumer terminated first:

$ ./echo3
Test<Enter>
Test
<Ctrl-D>
End of input!
$ ./echo3 | perl -e 'close STDIN'
Test<Enter>
Broken pipe!
$

You might wonder why (>->) returns an Effect that we have to run instead of directly returning an action in the base monad. This is because you can connect things other than Producers and Consumers, like Pipes, which are effectful stream transformations.

A Pipe is a monad transformer that is a mix between a Producer and Consumer, because a Pipe can both await and yield. The following example Pipe is analagous to the Prelude's take, only allowing a fixed number of values to flow through:

-- take.hs

import Control.Monad (replicateM_)
import Pipes
import Prelude hiding (take)

--              +--------- A 'Pipe' that
--              |    +---- 'await's 'a's and
--              |    | +-- 'yield's 'a's
--              |    | |
--              v    v v
take ::  Int -> Pipe a a IO ()
take n = do
    replicateM_ n $ do                     -- Repeat this block 'n' times
        x <- await                         -- 'await' a value of type 'a'
        yield x                            -- 'yield' a value of type 'a'
    lift $ putStrLn "You shall not pass!"  -- Fly, you fools!

You can use Pipes to transform Producers, Consumers, or even other Pipes using the same (>->) operator:

 (>->) :: Monad m => Producer a m r -> Pipe   a b m r -> Producer b m r
 (>->) :: Monad m => Pipe   a b m r -> Consumer b m r -> Consumer a m r
 (>->) :: Monad m => Pipe   a b m r -> Pipe   b c m r -> Pipe   a c m r

For example, you can compose take after stdinLn to limit the number of lines drawn from standard input:

maxInput :: Int -> Producer String IO ()
maxInput n = P.stdinLn >-> take n

>>> runEffect $ maxInput 3 >-> P.stdoutLn
Test<Enter>
Test
ABC<Enter>
ABC
42<Enter>
42
You shall not pass!
>>> 

... or you can pre-compose take before stdoutLn to limit the number of lines written to standard output:

maxOutput :: Int -> Consumer String IO ()
maxOutput n = take n >-> P.stdoutLn

>>> runEffect $ P.stdinLn >-> maxOutput 3
<Exact same behavior>

Those both gave the same behavior because (>->) is associative:

(p1 >-> p2) >-> p3 = p1 >-> (p2 >-> p3)

Therefore we can just leave out the parentheses:

>>> runEffect $ P.stdinLn >-> take 3 >-> P.stdoutLn
<Exact same behavior>

(>->) is designed to behave like the Unix pipe operator, except with less quirks. In fact, we can continue the analogy to Unix by defining cat (named after the Unix cat utility), which reforwards elements endlessly:

cat :: Monad m => Pipe a a m r
cat = forever $ do
    x <- await
    yield x

cat is the identity of (>->), meaning that cat satisfies the following two laws:

-- Useless use of 'cat'
cat >-> p = p

-- Forwarding output to 'cat' does nothing
p >-> cat = p

Therefore, (>->) and cat form a Category, specifically the category of Unix pipes, and Pipes are also composable.

A lot of Unix tools have very simple definitions when written using pipes:

-- unix.hs

import Control.Monad (forever)
import Pipes
import qualified Pipes.Prelude as P  -- Pipes.Prelude provides 'take', too
import Prelude hiding (head)

head :: Monad m => Int -> Pipe a a m ()
head = P.take

yes :: Monad m => Producer String m r
yes = forever $ yield "y"

main = runEffect $ yes >-> head 3 >-> P.stdoutLn

This prints out 3 'y's, just like the equivalent Unix pipeline:

$ ./unix
y
y
y
$ yes | head -3
y
y
y
$

This lets us write "Haskell pipes" instead of Unix pipes. These are much easier to build than Unix pipes and we can connect them directly within Haskell for interoperability with the Haskell language and ecosystem.
ListT

pipes also provides a "ListT done right" implementation. This differs from the implementation in transformers because this ListT:

    obeys the monad laws, and
    streams data immediately instead of collecting all results into memory.

The latter property is actually an elegant consequence of obeying the monad laws.

To bind a list within a ListT computation, combine Select and each:

import Pipes

pair :: ListT IO (Int, Int)
pair = do
    x <- Select $ each [1, 2]
    lift $ putStrLn $ "x = " ++ show x
    y <- Select $ each [3, 4]
    lift $ putStrLn $ "y = " ++ show y
    return (x, y)

You can then loop over a ListT by using every:

 every :: Monad m => ListT m a -> Producer a m ()

So you can use your ListT within a for loop:

>>> runEffect $ for (every pair) (lift . print)
x = 1
y = 3
(1,3)
y = 4
(1,4)
x = 2
y = 3
(2,3)
y = 4
(2,4)

... or a pipeline:

>>> import qualified Pipes.Prelude as P
>>> runEffect $ every pair >-> P.print
<Exact same behavior>

Note that ListT is lazy and only produces as many elements as we request:

>>> runEffect $ for (every pair >-> P.take 2) (lift . print)
x = 1
y = 3
(1,3)
y = 4
(1,4)

You can also go the other way, binding Producers directly within a ListT. In fact, this is actually what Select was already doing:

 Select :: Producer a m () -> ListT m a

This lets you write crazy code like:

import Pipes
import qualified Pipes.Prelude as P

input :: Producer String IO ()
input = P.stdinLn >-> P.takeWhile (/= "quit")

name :: ListT IO String
name = do
    firstName <- Select input
    lastName  <- Select input
    return (firstName ++ " " ++ lastName)

Here we're binding standard input non-deterministically (twice) as if it were an effectful list:

>>> runEffect $ every name >-> P.stdoutLn
Daniel<Enter>
Fischer<Enter>
Daniel Fischer
Wagner<Enter>
Daniel Wagner
quit<Enter>
Donald<Enter>
Stewart<Enter>
Donald Stewart
Duck<Enter>
Donald Duck
quit<Enter>
quit<Enter>
>>> 

Notice how this streams out values immediately as they are generated, rather than building up a large intermediate result and then printing all the values in one batch at the end.

ListT computations can be combined in more ways than Pipes, so try to program in ListT as much as possible and defer converting it to a Pipe as late as possible using loop.

You can combine ListT computations even if their inputs and outputs are completely different:

data In
    = InA A
    | InB B
    | InC C

data Out
    = OutD D
    | OutE E
    | OutF F

-- Independent computations

example1 :: A -> ListT IO D
example2 :: B -> ListT IO E
example3 :: C -> ListT IO F

-- Combined computation

total :: In -> ListT IO Out
total input = case input of
    InA a -> fmap OutD (example1 a)
    InB b -> fmap OutE (example2 b)
    InC c -> fmap OutF (example3 c)

Sometimes you have multiple computations that handle different inputs but the same output, in which case you don't need to unify their outputs:

-- Overlapping outputs

example1 :: A -> ListT IO Out
example2 :: B -> ListT IO Out
example3 :: C -> ListT IO Out

-- Combined computation

total :: In -> ListT IO Out
total input = case input of
    InA a -> example1 a
    InB b -> example2 b
    InC c -> example3 c

Other times you have multiple computations that handle the same input but produce different outputs. You can unify their outputs using the Monoid and Functor instances for ListT:

-- Overlapping inputs

example1 :: In -> ListT IO D
example2 :: In -> ListT IO E
example3 :: In -> ListT IO F

-- Combined computation

total :: In -> ListT IO Out
total input =
       fmap OutD (example1 input)
    <> fmap OutE (example2 input)
    <> fmap OutF (example3 input)

You can also chain ListT computations, feeding the output of the first computation as the input to the next computation:

-- End-to-end

aToB :: A -> ListT IO B
bToC :: B -> ListT IO C

-- Combined computation

aToC :: A -> LIstT IO C
aToC = aToB >=> bToC

... or you can just use do notation if you prefer.

However, the Pipe type is more general than ListT and can represent things like termination. Therefore you should consider mixing Pipes with ListT when you need to take advantage of these extra features:

-- Mix ListT with Pipes

example :: In -> ListT IO Out

pipe :: Pipe In Out IO ()
pipe = Pipes.takeWhile (not . isC) >-> loop example
  where
    isC (InC _) = True
    isC  _      = False

So promote your ListT logic to a Pipe when you need to take advantage of these Pipe-specific features.
Tricks

pipes is more powerful than meets the eye so this section presents some non-obvious tricks you may find useful.

Many pipe combinators will work on unusual pipe types and the next few examples will use the cat pipe to demonstrate this.

For example, you can loop over the output of a Pipe using for, which is how map is defined:

map :: Monad m => (a -> b) -> Pipe a b m r
map f = for cat $ \x -> yield (f x)

-- Read this as: For all values flowing downstream, apply 'f'

This is equivalent to:

map f = forever $ do
    x <- await
    yield (f x)

You can also feed a Pipe input using (>~). This means we could have instead defined the yes pipe like this:

yes :: Monad m => Producer String m r
yes = return "y" >~ cat

-- Read this as: Keep feeding "y" downstream

This is equivalent to:

yes = forever $ yield "y"

You can also sequence two Pipes together. This is how drop is defined:

drop :: Monad m => Int -> Pipe a a m r
drop n = do
    replicateM_ n await
    cat

This is equivalent to:

drop n = do
    replicateM_ n await
    forever $ do
        x <- await
        yield x

You can even compose pipes inside of another pipe:

customerService :: Producer String IO ()
customerService = do
    each [ "Hello, how can I help you?"        -- Begin with a script
         , "Hold for one second."
         ]
    P.stdinLn >-> P.takeWhile (/= "Goodbye!")  -- Now continue with a human

Also, you can often use each in conjunction with (~>) to traverse nested data structures. For example, you can print all non-Nothing elements from a doubly-nested list:

>>> runEffect $ (each ~> each ~> each ~> lift . print) [[Just 1, Nothing], [Just 2, Just 3]]
1
2
3

Another neat thing to know is that every has a more general type:

 every :: (Monad m, Enumerable t) => t m a -> Producer a m ()

Enumerable generalizes Foldable and if you have an effectful container of your own that you want others to traverse using pipes, just have your container implement the toListT method of the Enumerable class:

class Enumerable t where
    toListT :: Monad m => t m a -> ListT m a

You can even use Enumerable to traverse effectful types that are not even proper containers, like MaybeT:

input :: MaybeT IO String
input = do
    str <- lift getLine
    guard (str /= "Fail")
    return str

>>> runEffect $ every input >-> P.stdoutLn
Test<Enter>
Test
>>> runEffect $ every input >-> P.stdoutLn
Fail<Enter>
>>> 

Conclusion

This tutorial covers the concepts of connecting, building, and reading pipes code. However, this library is only the core component in an ecosystem of streaming components. Derived libraries that build immediately upon pipes include:

    pipes-concurrency: Concurrent reactive programming and message passing
    pipes-parse: Minimal utilities for stream parsing
    pipes-safe: Resource management and exception safety for pipes
    pipes-group: Grouping streams in constant space

These libraries provide functionality specialized to common streaming domains. Additionally, there are several libraries on Hackage that provide even higher-level functionality, which you can find by searching under the "Pipes" category or by looking for packages with a pipes- prefix in their name. Current examples include:

    pipes-extras: Miscellaneous utilities
    pipes-network/pipes-network-tls: Networking
    pipes-zlib: Compression and decompression
    pipes-binary: Binary serialization
    pipes-attoparsec: High-performance parsing
    pipes-aeson: JSON serialization and deserialization

Even these derived packages still do not explore the full potential of pipes functionality, which actually permits bidirectional communication. Advanced pipes users can explore this library in greater detail by studying the documentation in the Pipes.Core module to learn about the symmetry of the underlying Proxy type and operators.

To learn more about pipes, ask questions, or follow pipes development, you can subscribe to the haskell-pipes mailing list at:

https://groups.google.com/forum/#!forum/haskell-pipes

... or you can mail the list directly at:

mailto:haskell-pipes@googlegroups.com

Additionally, for questions regarding types or type errors, you might find the following appendix on types very useful.
Appendix: Types

pipes uses parametric polymorphism (i.e. generics) to overload all operations. You've probably noticed this overloading already:

    yield works within both Producers and Pipes
    await works within both Consumers and Pipes
    (>->) connects Producers, Consumers, and Pipes in varying ways

This overloading is great when it works, but when connections fail they produce type errors that appear intimidating at first. This section explains the underlying types so that you can work through type errors intelligently.

Producers, Consumers, Pipes, and Effects are all special cases of a single underlying type: a Proxy. This overarching type permits fully bidirectional communication on both an upstream and downstream interface. You can think of it as having the following shape:

Proxy a' a b' b m r

Upstream | Downstream
    +---------+
    |         |
a' <==       <== b'  -- Information flowing upstream
    |         |
a  ==>       ==> b   -- Information flowing downstream
    |    |    |
    +----|----+
         v
         r

The four core types do not use the upstream flow of information. This means that the a' and b' in the above diagram go unused unless you use the more advanced features provided in Pipes.Core.

pipes uses type synonyms to hide unused inputs or outputs and clean up type signatures. These type synonyms come in two flavors:

    Concrete type synonyms that explicitly close unused inputs and outputs of the Proxy type
    Polymorphic type synonyms that don't explicitly close unused inputs or outputs

The concrete type synonyms use () to close unused inputs and X (the uninhabited type) to close unused outputs:

    Effect: explicitly closes both ends, forbidding awaits and yields

type Effect = Proxy X () () X

 Upstream | Downstream
    +---------+
    |         |
X  <==       <== ()
    |         |
() ==>       ==> X
    |    |    |
    +----|----+
         v
         r

    Producer: explicitly closes the upstream end, forbidding awaits

type Producer b = Proxy X () () b

Upstream | Downstream
    +---------+
    |         |
X  <==       <== ()
    |         |
() ==>       ==> b
    |    |    |
    +----|----+
         v
         r

    Consumer: explicitly closes the downstream end, forbidding yields

type Consumer a = Proxy () a () X

Upstream | Downstream
    +---------+
    |         |
() <==       <== ()
    |         |
a  ==>       ==> X
    |    |    |
    +----|----+
         v
         r

    Pipe: marks both ends open, allowing both awaits and yields

type Pipe a b = Proxy () a () b

Upstream | Downstream
    +---------+
    |         |
() <==       <== ()
    |         |
a  ==>       ==> b
    |    |    |
    +----|----+
         v
         r

When you compose Proxys using (>->) all you are doing is placing them side by side and fusing them laterally. For example, when you compose a Producer, Pipe, and a Consumer, you can think of information flowing like this:

       Producer                Pipe                 Consumer
    +-----------+          +----------+          +------------+
    |           |          |          |          |            |
X  <==         <==   ()   <==        <==   ()   <==          <== ()
    |  stdinLn  |          |  take 3  |          |  stdoutLn  |
() ==>         ==> String ==>        ==> String ==>          ==> X
    |     |     |          |    |     |          |      |     |
    +-----|-----+          +----|-----+          +------|-----+
          v                     v                       v
          ()                    ()                      ()

Composition fuses away the intermediate interfaces, leaving behind an Effect:

                   Effect
    +-----------------------------------+
    |                                   |
X  <==                                 <== ()
    |  stdinLn >-> take 3 >-> stdoutLn  |
() ==>                                 ==> X
    |                                   |
    +----------------|------------------+
                     v
                     ()

pipes also provides polymorphic type synonyms with apostrophes at the end of their names. These use universal quantification to leave open any unused input or output ends (which I mark using *):

    Producer': marks the upstream end unused but still open

type Producer' b m r = forall x' x . Proxy x' x () b m r

Upstream | Downstream
    +---------+
    |         |
 * <==       <== ()
    |         |
 * ==>       ==> b
    |    |    |
    +----|----+
         v
         r

    Consumer': marks the downstream end unused but still open

type Consumer' a m r = forall y' y . Proxy () a y' y m r

Upstream | Downstream
    +---------+
    |         |
() <==       <== * 
    |         |
a  ==>       ==> *
    |    |    |
    +----|----+
         v
         r

    Effect': marks both ends unused but still open

type Effect' m r = forall x' x y' y . Proxy x' x y' y m r

Upstream | Downstream
    +---------+
    |         |
 * <==       <== * 
    |         |
 * ==>       ==> *
    |    |    |
    +----|----+
         v
         r

Note that there is no polymorphic generalization of a Pipe.

Like before, if you compose a Producer', a Pipe, and a Consumer':

       Producer'               Pipe                 Consumer'
    +-----------+          +----------+          +------------+
    |           |          |          |          |            |
 * <==         <==   ()   <==        <==   ()   <==          <== *
    |  stdinLn  |          |  take 3  |          |  stdoutLn  |
 * ==>         ==> String ==>        ==> String ==>          ==> *
    |     |     |          |     |    |          |      |     |
    +-----|-----+          +-----|----+          +------|-----+
          v                      v                      v
          ()                     ()                     ()

... they fuse into an Effect':

                   Effect'
    +-----------------------------------+
    |                                   |
 * <==                                 <== *
    |  stdinLn >-> take 3 >-> stdoutLn  |
 * ==>                                 ==> *
    |                                   |
    +----------------|------------------+
                     v
                     ()

Polymorphic type synonyms come in handy when you want to keep the type as general as possible. For example, the type signature for yield uses Producer' to keep the type signature simple while still leaving the upstream input end open:

 yield :: Monad m => a -> Producer' a m ()

This type signature lets us use yield within a Pipe, too, because the Pipe type synonym is a special case of the polymorphic Producer' type synonym:

 type Producer' b m r = forall x' x . Proxy x' x () b m r
 type Pipe    a b m r =               Proxy () a () b m r

The same is true for await, which uses the polymorphic Consumer' type synonym:

 await :: Monad m => Consumer' a m a

We can use await within a Pipe because a Pipe is a special case of the polymorphic Consumer' type synonym:

 type Consumer' a   m r = forall y' y . Proxy () a y' y m r
 type Pipe      a b m r =               Proxy () a () b m r

However, polymorphic type synonyms cause problems in many other cases:

    They usually give the wrong behavior when used as the argument of a function (known as the "negative" or "contravariant" position) like this:

f :: Producer' a m r -> ...  -- Wrong

f :: Producer  a m r -> ...  -- Right

The former function only accepts polymorphic Producers as arguments. The latter function accepts both polymorphic and concrete Producers, which is probably what you want.

    Even when you desire a polymorphic argument, this induces a higher-ranked type, because it translates to a forall which you cannot factor out to the top-level to simplify the type signature:

f :: (forall x' x y' . Proxy x' x y' m r) -> ...

These kinds of type signatures require the RankNTypes extension.

    Even when you have polymorphic type synonyms as the result of a function (i.e. the "positive" or "covariant" position), recent versions of ghc such still require the RankNTypes extension. For example, the fromHandle function from Pipes.Prelude requires RankNTypes to compile correctly on ghc-7.6.3:

fromHandle :: MonadIO m => Handle -> Producer' String m ()

    You can't use polymorphic type synonyms inside other type constructors without the ImpredicativeTypes extension:

io :: IO (Producer' a m r)  -- Type error without ImpredicativeTypes

    You can't partially apply polymorphic type synonyms:

stack :: MaybeT (Producer' a m) r  -- Type error

In these scenarios you should fall back on the concrete type synonyms, which are better behaved. If concrete type synonyms are unsatisfactory, then ask ghc to infer the most general type signature and use that.

For the purposes of debugging type errors you can just remember that:

 Input --+    +-- Output
         |    |
         v    v
Proxy a' a b' b m r
      ^    ^
      |    |
      +----+-- Ignore these

For example, let's say that you try to run the stdinLn Producer. This produces the following type error:

>>> runEffect P.stdinLn
<interactive>:4:5:
    Couldn't match expected type `X' with actual type `String'
    Expected type: Effect m0 r0
      Actual type: Proxy X () () String IO ()
    In the first argument of `runEffect', namely `P.stdinLn'
    In the expression: runEffect P.stdinLn

runEffect expects an Effect, which is equivalent to the following type:

Effect          IO () = Proxy X () () X      IO ()

... but stdinLn type-checks as a Producer, which has the following type:

Producer String IO () = Proxy X () () String IO ()

The fourth type variable (the output) does not match. For an Effect this type variable should be closed (i.e. X), but stdinLn has a String output, thus the type error:

   Couldn't match expected type `X' with actual type `String'

Any time you get type errors like these you can work through them by expanding out the type synonyms and seeing which type variables do not match.

You may also consult this table of type synonyms to more easily compare them:

type Effect             = Proxy X  () () X
type Producer         b = Proxy X  () () b
type Consumer    a      = Proxy () a  () X
type Pipe        a    b = Proxy () a  () b

type Server        b' b = Proxy X  () b' b 
type Client   a' a      = Proxy a' a  () X

type Effect'            m r = forall x' x y' y . Proxy x' x y' y m r
type Producer'        b m r = forall x' x      . Proxy x' x () b m r
type Consumer'   a      m r = forall      y' y . Proxy () a y' y m r

type Server'       b' b m r = forall x' x      . Proxy x' x b' b m r
type Client'  a' a      m r = forall      y' y . Proxy a' a y' y m r

Appendix: Time Complexity

There are three functions that give quadratic time complexity when used in within pipes:

    sequence
    replicateM
    mapM

For example, the time complexity of this code segment scales quadratically with n:

import Control.Monad (replicateM)
import Pipes

quadratic :: Int -> Consumer a m [a]
quadratic n = replicateM n await

These three functions are generally bad practice to use, because all three of them correspond to "ListT done wrong", building a list in memory instead of streaming results.

However, sometimes situations arise where one deliberately intends to build a list in memory. The solution is to use the "codensity transformation" to transform the code to run with linear time complexity. This involves:

    wrapping the code in the Codensity monad transformer (from Control.Monad.Codensity module of the kan-extensions package) using lift
    applying sequence / replicateM / mapM
    unwrapping the code using lowerCodensity

To illustrate this, we'd transform the above example to:

import Control.Monad.Codensity (lowerCodensity)

linear :: Monad m => Int -> Consumer a m [a]
linear n = lowerCodensity $ replicateM n $ lift await

This will produce the exact same result, but in linear time.
Copyright

This tutorial is licensed under a Creative Commons Attribution 4.0 International License

Produced by Haddock version 2.18.1
-}
