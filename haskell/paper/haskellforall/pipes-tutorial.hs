{-
Created       : 2014 May 01 (Thu) 21:59:04 by Harold Carr.
Last Modified : 2014 May 01 (Thu) 22:22:25 by Harold Carr.
-}

{-
Pipes Quick Start : read Pipes.Prelude
  https://hackage.haskell.org/package/pipes-4.1.6/docs/Pipes-Prelude.html

This tutorial is more extensive and explains the pipes API in greater detail and illustrates several idioms.

-- https://hackage.haskell.org/package/pipes-4.1.6/docs/Pipes-Tutorial.html

Haskell stream programming forces a choice of two of the following three features:
- Effects
  - sacrifice Effects : get pure/lazy lists,
    - can transform using composable functions in constant space, but without interleaving effects.
- Streaming
  - sacrifice Streaming : get mapM, forM and "ListT done wrong",
    - are composable and effectful,
    - but do not return a single result until the whole list has first been processed
      and loaded into memory.
- Composability
  - sacrifice Composability : get tightly coupled read/transform/write loop in IO,
    - which is streaming and effectful, but is not modular or separable.

Pipes gives all three: effectful, streaming, composable programming.

Pipes provides many abstractions based on (all connectable because they share same underlying type):
- effectful Producers (like generators)
- effectful Consumers (like iteratees)
- effectful Pipes (like Unix pipes)
- ListT done right.

Pipes requires a basic understanding of monad transformers.  Learn via:
- "Monad Transformers - Step by Step"
  https://www.fpcomplete.com/user/haroldcarr/example-of-why-to-use-monads-what-they-can-do
- chapter 18 of "Real World Haskell"
  http://book.realworldhaskell.org/read/monad-transformers.html
- documentation of the transformers library.
  http://stackoverflow.com/questions/2769487/mtl-transformers-monads-fd-monadlib-and-the-paradox-of-choice
-}

------------------------------------------------------------------------------
{- INTRO

Connect streaming components that stream data in constant memory.

Components communicate using two commands:
- yield: Send output data
- await: Receive input data

Components built around yield/await:
- Producers yield values              : model streaming sources
- Consumers await values              : model streaming sinks
- Pipes both yield and await values   : model stream transformations
- Effects can neither yield nor await : model non-streaming components

Connect components via (4 ways parallel 4 type above):
- for handles yields
- (>~) handles awaits
- (>->) handles both yields and awaits
- (>>=) handles return values

Done connecting when you get an Effect: meaning all input/output has been handled.
Run this final Effect to begin streaming.
-}

------------------------------------------------------------------------------
{- PRODUCERS

Producers : effectful streams of input.

Producer is a monad transformer that extends any base monad with a `yield` command.
`yield` sends output downstream to an anonymous handler, decoupling generating
values from consuming them.

`yield` emits a value, then suspends the Producer until the value is consumed.
If value is not consumed then `yield` never returns.

-}
import           Control.Monad (unless)
import           Pipes
import           System.IO     (isEOF)

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
    eof <- lift isEOF        -- 'lift' an 'IO' action from the base monad
    unless eof $ do
        str <- lift getLine
        yield str            -- 'yield' the 'String'
        stdinLn              -- Loop


------------------------------------------------------------------------------
{-
-}

loop :: Effect IO ()
-- more concise: loop = for stdinLn (lift . putStrLn)
loop = for stdinLn $ \str -> do  -- Read this like: "for str in stdinLn"
    lift $ putStrLn str          -- The body of the 'for' loop

-- for loops over stdinLn, replaces every yield in stdinLn with the body of the loop, printing each line
-- equivalent to:
{-
 loop = do                      |  stdinLn = do
     eof <- lift isEOF          |      eof <- lift isEOF
     unless eof $ do            |      unless eof $ do
         str <- lift getLine    |          str <- lift getLine
         (lift . putStrLn) str  |          yield str
         loop                   |          stdinLn
You can think of yield as creating a hole and a for loop is one way to fill that hole.
-}

runStdinLoop :: IO ()
-- OR: main = runEffect $ for stdinLn (lift . putStrLn)
runStdinLoop = runEffect loop

-- use for to loop over lists (or any Foldable): via convert list to Producer using each:

runListLoop :: IO ()
runListLoop = runEffect $ for (each [1..4]) (lift . print)

-- End of file.
