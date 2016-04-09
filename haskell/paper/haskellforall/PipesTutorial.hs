module PipesTutorial where

{-
Created       : 2014 May 01 (Thu) 21:59:04 by Harold Carr.
Last Modified : 2016 Apr 07 (Thu) 19:26:03 by Harold Carr.
-}

{-
-- https://hackage.haskell.org/package/pipes-4.1.8/docs/Pipes-Tutorial.html

Haskell stream programming forces choice of two of three features:
- Effects
  - sacrifice Effects : get pure/lazy lists
    - can transform using composable functions in constant space
    - but no interleaving effects
- Streaming
  - sacrifice Streaming : get mapM, forM and "ListT done wrong"
    - composable and effectful
    - no result until whole list processed / loaded into memory
- Composability
  - sacrifice Composability : get tightly coupled read/transform/write loop in IO
    - streaming and effectful
    - not modular nor separable

Pipes gives all three: effectful, streaming, composable programming.

Pipes provides many abstractions based on (all connectable because they share same underlying type):
- effectful Producers (like generators)
- effectful Consumers (like iteratees)
- effectful Pipes (like Unix pipes)
- ListT done right.
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

Producer : monad transformer that extends any base monad with a `yield` command.
`yield` sends output downstream to anonymous handler,
decoupling generating values from consuming them.

`yield` emits value, suspends Producer until value consumed.
If value not consumed, `yield` never returns.

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
        stdinLn              -- loop

------------------------------------------------------------------------------
{-
'for' : simple way to consume Producer

--                +-- Producer      +-- The body of the   +-- Result
--                |   to loop       |   loop              |
--                v   over          v                     v
--                --------------    ------------------    ----------
for :: Monad m => Producer a m r -> (a -> Effect m ()) -> Effect m r

(for producer body) loops over (producer), substituting each yield in (producer) with (body).

body takes one 'a' arg, same as output Producer (body gets input from Producer and nowhere else).

return value of Producer matches return value of result
(e.g., for must loop over entire Producer not skipping anything).

Above types are simplified from the real types.

-}

loop :: Effect IO ()
-- more concise: loop = for stdinLn (lift . putStrLn)
loop = for stdinLn $ \str -> -- Read this like: "for str in stdinLn"
    lift $ putStrLn str      -- The body of the 'for' loop

-- for loops over stdinLn, replaces every yield in stdinLn with body of loop:
-- equivalent to:
{-
 loop = do                      |  stdinLn = do
     eof <- lift isEOF          |      eof <- lift isEOF
     unless eof $ do            |      unless eof $ do
         str <- lift getLine    |          str <- lift getLine
         (lift . putStrLn) str  |          yield str
         loop                   |          stdinLn
Think of 'yield' as creating a hole, and 'for' loop as one way to fill hole.
-}

runStdinLoop :: IO ()
-- OR: main = runEffect $ for stdinLn (lift . putStrLn)
runStdinLoop = runEffect loop

-- 'for' with any Foldable: e.g., convert list to Producer using each:

runListLoop :: IO ()
runListLoop  = runEffect $ for (each [1..4::Int])     (lift . print)

runMaybeLoop :: IO ()
runMaybeLoop = runEffect $ for (each (Just (1::Int))) (lift . print)

-- End of file.
