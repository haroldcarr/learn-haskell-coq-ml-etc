{-
Created       : 2014 May 01 (Thu) 21:59:04 by Harold Carr.
Last Modified : 2014 May 01 (Thu) 22:22:25 by Harold Carr.
-}

-- http://hackage.haskell.org/package/pipes-4.1.1/docs/Pipes-Tutorial.html

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

 -- echo.hs

loop :: Effect IO ()
-- more concise: loop = for stdinLn (lift . putStrLn)
loop = for stdinLn $ \str -> do  -- Read this like: "for str in stdinLn"
    lift $ putStrLn str          -- The body of the 'for' loop

-- for loops over stdinLn, replaces every yield in stdinLn with the body of the loop, printing each line
-- exactly equivalent to:
{-
 loop = do                      |  stdinLn = do
     eof <- lift isEOF          |      eof <- lift isEOF
     unless eof $ do            |      unless eof $ do
         str <- lift getLine    |          str <- lift getLine
         (lift . putStrLn) str  |          yield str
         loop                   |          stdinLn
You can think of yield as creating a hole and a for loop is one way to fill that hole.
-}

main :: IO ()
-- OR: main = runEffect $ for stdinLn (lift . putStrLn)
main = runEffect loop

-- End of file.
