{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module FM_EK_Teletype where

import           Control.Exception     (catch)
import           Control.Monad.Free    (Free, MonadFree, iterM, liftF)
import           Control.Monad.Free.TH (makeFree)
import           Control.Monad.Loops   (unfoldM)
import           Control.Monad.State
import qualified System.Exit           as SE hiding (ExitSuccess)
import           System.IO             (isEOF)
import           System.IO.Error       (ioeGetErrorString)

------------------------------------------------------------------------------
-- http://hackage.haskell.org/package/free-4.12.4/docs/examples/Teletype.lhs

-- 'k' stands for next action to execute

type Error = String

data TeletypeEK k
    = Halt                              -- Abort (ignore all following instructions)
    | NL                            k   -- Newline
    | Read        (Char  ->         k)  -- Get a character from the terminal
    | ReadOrEOF { onEOF  ::         k,
                  onChar :: Char -> k } -- GetChar if not end of file
    | ReadOrError (Error ->         k)
                  (Char  ->         k)  -- GetChar with error code
    | PutStrEK String               k   -- Write a message to the terminal
    deriving (Functor)

makeFree ''TeletypeEK

{-
generates

@
 halt        :: (MonadFree TeletypeEK m) => m a
 nL          :: (MonadFree TeletypeEK m) => m ()
 read        :: (MonadFree TeletypeEK m) => m Char
 readOrEOF   :: (MonadFree TeletypeEK m) => m (Maybe Char)
 readOrError :: (MonadFree TeletypeEK m) => m (Either Error Char)
 putStrEK    :: (MonadFree TeletypeEK m) => String -> m ()
@
-}

-- need instance of 'MonadFree TeletypeEK'
-- 'TeletypeEK' is a 'Functor', therefore:

type TeletypeM = Free TeletypeEK

runTeletypeIO :: TeletypeM a -> IO a
runTeletypeIO = iterM run
  where
    run :: TeletypeEK (IO a) -> IO a
    run Halt                   =          SE.exitSuccess
    run (Read               k) =          getChar      >>= k
    run (ReadOrEOF   eof    k) = isEOF >>= \b ->
                                     if b
                                     then eof
                                     else getChar      >>= k
    run (ReadOrError ferror k) =   catch (getChar      >>= k) (ferror . ioeGetErrorString)
    run (NL                 k) =          putChar '\n' >>  k
    run (PutStrEK    str    k) =          putStr  str  >>  k

readLine :: TeletypeM String
readLine = unfoldM $ mfilter (/= '\n') <$> readOrEOF

hello :: TeletypeM ()
hello = do
    putStrEK "Hello! What's your name?"; nL
    name <- readLine
    putStrEK "Nice to meet you, "; putStrEK name; putStrEK "."; nL
    halt

testEK :: IO ()
testEK = runTeletypeIO hello
