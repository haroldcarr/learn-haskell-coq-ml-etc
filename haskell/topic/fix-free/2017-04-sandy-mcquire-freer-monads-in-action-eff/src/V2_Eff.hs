{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- http://reasonablypolymorphic.com/dont-eff-it-up/

module V2_Eff where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Prelude hiding             (log)
import           Test.HUnit                 (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util            as U (t)

------------------------------------------------------------------------------
-- custom version

data Bank a where
  GetCurrentBalance :: Bank Int
  PutCurrentBalance :: Int -> Bank ()

{-
CAN generate following explict functions via:
{-# LANGUAGE TemplateHaskell #-}

makeFreer ''Bank
-}

getCurrentBalance :: Member Bank r => Eff r Int
getCurrentBalance = send GetCurrentBalance

putCurrentBalance :: Member Bank r => Int -> Eff r ()
putCurrentBalance amount = send $ PutCurrentBalance amount

data Logger a where
  Log :: String -> Logger ()

log :: Member Logger r => String -> Eff r ()
log l = send $ Log l

withdraw0 :: (Member Bank r, Member Logger r) => Int -> Eff r (Maybe (Int,Int,Int))
withdraw0 desired = do
  balance <- getCurrentBalance
  if balance < desired then do
    log "not enough funds"
    return Nothing
  else do
    let newBalance = balance - desired
    putCurrentBalance newBalance
    return $ Just (balance, desired, newBalance)

-- IO interpretation

runBankIO :: Member IO r
          => Eff (Bank ': r) a
          -> Eff r a
runBankIO = runNat bank2io
  where
    bank2io :: Bank x -> IO x
    bank2io GetCurrentBalance            = fmap read (putStr "> " >> getLine)
    bank2io (PutCurrentBalance newValue) = print newValue

runLoggerIO :: Member IO r
            => Eff (Logger ': r) a
            -> Eff r a
runLoggerIO = runNat logger2io
  where
    logger2io :: Logger x -> IO x
    logger2io (Log s) = putStrLn s

runBankAndLoggerIO :: Eff '[Bank, Logger, IO] a -> IO a
runBankAndLoggerIO = runM . runLoggerIO . runBankIO

withdrawIO :: Int -> IO (Maybe (Int,Int,Int))
withdrawIO a = runBankAndLoggerIO $ withdraw0 a

-- test interpretation

testBank :: forall r a
          . Int
         -> Eff (Bank ': r) a
         -> Eff r a
testBank balance = handleRelayS balance (const pure) bind
  where
    bind :: forall x
          . Int
         -> Bank x
         -> (Int -> x -> Eff r a)
         -> Eff r a
    bind s GetCurrentBalance      cont = cont s  s
    bind _ (PutCurrentBalance s') cont = cont s' ()

ignoreLogger :: forall r a
              . Eff (Logger ': r) a
             -> Eff r a
ignoreLogger = handleRelay pure bind
  where
    bind :: forall x
          . Logger x
         -> (x -> Eff r a)
         -> Eff r a
    bind (Log _) cont = cont ()

runBankIgnoreLoggerTest :: Int -> Eff '[Bank, Logger] c -> c
runBankIgnoreLoggerTest initialBalance = run . ignoreLogger . testBank initialBalance

withdrawTest :: Int -> Int -> Maybe (Int, Int, Int)
withdrawTest i w = runBankIgnoreLoggerTest i $ withdraw0 w

twd1 = U.t "twd1" (withdrawTest 100 15) (Just (100,15,85))
twd2 = U.t "twd2" (withdrawTest   0 15) Nothing

------------------------------------------------------------------------------
-- replace Bank with State and Logger with Writer

withdraw :: ( Member (State  Int)    r
            , Member (Writer String) r
            )
         => Int
         -> Eff r (Maybe (Int,Int,Int))
withdraw desired = do
  balance :: Int <- get
  if balance < desired then do
    tell ("not enough funds"::String)
    return Nothing
  else do
    let newBalance = balance - desired
    put newBalance
    return $ Just (balance, desired, newBalance)

-- https://www.reddit.com/r/haskell/comments/3joxd7/freer_monads_more_extensible_effects/curyrp3/?st=j25a1ebj&sh=43a8ccb9
d :: Int -> Int -> ((Maybe (Int,Int,Int), String), Int)
d initialBalance w = run . flip runState initialBalance . runWriter $ withdraw w

td2 = U.t "td2" (d 100 15) ((Just (100,15,85),                ""), 85)
td1 = U.t "td1" (d 15 100) ((Nothing         ,"not enough funds"), 15)

------------------------------------------------------------------------------
-- interpreting effects in terms of one another.
{-
data Exc e a where
  ThrowError :: e -> Exc e a

makeFreer ''Exc

accumThenThrow :: ( Eq e
                  , Monoid e
                  , Member (Exc e) r
                  )
                => Eff (Writer e ': r) a
                -> Eff r a
accumThenThrow prog = do
  let (a, e) = pureWriter prog
  unless (e == mempty) $ throwError e
  return a

-- non-trivial transformations.

data SetOf s a where
  SetAdd      :: s -> SetOf s ()
  SetContains :: s -> SetOf s Bool

makeFreer ''SetOf

dedupWriter :: ( Member (SetOf  w) r
               , Member (Writer w) r
               )
            => Eff r a
            -> Eff r a
dedupWriter = interpose pure bind
  where
    bind (Tell w) cont = do
      alreadyTold <- setContains w
      unless alreadyTold $ do
        setAdd w
        tell w
      cont ()
-}
------------------------------------------------------------------------------

test :: IO Counts
test  =
  runTestTT $ TestList $ twd1 ++ twd2 ++ td1 ++ td2

