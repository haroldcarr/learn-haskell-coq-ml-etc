{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- http://reasonablypolymorphic.com/dont-eff-it-up/

module VEff where

import           Control.Monad.Freer
import           Control.Monad.Freer.State
import           Control.Monad.Freer.Writer
import           Prelude hiding             (log)
import           Test.HUnit                 (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util            as U (t, e)

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
  if balance < desired
    then do
      log "not enough funds"
      return Nothing
    else do
      let newBalance = balance - desired
      putCurrentBalance newBalance
      return $ Just (balance, desired, newBalance)

-- production interpreters

runLogger :: Member IO r
          => Eff (Logger ': r) a
          -> Eff r a
runLogger = runNat logger2io
  where
    logger2io :: Logger x -> IO x
    logger2io (Log s) = putStrLn s

runBank :: Member IO r
        => Eff (Bank ': r) a
        -> Eff r a
runBank = runNat bank2io
  where
    bank2io :: Bank x -> IO x
    bank2io GetCurrentBalance            = putStr "> " >> getLine >>= return . read
    bank2io (PutCurrentBalance newValue) = putStrLn $ show newValue

doit :: Eff '[Bank, Logger, IO] a -> IO a
doit = runM . runLogger . runBank

wd :: Int -> IO (Maybe (Int,Int,Int))
wd a = doit $ withdraw0 a

-- test interpreters

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

doitTest :: Int -> Eff '[Bank, Logger] c -> c
doitTest initialBalance = run . ignoreLogger . testBank initialBalance

wdTest :: Int -> Int -> Maybe (Int, Int, Int)
wdTest i w = doitTest i $ withdraw0 w

twd1 = U.t "twd1" (wdTest 100 15) (Just (100,15,85))
twd2 = U.t "twd2" (wdTest   0 15) Nothing

------------------------------------------------------------------------------
-- replace Bank with State and Logger with Writer

withdraw :: ( Member (State  Int)    r
            , Member (Writer String) r
            )
         => Int
         -> Eff r (Maybe (Int,Int,Int))
withdraw desired = do
  balance :: Int <- get
  if balance < desired
     then do
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

test :: IO Counts
test  =
  runTestTT $ TestList $ twd1 ++ twd2 ++ td1 ++ td2
