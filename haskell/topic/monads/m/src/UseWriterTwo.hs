{-# LANGUAGE FlexibleContexts #-}

module UseWriterTwo where

import Control.Monad.Writer.Strict
import Prelude hiding (log)
import Test.HUnit as T
import Test.HUnit.Util as U

------------------------------------------------------------------------------
data Action = One | Two deriving (Eq, Show)

lAct :: (MonadWriter ([Action], [String])  m) => Action -> m ()
lAct x = tell ([x],  [])
lStr :: (MonadWriter ([Action], [String])  m) => String -> m ()
lStr x = tell ( [], [x])

use :: (MonadWriter ([Action], [String])  m) => m ()
use  = do lAct One; lStr "1"; lAct Two; lStr "2"; return ()

tu :: [Test]
tu  = U.t "tul" (runWriter use)  ((),([One,Two],["1","2"]))

testWriterTwo = runTestTT $ TestList {- $ -} tu
