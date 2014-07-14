{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.HCTestAll where

-- need for unsugared do below
import           Prelude            as P (return, (>>))

import           Course.Applicative (testApplicative)
import           Course.Apply       (testApply)
import           Course.Bind        (testBind)
import           Course.Comonad     (testComonad)
import           Course.Functor     (testFunctor)
import           Course.List        (testList)
import           Course.Optional    (testOptional)
import           Course.State       (testState)
import           Course.StateT      (testStateT)

import           System.IO
import qualified Test.HUnit         as T

runTests :: IO T.Counts
runTests = do
  testApplicative
  testApply
  testBind
  testComonad
  testFunctor
  testList
  testOptional
  testState
  testStateT

-- End of file.
