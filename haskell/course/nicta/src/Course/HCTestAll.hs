{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.HCTestAll where

import           Course.Applicative (testApplicative)
import           Course.Apply       (testApply)
import           Course.Bind        (testBind)
import           Course.Comonad     (testComonad)
import           Course.Functor     (testFunctor)
import           Course.List        (testList)
import           Course.Optional    (testOptional)
import           Course.State       (testState)
import           Course.StateT      (testStateT)

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
