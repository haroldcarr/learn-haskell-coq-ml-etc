module Test where

import           Reader
import           State
import           Writer

testAll = do
  testReader
  testState
  testWriter

