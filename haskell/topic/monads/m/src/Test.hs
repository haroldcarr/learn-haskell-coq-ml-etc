module Test where

import           Reader
import           State
import           Writer

ta = testAll
testAll = do
  testReader
  testState
  testWriter

