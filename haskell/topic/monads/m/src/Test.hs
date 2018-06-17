module Test where

import           UseReader
import           UseReaderState
import           UseState
import           UseWriter

ta = testAll
testAll = do
  testReader
  testReaderState
  testState
  testWriter

