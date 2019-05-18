module XSpec where

import           HC1
import           HC2
import           Lib
import           Test.Hspec

spec :: Spec
spec = describe "X" $ do
  t1
  t1'
  t2
  t2'
  t3
  t4
  t5
  -----
  h1
  h1'
  h1''
  h2
  h2'
  h4
  h5
  h5'
  h5''
  h6
  -----
  h24
  h25
