module TestSpec where

------------------------------------------------------------------------------
import           Lib
------------------------------------------------------------------------------
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit
import           Test.HUnit               (Test (TestList))
------------------------------------------------------------------------------

spec :: Spec
spec  =
  fromHUnitTest $ TestList $
  ttypeOf1     ++ ttypeOf      ++
  treifyNatZ   ++ treifyNatSZ  ++ treifyNatSSZ ++
  tisUnitT     ++ tisUnitF     ++
  tSum         ++
  tflatten1    ++ tflatten2    ++
  tgnumFieldsL ++ tgnumFieldsR ++ tnumFieldsL  ++ tnumFieldsR



