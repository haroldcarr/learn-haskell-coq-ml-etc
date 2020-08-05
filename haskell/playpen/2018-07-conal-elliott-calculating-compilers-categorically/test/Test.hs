{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import X1 hiding ((++))
import Protolude
import Test.HUnit

main :: IO ()
main = void $ runTestTT $ TestList
  $  lemma1
  ++ simplifyId
  ++ simplifyLHS
  ++ simplifyRHS
  ++ simplifiedLHSSpecification
  ++ strengthenedLHSSpec
  ++ monidalLaw1
  ++ monidalLaw2
  ++ secondInTermsOfFirst
  ++ firstInTermsOfSecond
  ++ productInTermsOf
  ++ stackEvolves1
  ++ stackEvolves2
  ++ stackEvolves3

