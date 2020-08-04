{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Lib
import Protolude
import Test.HUnit

main :: IO ()
main = void $ runTestTT $ TestList
  $  lemma1
  ++ simplifyId
  ++ simplifyLHS
  ++ simplifyRHS
  ++ simplifiedSpecification
  ++ strengthenedSpec
  ++ monidalLaw1
  ++ monidalLaw2
  ++ secondInTermsOfFirst
  ++ firstInTermsOfSecond
  ++ productInTermsOf
  ++ stackEvolves1
  ++ stackEvolves2
  ++ stackEvolves3

