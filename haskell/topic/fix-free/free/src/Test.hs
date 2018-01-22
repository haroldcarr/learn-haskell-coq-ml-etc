{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test where

import qualified FAM                                                            (test)
import qualified FM_DebasishG_2013_01_a_language_and_its_interpretation as FM_D (test)
import qualified FM_GG_2012_07_purify_code                              as FM_GG (test)
import qualified FM_MX_Redis                                            as FM_MX (test)
import qualified HA_Operational_Monad_Tutorial                          as HA_Op (test)

allTests = do
  FAM.test
  FM_D.test
  FM_GG.test
  FM_MX.test
  HA_Op.test
