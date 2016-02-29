{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module X_2_4_DeMorgan_Adequacy_Duality where

import qualified Test.HUnit                                   as U (Counts, Test (TestList),
                                                                    runTestTT)
import qualified Test.HUnit.Util                              as U (t, tt)
import           X_2_1_Prop_Syntax
import           X_2_2_Prop_Semantics
import           X_2_3_Prop_Validity_Satisfiability_Tautology

-- 2.2 De Morgan laws, adequacy and duality

tfat = U.t "tfat"
    (and (fmap (tautology . pr) [ "T <-> F -> F"
                                , "~p <-> p -> F"
                                , "p ^ q <-> (p -> q -> F) -> F"
                                , "p v q <-> (p -> F) -> q"
                                , "(p <-> q) <-> ((p -> q) -> (q -> p) -> F) -> F"
                                ]))
    True

------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tfat

-- end of file ---
