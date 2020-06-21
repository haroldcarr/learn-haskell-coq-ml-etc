{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module X_2_4_DeMorgan_Adequacy_Duality where

import           Data.Function                                (on)
import           Data.Maybe                                   (fromJust)
import qualified Test.HUnit                                   as U (Counts, Test (TestList),
                                                                    runTestTT)
import qualified Test.HUnit.Util                              as U (t)
import           X_2_1_Prop_Syntax
import           X_2_2_Prop_Semantics
import           X_2_3_Prop_Validity_Satisfiability_Tautology

-- 2.4 De Morgan laws, adequacy and duality

tfat = U.t "tfat"
    (and (fmap (tautology . pr) [ "T <-> F -> F"
                                , "~p <-> p -> F"
                                , "p ^ q <-> (p -> q -> F) -> F"
                                , "p v q <-> (p -> F) -> q"
                                , "(p <-> q) <-> ((p -> q) -> (q -> p) -> F) -> F"
                                ]))
    True

-- Duality

dual fs = case fs of
    F          -> return T
    T          -> return F
    (Atom _)   -> return fs
    (Not  p)   -> dual p >>= \p' -> return $ Not p'
    (And  p q) -> dd Or  p q
    (Or   p q) -> dd And p q
    _          -> Nothing
  where
    dd f p q = dual p >>= \p' -> dual q >>= \q' -> return $ f p' q'

dualFJ = fromJust . dual

td1 = U.t "td1"
    (pp (fromJust (dual (pr "p v ~p"))))
    "p ^ ~p"

exDeMorganEtc =
    [ ( "~(p  v  q)" ,   "~p ^ ~q"              )
    , ( "~(p  ^  q)" ,   "~p v ~q"              )
    , (   "p  v  q"  , "~(~p ^ ~q)"             )
    , (   "p  ^  q"  , "~(~p v ~q)"             )
    , (   "p <-> q"  , "~( p ^ ~q) ^ ~(~p ^ q)" ) -- NOTE: do not do DUAL on this
    ]

td2 = U.t "td2"
    (and (fmap (\(x,y) -> pr x == dualFJ (dualFJ (pr x)) &&
                          pr y == dualFJ (dualFJ (pr y))) (take 4 exDeMorganEtc)))
    True

logicallyEq :: Ord a => Formula a -> Formula a -> Bool
logicallyEq p q =
    let ttp = truthtable p
        ttq = truthtable q
    -- length ensures same number of propositional variables in each formula
    in length ttp == length ttq ||
       -- ensure same value for each valuation
       and (zipWith ((==) `on` snd) ttp ttq)

tleq = U.t "tleq"
    (and (fmap (\(x,y) -> (logicallyEq (pr x) (pr y))) exDeMorganEtc))
    True

corollary2_8_b = U.t "corollary2_8"
    (and (fmap (tautology . Not . fromJust . dual . pr) [ "a v ~a"
                                                        , "(a ^ b) v ~a v ~b"
                                                        ]))
    True

------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tfat ++ td1 ++ td2 ++ tleq ++ corollary2_8_b

-- end of file ---
