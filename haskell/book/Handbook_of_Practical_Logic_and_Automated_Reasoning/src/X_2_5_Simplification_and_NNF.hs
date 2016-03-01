{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module X_2_5_Simplification_and_NNF where

import qualified Test.HUnit                                   as U (Counts, Test (TestList),
                                                                    runTestTT)
import qualified Test.HUnit.Util                              as U (t)
import           X_2_1_Prop_Syntax
import           X_2_3_Prop_Validity_Satisfiability_Tautology hiding (tps1)

-- 2.5 Simplification and negation normal form

psimplify1 fs = case fs of
    Not F       -> T
    Not T       -> F
    Not (Not p) -> p
    And  _ F    -> F
    And  F _    -> F
    And  T p    -> p
    And  p T    -> p
    Or   _ T    -> T
    Or   T _    -> T
    Or   p F    -> p
    Or   F p    -> p
    Impl F _    -> T
    Impl _ T    -> T
    Impl T p    -> p
    Impl p F    -> Not p
    Iff  p T    -> p
    Iff  T p    -> p
    Iff  p F    -> Not p
    Iff  F p    -> Not p
    _           -> fs

psimplify fs = case fs of
    Not  p   -> psimplify1 (Not  (psimplify p))
    And  p q -> psimplify1 (And  (psimplify p) (psimplify q))
    Or   p q -> psimplify1 (Or   (psimplify p) (psimplify q))
    Impl p q -> psimplify1 (Impl (psimplify p) (psimplify q))
    Iff  p q -> psimplify1 (Iff  (psimplify p) (psimplify q))
    _        -> fs

tps1 = U.t "tps1"
    (pp (psimplify (pr "(T -> (x <-> F)) -> ~(y v F ^ z)")))
    "~x -> ~y"

tps2 = U.t "tps2"
    (pp (psimplify (pr "((x -> y) -> T) v ~F")))
    "T"

-- partial
isNegative (Not (Atom _)) = True
isNegative      (Atom _)  = False

isPositive = not . isNegative

-- partial
negate (Not a@(Atom _)) = a
negate      a@(Atom _)  = Not a

nnf = nnf' . psimplify
  where
    nnf' fs = case fs of
         And  p q       -> And (nnf'           p)  (nnf'      q)
         Or   p q       -> Or  (nnf'           p)  (nnf'      q)
         Impl p q       -> Or  (nnf'      (Not p)) (nnf'      q)
         Iff  p q       -> Or  (And (nnf'      p)  (nnf'      q))
                               (And (nnf' (Not p)) (nnf' (Not q)))
         Not (Not  p)   ->      nnf'           p
         Not (And  p q) -> Or  (nnf'      (Not p)) (nnf' (Not q))
         Not (Or   p q) -> And (nnf'      (Not p)) (nnf' (Not q))
         Not (Impl p q) -> And (nnf'           p)  (nnf' (Not q))
         Not (Iff  p q) -> Or  (And (nnf'      p)  (nnf' (Not q)))
                               (And (nnf' (Not p)) (nnf'      q))
         _              -> fs

tnnf1 = U.t "tnnf1"
    (let fm      = "(p <-> q) <-> ~(r -> s)"
         fmpr    = pr fm
         fmnnf   = nnf fmpr
         taut    = tautology (Iff fmpr fmnnf)
         fmnnfpp = pp fmnnf
     in (fm, fmnnfpp, taut))
    ( "(p <-> q) <-> ~(r -> s)"
    , "(((p ^ q) v (~p ^ ~q)) ^ r ^ ~s) v (((p ^ ~q) v (~p ^ q)) ^ (~r v s))"
    , True
    )

-- only pushes negation down to atoms (does not eliminate Iff)

nenf = nenf' . psimplify
  where
    nenf' fs = case fs of
        Not (Not  p)   ->      nenf'           p
        Not (And  p q) -> Or  (nenf'      (Not p)) (nenf' (Not q))
        Not (Or   p q) -> And (nenf'      (Not p)) (nenf' (Not q))
        Not (Impl p q) -> And (nenf'           p)  (nenf' (Not q))
        Not (Iff  p q) -> Iff (nenf'      p)       (nenf' (Not q)) -- different from nnf
        And  p q       -> And (nenf'           p)  (nenf'      q)
        Or   p q       -> Or  (nenf'           p)  (nenf'      q)
        Impl p q       -> Or  (nenf'      (Not p)) (nenf'      q)
        Iff  p q       -> Iff (nenf'           p)  (nenf'      q)  -- different from nnf
        _              -> fs

tnnf2 = U.t "tnnf2"
    (let fm      = "(p <-> q) <-> ~(r -> s)"
         fmpr    = pr fm
         fmnnf   = nenf fmpr
         taut    = tautology (Iff fmpr fmnnf)
         fmnnfpp = pp fmnnf
     in (fm, fmnnfpp, taut))
    ( "(p <-> q) <-> ~(r -> s)"
    ,  "p <-> q <-> r ^ ~s"
    , True
    )

tttt = U.t "tttt"
    (and (fmap (tautology . pr) [ "(p -> pp) ^ (q -> qq) -> (p ^ q -> pp ^ qq)"
                                , "(p -> pp) ^ (q -> qq) -> (p v q -> pp v qq)"
                                ]))
    True

------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tps1 ++ tps2 ++ tnnf1 ++ tnnf2 ++ tttt

-- end of file ---
