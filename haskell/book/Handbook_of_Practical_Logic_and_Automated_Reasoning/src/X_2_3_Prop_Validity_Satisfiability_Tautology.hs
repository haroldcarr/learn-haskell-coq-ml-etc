{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module X_2_3_Prop_Validity_Satisfiability_Tautology where

import           Data.Maybe           (fromMaybe)
import qualified Test.HUnit           as U (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util      as U (t, tt)
import           X_2_1_Prop_Syntax
import           X_2_2_Prop_Semantics

-- 2.3 Validity, satisfiability and tautology

{-
Valuation v *satisfies* a formula p if eval p v = true.  A formula is said to be:

- a tautology or logically valid if is satisfied by all valuations,
  - truth-table is ‘true’ in all rows
  - ‘always true’
  - like equation true for all values of vars : x2 − y2 = (x + y)(x − y)
- satisfiable if it is satisfied by some valuation(s)
  - truth-table is ‘true’ in at least one row;
  - ‘sometimes (but possibly not always) true’
  - like equation that has at least one solution : x2 + 2 = 3x
- unsatisfiable or a contradiction if no valuation satisfies it
  - truth-table is ‘false’ in all rows.
  - ‘always false’
  - like unsolvable equation : 0 * x = 1

tautology is also satisfiable

unsatisfiable if it is not satisfiable

in any valuation eval (¬p) v is false iff eval p v is true
- so p is a tautology if and only if ¬p is unsatisfiable.

-}

tTautlogy = U.tt "tTautlogy"
    (fmap snd (concat
               [ truthtable (pr "T")
               , truthtable (pr "p ^ q -> p v q")
               , truthtable (pr "((p -> q) -> p) -> p")]))
    "T"

tContradition = U.tt "tContradition"
    (fmap snd (concat
               [ truthtable (pr "F")
               , truthtable (pr "p ^ ~p")
               , truthtable (pr "~p ^ p")]))
    "F"

{-
extend (un)satisfiability to a set of formulas:
- set Γ of formulas is said to be satisfiable
  if there is a valuation v that simultaneously satisfies them all

Note the ‘simultaneously’
- {p ∧ ¬q, ¬p ∧ q} is unsatisfiable even though each formula by itself is satisfiable.

When the set concerned is finite, Γ = {p1 ,..., pn},
- satisfiability of Γ is equivalent to p1 ∧ ··· ∧ pn
- later will need to consider satisfiability of infinite sets of formulas, so that doesn't work

Notation Γ |= q : means ‘for all valuations in which all p ∈ Γ are true, q is true’.
- in case of finite Γ = {p1 ,..., pn}, equivalent to assertion p1 ∧ ··· ∧ pn ⇒ q is a tautology
- in the case Γ = emptySet, common to write |= p, meaning p is a tautology.
-}

-- Tautology and satisfiability checking

tautology fs =
    and $ onAllValuations (\v -> [eval fs v]) (const False) (atoms fs)

ttt = U.tt "ttt"
      [ tautology (pr "p v ~p")
      , tautology (pr "(p v q) ^ ~(p ^ q) -> (~p <-> q)")]
      True

ttf = U.tt "ttf"
      [ tautology (pr "p v q -> p")
      , tautology (pr "p v q -> q v (p <-> q)")]
      False

unsatisfiable :: Ord a => Formula a -> Bool
unsatisfiable = tautology . Not

satisfiable :: Ord a => Formula a -> Bool
satisfiable = not . unsatisfiable

-- Substitution

-- https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/.singlefun.html
(|=>) :: Eq a => a -> b -> a -> Maybe b
(x |=> y) z = if z == x then Just y else Nothing

-- https://www.cl.cam.ac.uk/~jrh13/hol-light/HTML/tryapplyd.html
tryApplyD :: (a -> Maybe b) -> a -> b -> b
tryApplyD f x d = fromMaybe d (f x)

psubst :: (a -> Maybe (Formula a)) -> Formula a -> Formula a
psubst f = onAtoms (\p -> tryApplyD f p (Atom p))
{-
tps1 = U.t "tps1"
    --      (Formula [Char] -> Maybe (Formula String)) -> Formula String
    (psubst ((Atom "p") |=> (pr "p ^ q"))                 (pr "p ^ q ^ p ^ q"))
    (Atom "c")
-}
------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tTautlogy ++ tContradition ++ ttt ++ ttf

-- end of file ---
