{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module X_2_2_Prop_Semantics where

import           Data.List         (nub, sort)
import qualified Data.Map          as Map (fromList, lookup)
import           Data.Maybe        (fromJust)
import qualified Test.HUnit        as U (Counts, Test (TestList), runTestTT)
import qualified Test.HUnit.Util   as U (t, tt)
import           X_2_1_Prop_Syntax

-- 2.2

eval fm v = case fm of
    F          -> False
    T          -> True
    (Atom x)   -> v x
    (Not  p)   -> not (eval p v)
    (And  p q) ->      eval p v  && eval q v
    (Or   p q) ->      eval p v  || eval q v
    (Impl p q) -> not (eval p v) || eval q v
    (Iff  p q) ->      eval p v  == eval q v

te v fs = let pq x = fromJust (Map.lookup x (Map.fromList v))
          in fmap ((`eval` pq) . pr) fs

tev1 = U.tt "tev1"
       (te [("p",False),("q",False)]
           [ "p ^ q", "p v q", "~(p -> q)", "~(p <-> q)" ])
       False

tev2 = U.tt "tev2"
       (te [("p",False),("q",True)]
           [ "p ^ q", "~(p v q)", "~(p -> q)", "p <-> q"])
       False

tev3 = U.tt "tev3"
       (te [("p",True),("q",False)]
           [ "p ^ q", "~(p v q)", "p -> q", "p <-> q", "~p", "~(~q)"]) -- TODO ~~q
       False

tev4 = U.tt "tev4"
       (te [("p",True),("q",True)]
           [ "(p ^ q)", "p v q", "p -> q", "p <-> q"])
       True
{-
Theorem 2.1 For any prop formula p, the set atoms(p) is finite.
Proof By induction on structure of p.

- If p is T or F, then atoms(p) is the empty set: finite.
- If p is an atom, atoms(p) is a singleton set: finite.
- If p is of form ~q, then by the induction hypothesis, atoms(q) is finite
  and by definition atoms(¬q) = atoms(q).
- If p is of the form q ∧ r, q ∨ r, q ⇒ r or q ⇔ r, then atoms(p) = atoms(q) ∪ atoms(r).
  By inductive hypothesis, both atoms(q) and atoms(r) are finite
  and the union of two finite sets is finite. QED

Theorem 2.2 For any prop formula p, if two valuations υ and υ' agree on atoms(p)
(i.e. υ(x) = υ'(x) for all x in atoms(p)), then eval p υ = eval p υ'.
Proof By induction on structure of p.
- If p is of T or F, then it is independent of the valuation.
- If p is atom x, then atoms(x) = {x} and by assumption υ(x) = υ'(x).
  Hence eval p υ = υ(x) = υ'(x) = eval p υ'.
- If p is q ∧ r, q ∨ r, q ⇒ r or q ⇔ r, then atoms(p) = atoms(q) ∪ atoms(r).
  Since valuations agree on the union of the two sets, they agree, a fortiori, on each of atoms(q)
  and atoms(r).
  We can therefore apply the inductive hypothesis to conclude
  that eval q υ = eval q υ' and
  that eval r υ = eval r υ'.
  Since evaluation of p is a function of these subevaluations, eval p υ = eval p υ'. QED
-}
atoms :: (Eq a, Ord a, Foldable t) => t a -> [a]
atoms = sort . nub . foldr (:) []

ta1 = U.t "ta1"
      (atoms (pr "p ^ q v s -> ~p v (r <-> s)"))
      ["p","q","r","s"]

onAllValuations :: Eq t => ((t -> Bool) -> [a]) -> (t -> Bool) -> [t] -> [a]
onAllValuations f v as = case as of
    []     -> f v
    (p:ps) -> let v' t q = if q == p then t else v q
              in onAllValuations f (v' False) ps ++
                 onAllValuations f (v' True ) ps

truthtable fm =
    let ats        = atoms fm
        toString p = if p then "T" else "F"
        mkRow v    =
            let lis = fmap (toString . v) ats
                ans = toString (eval fm v)
                in [(lis,ans)]
    in onAllValuations mkRow (const False) ats

ttt1 = U.t "ttt1"
       (truthtable (pr "p ^ q -> q ^ r"))
       [ (["F","F","F"],"T")
       , (["F","F","T"],"T")
       , (["F","T","F"],"T")
       , (["F","T","T"],"T")
       , (["T","F","F"],"T")
       , (["T","F","T"],"T")
       , (["T","T","F"],"F")
       , (["T","T","T"],"T") ]

------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tev1 ++ tev2 ++ tev3 ++ tev4 ++
    ta1 ++
    ttt1

{-
Implication
p ⇒ q not like English reading ‘p implies q’ or ‘if p then q’.

truth-value of p ⇒ q in terms of truth-values of p and q

semantics is only reasonable one

intuitively : if p and p ⇒ q are true, then so is q
- consequently if p is true and q is false, then p ⇒ q must be false

Possible that p ∧ q ⇒ p is always true: semantics makes this true whatever truth-values of p and q

English : ‘p implies q’ or ‘if p then q’ mean a causal connection between p and q.

Does not seem reasonable to assert ‘p implies q’ just because not the case that p/true q/false

definition : ‘p implies q’ is true whenver q is true, regardless of p value
or any relation between p and q

Also ‘p implies q’ is true whenever p is false, regardless of q.
- ‘if the moon is made of cheese then 2 + 2 = 5’ is true

intuitive meaning of ‘if p, then q’ is not simply that we do not have p∧¬q,
but more strongly that we cannot under any circumstances have p ∧ ¬q.

‘whatever the value(s) taken by the component variables’

regard truth-functional conditional ‘p ⇒ q’ as distinct from everyday notions

BI-IMPLICATION: IF AND ONLY IF

‘if and only if ’ : implication in both directions
- ‘p if and only if q’ :
  - ‘p implies q &&
  -  q implies p’.

Prove IFF by proving ‘if p then q’ and ‘if q then p’
(like proving x = y by proving x ≤ y and y ≤ x).
-}

-- end of file ---
