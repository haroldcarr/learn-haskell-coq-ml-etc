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
    (Atom x)   -> fromJust (Map.lookup x v)
    (Not  p)   -> not (eval p v)
    (And  p q) ->      eval p v  && eval q v
    (Or   p q) ->      eval p v  || eval q v
    (Impl p q) -> not (eval p v) || eval q v
    (Iff  p q) ->      eval p v  == eval q v

te v fs = let pq = Map.fromList v
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

atoms :: (Eq a, Ord a, Foldable t) => t a -> [a]
atoms = sort . nub . foldr (:) []

ta1 = U.t "ta1"
      (atoms (pr "p ^ q v s -> ~p v (r <-> s)"))
      ["p","q","r","s"]

------------------------------------------------------------------------------
-- test

test :: IO U.Counts
test =
    U.runTestTT $ U.TestList $
    tev1 ++ tev2 ++ tev3 ++ tev4 ++
    ta1

-- end of file ---
