> module P1_A where
>
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

IOHK | CARDANO BLOG
Proof refinement basics
MARCH 07, 2017     DARRYL MCADAMS
twitter: @psygnisfive on Twitter
freenode: augur (in #languagengine and #haskell)

discuss overall structure of a proof refinement system

use-cases : implementing
- automatic theorem provers
- proof assistants
- type checkers for programming languages

proof refinement methodology history: LCF, HOL, Nuprl, Coq
- but techniques never penetrated into mainstream programming language implementation

GitHub repo for this project : https://github.com/psygnisfive/proof-refinement-basics

Prologue

IOHK: design/impl Plutus language (pure FP, static types): scripting language for blockchain
- has formal type theoretic specification

recently built a new framework for implementing programming languages
- different from usual way
- has more direct connection to the formal specification
- makes some bugs easier to eliminate
  - metavariables and unification : unification-driven type inference :
    - necessary to propagate updates to the infor state of type checking algorithm
- features easier to program
  - error messages with info about where error occurs : both source and logical structure of program
    - nature of error, and possible solutions

Proofs and Proof Systems

proof : tree structure with labeled nodes
- valid trees defined by set of rules specifying how a node in a tree may relate to its child nodes
- labels are usually hypothetical judgments
- rules are inference rules

example rule: ok to have node labeled A∧B is true in a proof tree, IFF its child nodes are A is true and B is true

A true   B true
--------------- ^I
    A∧B true

justifies example:

--------- hyp(p)  ------------ hyp(p)
B∧A true          B∧A true
-------- ∧E2      ----------- ^E1
A true            B true
----------------------------- ∧I
           A∧B true
----------------------------- ⊃I(p)
        (B∧A)⊃(A∧B) true


When used for constructing a proof : use rule use in directional fashion
- start from conclusion — the parent node — that is what is needed to show
- work backwards to premises of rule — the child nodes — to justify conclusion

dynamic process
- building proof tree
- rule says : to build proof tree with root labeled A&B is true
  expand that node to have two child nodes A is true and B is true
  then try to build those trees

example : turn:


A∧B true
----------------- ⊃I(p)
(B∧A)⊃(A∧B) true

into:



A true    B true
---------------- ∧I
    A∧B true
---------------- ⊃I(p)
(B∧A)⊃(A∧B) true

important to distinguish between two kinds of nodes
- nodes that appear in finished proof trees as is
- nodes that must still be expanded before they can appear in a finished proof tree

In above trees : indicated by
- nodes in finished proof trees have a bar above them labeled by the name of rule that justified them
- nodes which must still be expanded had no bar : call them goals

Building a Proof Refinement System

system for
- equality (x == y) and
- addition (x + y = z) as relations among natural numbers

> data Nat = Zero | Suc Nat deriving (Eq, Show)
>
> data Judgment = Equal Nat Nat | Plus Nat Nat Nat deriving (Eq, Show)

way to decompose a statement such as Plus (Suc Zero) (Suc Zero) Zero into subproblems that would justify it or fail

TODO: abstract over choice of Maybe vs List, provided type operator is monad

> decomposeEqual :: Nat -> Nat -> [[Judgment]]
> decomposeEqual  Zero    Zero   = [[]]
> decomposeEqual (Suc x) (Suc y) = [[Equal x y]]
> decomposeEqual      _       _  = []
>
> decomposePlus :: Nat -> Nat -> Nat -> [[Judgment]]
> decomposePlus  Zero   y      z  = [[Equal y z]]
> decomposePlus (Suc x) y (Suc z) = [[Plus x y z]]
> decomposePlus      _  _      _  = []

> dp :: [Test]
> dp = U.t "dp"
>    (decomposePlus (Suc Zero) (Suc Zero) (Suc (Suc (Suc Zero))))
>    [[Plus Zero (Suc Zero) (Suc (Suc Zero))]]

> decompose :: Judgment -> [[Judgment]]
> decompose (Equal x y)   = decomposeEqual x y
> decompose (Plus  x y z) = decomposePlus  x y z

what constitutes a proof tree


> --                         label for node : the conclusion of inference that justifies the node
> --                         |
> --                         |        sub-proofs that prove each of the premises of inference
> --                         v        v
> data ProofTree = ProofTree Judgment [ProofTree] deriving (Show)

                        ------------
                        zero == zero
                    ----------------------
                    suc(zero) == suc(zero)
                 ----------------------------
                 zero + suc(zero) = suc(zero)
            --------------------------------------
            suc(zero) + suc(zero) = suc(suc(zero))
       ------------------------------------------------
       suc(suc(zero)) + suc(zero) = suc(suc(suc(zero)))

represented by

> proofOfAddition :: ProofTree
> proofOfAddition =
>   ProofTree
>     (Plus (Suc (Suc Zero))
>           (Suc Zero)
>           (Suc (Suc (Suc Zero))))
>     [ ProofTree
>         (Plus (Suc Zero)
>               (Suc Zero)
>               (Suc (Suc Zero)))
>         [ ProofTree
>             (Plus Zero
>                   (Suc Zero)
>                   (Suc Zero))
>             [ ProofTree
>                 (Equal (Suc Zero) (Suc Zero))
>                 [ ProofTree
>                     (Equal Zero Zero)
>                     []
>                 ]
>             ]
>         ]
>     ]

try to find a proof for given judgment

> findProof :: Judgment -> [ProofTree]
> findProof j = do
>   js <- decompose j
>   ts <- mapM findProof js
>   return (ProofTree j ts)

------------------------------------------------------------------------------

> testp1a :: IO Counts
> testp1a  =
>     runTestTT $ TestList {- $ -} dp

