> module P1_B where
>
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

> data Nat = Zero | Suc Nat deriving (Eq, Show)

A Type Checker with Proof Refinement

define types
- natural numbers
- product types (pairs)
- arrow types (functions)

      <type> ::= "Nat" | <type> "×" <type> | <type> "→" <type>

> data Type    =  Nat  | Prod Type Type    | Arr Type Type deriving (Eq, Show)

specify program to be type checked

<program> ::= <name> | "〈" <program> "," <program> "〉"
            | "fst" <program> | "snd" <program>
            | "λ" <name> "." <program> | <program> <program>

> data Program
>   = Var   String
>   | ZeroP
>   | SucP  Program
>   | Pair  Program Program
>   | Fst   Type    Program
>   | Snd   Type    Program
>   | Lam   String  Program
>   | App   Type    Program Program
>   deriving (Eq, Show)

programs built with Fst, Snd, and App have more arguments than usual because we want to do only type checking,
so need to supply information that’s not present in the types when working in a bottom up fashion.
Avoid doing this later.

judgment HasType G M A
- G is a type context that assigns Types to variables (represented by a list of string-type pairs)
- M is some program
- A is some type

> data Judgment = HasType [(String,Type)] Program Type deriving (Eq, Show)

> decomposeHasType
>   :: [(String,Type)]
>   -> Program
>   -> Type
>   -> Maybe [Judgment]
> decomposeHasType g (Var x)           a    = case lookup x g of
>                                               Nothing -> Nothing
>                                               Just a2 -> if a == a2 then Just [] else Nothing
> decomposeHasType _  ZeroP       Nat       = Just []
> decomposeHasType g (SucP m)     Nat       = Just [HasType        g  m  Nat]
> decomposeHasType g (Pair m n)  (Prod a b) = Just [HasType        g  m       a   , HasType g n b]
> decomposeHasType g (Fst b p)         a    = Just [HasType        g  p (Prod a b)]
> decomposeHasType g (Snd a p)           b  = Just [HasType        g  p (Prod a b)]
> decomposeHasType g (Lam x m)   (Arr  a b) = Just [HasType ((x,a):g) m         b]
> decomposeHasType g (App a m n)         b  = Just [HasType        g  m (Arr  a b), HasType g n a]
> decomposeHasType _          _          _  = Nothing
>
> decompose :: Judgment -> Maybe [Judgment]
> decompose (HasType g m a) = decomposeHasType g m a

> data ProofTree = ProofTree Judgment [ProofTree] deriving (Eq, Show)
>
> findProof :: Judgment -> Maybe ProofTree
> findProof j = do
>   js <- decompose j
>   ts <- mapM findProof js
>   return (ProofTree j ts)

> an :: [Test]
> an = U.t "an"
>    (findProof (HasType [] (Lam "x" (Var "x")) (Arr Nat Nat)))
>    (Just
>      (ProofTree
>        (HasType [] (Lam "x" (Var "x")) (Arr Nat Nat))
>        [ProofTree
>          (HasType [("x",Nat)] (Var "x") Nat)
>          []
>        ]))
> pn :: [Test]
> pn = U.t "pn"
>    (findProof (HasType [] (Lam "x" (Var "x")) (Prod Nat Nat)))
>    Nothing

Conclusion

system is unidirectional: information flows from bottom to top, justifying expansions of goals into subtrees.

No information flows back down from subtrees to parent nodes.

next : add bidirectional information flow
- enables type checkinging judgments that return info used for determining if a node expansion is justified
- practical consequence : programs less cluttered with extraneous information

------------------------------------------------------------------------------

> testp1b :: IO Counts
> testp1b  =
>     runTestTT $ TestList $ an ++ pn

