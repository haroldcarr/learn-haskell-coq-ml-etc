> module P2 where
>
> import           Test.HUnit            (Counts, Test (TestList), runTestTT)
> import qualified Test.HUnit.Util       as U (t)

https://iohk.io/blog/bidirectional-proof-refinement/

Bidirectional proof refinement
 MARCH 16, 2017     DARRYL MCADAMS

P1* : info flows in one direction: from root of proof tree to leaves.

how to get info to flow in opposite direction too

code : https://github.com/psygnisfive/bidirectional-proof-refinement

Addition Again

> data Nat = Zero | Suc Nat deriving (Eq, Show)

Previous addition judgment Plus was a three-argument judgment
- Plus L M N claims N is sum of L / M
- what if only some of the arg given to this judgment
- e.g., Prolog metavariables
- proof refinement : different approach
- use bidirectionality
  - specify judgment’s arg can come in two modes: input and output

Plus12: input mode for first two arguments to Plus. third is output
Plus13: output is second arg : Plus13 L N means L subtracted from N

> data Judgment = Plus12 Nat Nat | Plus13 Nat Nat deriving (Eq, Show)

Plus12 decomposer
- only 2 args
- expand a goal into subgoals (like before)
- synthesize result from results of subgoals
  - before : map from a goal to some new goals
  - now    : also map from some subresults to a result

> decomposePlus12 :: Nat -> Nat -> Maybe ([Judgment], [Nat] -> Nat)
> decomposePlus12  Zero   y = Just ([]          , const y)
> decomposePlus12 (Suc x) y = Just ([Plus12 x y], \[z] -> Suc z)

partial: for some inputs: no decomposition
- because first argument is larger than second

> decomposePlus13 :: Nat -> Nat -> Maybe ([Judgment], [Nat] -> Nat)
> decomposePlus13  Zero        z  = Just ([]          , const z)
> decomposePlus13 (Suc x) (Suc z) = Just ([Plus13 x z], \[x'] -> x')
> decomposePlus13      _       _  = Nothing

> decompose :: Judgment -> Maybe ([Judgment], [Nat] -> Nat)
> decompose (Plus12 x y) = decomposePlus12 x y
> decompose (Plus13 x z) = decomposePlus13 x z

Maybe not needed because can always decompose.  Nothing never used.
Here to be similar to previous system.

building proof tree
- similar to before
- now also need to use of synthesis functions in parallel with building proof tree

> data ProofTree = ProofTree Judgment [ProofTree] deriving (Eq, Show)
>
> findProof :: Judgment -> Maybe (ProofTree, Nat)
> findProof j = case decompose j of
>   Nothing -> Nothing
>   Just (js, f) -> case mapM findProof js of
>     Nothing  -> Nothing
>     Just tns ->
>       let (ts, ns) = unzip tns
>       in Just (ProofTree j ts, f ns)


Now get back
- proof tree
- resulting value (the Nat that would have been third arg of Plus in previous version)

> p13 :: [Test]
> p13 = U.t "p13"
>     (findProof (Plus13 (Suc Zero) (Suc (Suc (Suc Zero)))))
>     (Just
>       ( ProofTree
>           (Plus13
>             (Suc Zero)
>             (Suc (Suc (Suc Zero))))
>           [ ProofTree
>               (Plus13
>                 Zero
>                 (Suc (Suc Zero)))
>               []
>           ]
>       , Suc (Suc Zero))) -- result

> p13' :: [Test]
> p13' = U.t "p13'"
>     (findProof (Plus13 (Suc (Suc Zero)) (Suc Zero)))
>     Nothing -- can’t subtract a number from something smaller than it

Type Checking Again

do same thing with type checker as done with addition
- split HasType judgment into two judgments:
- Check
  - used in cases where type must be provided for proof to go through
- Synth
  - used when structure of program is enough to determine type

-- Haskell

data Judgment = Check [(String,Type)] Program Type
              | Synth [(String,Type)] Program
  deriving (Show)

Additionally, because of these changes in information flow, we’ll remove some type annotations from the various program forms, and instead introduce a general type annotation form that will be used to shift between judgments explicitly.


-- Haskell

data Program = Var String | Ann Program Type
             | Pair Program Program | Fst Program | Snd Program
             | Lam String Program | App Program Program
  deriving (Show)

The last change that we’ll make will be to change the notion of synthesis a little bit from what we had before. In the addition section, we said merely that we needed a function that synthesized a new value from some subvalues. More generally, though, we need that process to be able to fail, because we wish to place constraints on the synthesized values. To capture this, we’ll wrap the return type in Maybe.


-- Haskell

decomposeCheck
  :: [(String,Type)]
  -> Program
  -> Type
  -> Maybe ([Judgment], [Type] -> Maybe Type)
decomposeCheck g (Pair m n) (Prod a b) =
  Just ([Check g m a, Check g n b], \as -> Just undefined)
decomposeCheck g (Lam x m) (Arr a b) =
  Just ([Check ((x,a):g) m b], \as -> Just undefined)
decomposeCheck g m a =
  Just ( [Synth g m]
       , \[a2] -> if a == a2 then Just undefined else Nothing
       )

decomposeSynth
  :: [(String,Type)]
  -> Program
  -> Maybe ([Judgment], [Type] -> Maybe Type)
decomposeSynth g (Var x) =
  case lookup x g of
    Nothing -> Nothing
    Just a -> Just ([], \as -> Just a)
decomposeSynth g (Ann m a) =
  Just ([Check g m a], \as -> Just a)
decomposeSynth g (Fst p) =
  Just ( [Synth g p]
       , \[t] -> case t of
                   Prod a b -> Just a
                   _ -> Nothing
       )
decomposeSynth g (Snd p) =
  Just ( [Synth g p]
       , \[t] -> case t of
                   Prod a b -> Just b
                   _ -> Nothing
       )
decomposeSynth g (App f x) =
  Just ( [Synth g f, Synth g x]
       , \[s,t] -> case s of
                     Arr a b | a == t -> Just b
                     _ -> Nothing)
decomposeSynth _ _ = Nothing

decompose :: Judgment -> Maybe ([Judgment], [Type] -> Maybe Type)
decompose (Check g m a) = decomposeCheck g m a
decompose (Synth g m) = decomposeSynth g m

Finally, the findProof function has to be augmented slightly to deal with the new Maybe in the return type of the synthesizing function:

findProof :: Judgment -> Maybe (ProofTree, Type)
findProof j =
  case decompose j of
    Nothing -> Nothing
    Just (js,f) -> case sequence (map findProof js) of
      Nothing -> Nothing
      Just tsas ->
        let (ts,as) = unzip tsas
        in case f as of
             Nothing -> Nothing
             Just a -> Just (ProofTree j ts, a)

If we now try to find proofs for some typical synthesis and checking examples, we find what we expect:


findProof (Check [] (Lam "p" (Fst (Var "p"))) (Arr (Prod Nat Nat) Nat))

findProof (Check [] (Lam "p" (Fst (Var "p"))) (Arr Nat Nat))

findProof (Synth [("p",Prod Nat Nat)] (Fst (Var "p")))

Conclusion

limitations and drawbacks
- rule for App
  - has to synthesize type of both function and argument, then compare them
- means certain programs require additional type annotations to be written
  - But unnecessary, because when type of function is synthesied, can use that info to check arg

next post : how to do : combines both upward and downward flows of info

------------------------------------------------------------------------------

> testp2 :: IO Counts
> testp2  =
>     runTestTT $ TestList $ p13 ++ p13'
