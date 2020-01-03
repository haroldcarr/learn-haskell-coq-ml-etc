module Print where

------------------------------------------------------------------------------
import           PiTypes
import           STTypes
------------------------------------------------------------------------------
import qualified Text.PrettyPrint.HughesPJ as PP
------------------------------------------------------------------------------

tPrint :: Int -> Type -> PP.Doc
tPrint _ (TFree (Global s))  = PP.text s
tPrint p (Fun ty ty')        = parensIf (p > 0) (PP.sep [tPrint 0 ty <> PP.text " ->", PP.nest 2 (tPrint 0 ty')])
tPrint _ _                   = undefined

iPrint :: Int -> Int -> ITerm -> PP.Doc
iPrint p ii (Ann c ty)       = parensIf (p > 1) (cPrint 2 ii c <> PP.text " :: " <> tPrint 0 ty)
iPrint _ ii (Bound k)        = PP.text (vars !! (ii - k - 1))
iPrint _ _  (Free (Global s))= PP.text s
iPrint p ii (i :@: c)        = parensIf (p > 2) (PP.sep [iPrint 2 ii i, PP.nest 2 (cPrint 3 ii c)])
iPrint _  _ x                = PP.text ("[" ++ show x ++ "]")

cPrint :: Int -> Int -> CTerm -> PP.Doc
cPrint p ii (Inf i) = iPrint p ii i
cPrint p ii (Lam c) = parensIf (p > 0) (PP.text "\\" <> PP.text (vars !! ii) <> PP.text " -> " <> cPrint 0 (ii + 1) c)

vars :: [String]
vars = [ c : n | n <- "" : map show [(1::Int)..], c <- ['x','y','z'] ++ ['a'..'w'] ]

parensIf :: Bool -> PP.Doc -> PP.Doc
parensIf True  = PP.parens
parensIf False = id

print     :: CTerm -> String
print      = PP.render . cPrint 0 0
printType :: Type -> String
printType  = PP.render . tPrint 0

------------------------------------------------------------------------------

iPrint_ :: Int -> Int -> ITerm_ -> PP.Doc
iPrint_ p ii (Ann_ c ty)       = parensIf (p > 1) (cPrint_ 2 ii c <> PP.text " :: " <> cPrint_ 0 ii ty)
iPrint_ _  _ Star_             = PP.text "*"
iPrint_ p ii (Pi_ d (Inf_ (Pi_ d' r)))
                               = parensIf (p > 0) (nestedForall_ (ii + 2) [(ii + 1, d'), (ii, d)] r)
iPrint_ p ii (Pi_ d r)         = parensIf (p > 0) (PP.sep [PP.text "forall " <> PP.text (vars !! ii) <> PP.text " :: " <> cPrint_ 0 ii d <> PP.text " .", cPrint_ 0 (ii + 1) r])
iPrint_ _ ii (Bound_ k)        = PP.text (vars !! (ii - k - 1))
iPrint_ _  _ (Free_ (Global s))= PP.text s
iPrint_ p ii (i :$: c)         = parensIf (p > 2) (PP.sep [iPrint_ 2 ii i, PP.nest 2 (cPrint_ 3 ii c)])
iPrint_ _  _ Nat_              = PP.text "Nat"
iPrint_ p ii (NatElim_ m z s n)= iPrint_ p ii (Free_ (Global "natElim") :$: m :$: z :$: s :$: n)
iPrint_ p ii (Vec_ a n)        = iPrint_ p ii (Free_ (Global "Vec") :$: a :$: n)
iPrint_ p ii (VecElim_ a m mn mc n xs)
                               = iPrint_ p ii (Free_ (Global "vecElim") :$: a :$: m :$: mn :$: mc :$: n :$: xs)
iPrint_ p ii (Eq_ a x y)       = iPrint_ p ii (Free_ (Global "Eq") :$: a :$: x :$: y)
iPrint_ p ii (EqElim_ a m mr x y eq)
                                 = iPrint_ p ii (Free_ (Global "eqElim") :$: a :$: m :$: mr :$: x :$: y :$: eq)
iPrint_ p ii (Fin_ n)          = iPrint_ p ii (Free_ (Global "Fin") :$: n)
iPrint_ p ii (FinElim_ m mz ms n f)
                               = iPrint_ p ii (Free_ (Global "finElim") :$: m :$: mz :$: ms :$: n :$: f)
iPrint_ _  _ x                 = PP.text ("[" ++ show x ++ "]")

cPrint_ :: Int -> Int -> CTerm_ -> PP.Doc
cPrint_ p ii (Inf_ i)    = iPrint_ p ii i
cPrint_ p ii (Lam_ c)    = parensIf (p > 0) (PP.text "\\" <> PP.text (vars !! ii) <> PP.text " -> " <> cPrint_ 0 (ii + 1) c)
cPrint_ _ ii Zero_       = fromNat_ 0 ii Zero_     --  PP.text "Zero"
cPrint_ _ ii (Succ_ n)   = fromNat_ 0 ii (Succ_ n) --  iPrint_ p ii (Free_ (Global "Succ") :$: n)
cPrint_ p ii (Nil_ a)    = iPrint_ p ii (Free_ (Global "Nil") :$: a)
cPrint_ p ii (Cons_ a n x xs) =
                             iPrint_ p ii (Free_ (Global "Cons") :$: a :$: n :$: x :$: xs)
cPrint_ p ii (Refl_ a x) = iPrint_ p ii (Free_ (Global "Refl") :$: a :$: x)
cPrint_ p ii (FZero_ n)  = iPrint_ p ii (Free_ (Global "FZero") :$: n)
cPrint_ p ii (FSucc_ n f)= iPrint_ p ii (Free_ (Global "FSucc") :$: n :$: f)

fromNat_ :: Int -> Int -> CTerm_ -> PP.Doc
fromNat_ n  _ Zero_ = PP.int n
fromNat_ n ii (Succ_ k) = fromNat_ (n + 1) ii k
fromNat_ n ii t = parensIf True (PP.int n <> PP.text " + " <> cPrint_ 0 ii t)

nestedForall_ :: Int -> [(Int, CTerm_)] -> CTerm_ -> PP.Doc
nestedForall_ ii ds (Inf_ (Pi_ d r)) = nestedForall_ (ii + 1) ((ii, d) : ds) r
nestedForall_ ii ds x                = PP.sep [PP.text "forall " <> PP.sep [parensIf True (PP.text (vars !! n) <> PP.text " :: " <> cPrint_ 0 n d) | (n,d) <- reverse ds] <> PP.text " .", cPrint_ 0 ii x]
