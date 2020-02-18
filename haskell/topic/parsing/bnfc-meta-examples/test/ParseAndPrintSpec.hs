{-# LANGUAGE QuasiQuotes #-}

module ParseAndPrintSpec where

------------------------------------------------------------------------------
import           HC.ParseAndPrint
------------------------------------------------------------------------------
import           Language.LBNF.Compiletime
import           Test.Hspec
------------------------------------------------------------------------------

spec :: Spec
spec  = do
  parseST
  printST
  deBruijnST

parseST :: Spec
parseST  = describe "parseST" $ do
  --------------------------------------------------
  tpGType "a"
    (Ok (GTFree (Ident "a")))
  tpGITerm "ann x :: a"
    (Ok (GAnn (GInf (GVar (Ident "x")))
              (GTFree (Ident "a"))))
  --------------------------------------------------
  tpGType "a -> b"
    (Ok (GFun (GTFree (Ident "a"))
              (GTFree (Ident "b"))))
  tpGITerm "ann \\x -> x :: a -> a"
    (Ok (GAnn (GLam (Ident "x") []
                    (GInf (GVar (Ident "x"))))
              (GFun (GTFree (Ident "a"))
                    (GTFree (Ident "a")))))
  --------------------------------------------------
  tpGType "a -> b -> c"
    (Ok (GFun (GTFree (Ident "a"))
              (GFun (GTFree (Ident "b"))
                    (GTFree (Ident "c")))))
  --------------------------------------------------
  tpGCTerm "x"
    (Ok (GInf (GVar (Ident "x"))))
  tpGITerm "x"
    (Ok       (GVar (Ident "x")))
  --------------------------------------------------
  tpGCTerm "\\x -> x"
    (Ok (GLam (Ident "x") []
              (GInf (GVar (Ident "x")))))
  tpGStm "let x = ann \\x   -> x :: a"
    (Ok (GLet (Ident "x")
              (GAnn (GLam (Ident "x") []
                          (GInf (GVar (Ident "x"))))
                    (GTFree (Ident "a")))))
  tpGStm "let x = ann \\x   -> x :: a -> a"
    (Ok (GLet (Ident "x")
              (GAnn (GLam (Ident "x") []
                          (GInf (GVar (Ident "x"))))
                    (GFun (GTFree (Ident "a")) (GTFree (Ident "a"))))))
  tpGStm "let x = ann \\x x -> x :: a -> a -> a"
    (Ok (GLet (Ident "x")
              (GAnn (GLam (Ident "x") [Ident "x"]
                          (GInf (GVar (Ident "x"))))
                    (GFun (GTFree (Ident "a"))
                          (GFun (GTFree (Ident "a")) (GTFree (Ident "a")))))))
  --------------------------------------------------
  tpGCTerm "\\x y -> (x y)"
    (Ok (GLam (Ident "x") [Ident "y"]
              (GInf (GAp (GVar (Ident "x")) (GInf (GVar (Ident "y")))))))
  tpGCTerm "\\x y z-> (x y)"
    (Ok (GLam (Ident "x") [Ident "y",Ident "z"]
              (GInf (GAp (GVar (Ident "x")) (GInf (GVar (Ident "y")))))))
  --------------------------------------------------
  tpGCTerm "(x x)"
    (Ok (GInf  (GAp (GVar (Ident "x")) (GInf (GVar (Ident "x"))))))
  tpGITerm "(x x)"
    (Ok        (GAp (GVar (Ident "x")) (GInf (GVar (Ident "x")))))
  tpGStm "(x z)"
    (Ok (GEval (GAp (GVar (Ident "x")) (GInf (GVar (Ident "z"))))))
  --------------------------------------------------
  tpGStm "((x y) z)"
    (Ok (GEval (GAp (GAp       (GVar (Ident "x"))
                         (GInf (GVar (Ident "y"))))
                    (GInf (GVar (Ident "z"))))))

------------------------------------------------------------------------------

printST :: Spec
printST  = describe "printST" $ do
  it "print \\x y z -> ((z y) x)" $
    printTree [gCTerm|\x y z -> ((z y) x)|] `shouldBe`
                   "\\ x y z -> ((z y)x)"
  it "print let x = ann \\x x -> x :: a -> a -> a" $
    printTree [gStm|let x = ann \x x -> x :: a -> a -> a|] `shouldBe`
                   "let x = ann \\ x x -> x :: a -> a -> a"

------------------------------------------------------------------------------

deBruijnST :: Spec
deBruijnST  = describe "deBruijnST" $
  tdbC "\\x y z-> (((z y) x) g)"
    (Lam (Lam (Lam (Inf (((Bound 0 :@: Inf (Bound 1)) :@: Inf (Bound 2)) :@: Inf (Free (Global "g")))))))

------------------------------------------------------------------------------

tpGStm :: String -> ParseMonad GStm -> Spec
tpGStm x expect = it ("pGStm " ++ x) $ pGStm (myLexer x) `shouldBe` expect

tpGITerm :: String -> ParseMonad GITerm -> Spec
tpGITerm x expect = it ("pGITerm " ++ x) $ pGITerm (myLexer x) `shouldBe` expect

tpGCTerm :: String -> ParseMonad GCTerm -> Spec
tpGCTerm x expect = it ("pGCTerm " ++ x) $ pGCTerm (myLexer x) `shouldBe` expect

tpGType :: String -> ParseMonad GType -> Spec
tpGType x expect = it ("pGType " ++ x) $ pGType (myLexer x) `shouldBe` expect

tdbC :: String -> CTerm -> Spec
tdbC x expect = it x $ case pGCTerm (myLexer x) of
  Ok ok -> deBruijnC [] ok `shouldBe` expect
  bad   -> show bad        `shouldBe` "WRONG"
