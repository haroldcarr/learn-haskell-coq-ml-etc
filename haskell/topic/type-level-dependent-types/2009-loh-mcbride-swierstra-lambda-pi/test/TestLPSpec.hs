module TestLPSpec where

------------------------------------------------------------------------------
import           LambdaPi                      hiding (it)
------------------------------------------------------------------------------
import           Test.Hspec
import qualified Text.ParserCombinators.Parsec as P
------------------------------------------------------------------------------

-- TODO : why do lambdas need double backslashes here, but not interactively nor in files?

spec :: Spec
spec  = do
  evalST
  quoteST
  parseST
  handleST
  parseLP

------------------------------------------------------------------------------

id',const' :: CTerm
id'      = Lam (Inf (Bound 0))
const'   = Lam (Lam (Inf (Bound 1)))

tfree   :: String -> Type
tfree a  = TFree (Global a)
free    :: String -> CTerm
free x   = Inf (Free (Global x))

term1,term2 :: ITerm
term1    = Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"
term2    = Ann const' (Fun (Fun (tfree "b") (tfree "b"))
                           (Fun (tfree "a")
                                (Fun (tfree "b") (tfree "b"))))
           :@: id' :@: free "y"

env1,env2 :: [(Name, Info)]
env1     = [ (Global "y", HasType (tfree "a"))
           , (Global "a", HasKind Star) ]
env2     =   (Global "b", HasKind Star) : env1

test_eval1,test_eval2 :: CTerm
test_eval1= quote0 (iEval term1 ([],[]))
test_eval2= quote0 (iEval term2 ([],[]))

test_type1,test_type2 :: Either String Type
test_type1= iType0 env1 term1
test_type2= iType0 env2 term2

evalST :: Spec
evalST  = describe "evalST" $ do
  it "eval term1" $ test_eval1 `shouldBe` (Inf (Free (Global "y")))
  it "eval term2" $ test_eval2 `shouldBe` (Lam (Inf  (Bound  0)))
  it "type term1" $ test_type1 `shouldBe` (Right (TFree (Global "a")))
  it "type term2" $ test_type2 `shouldBe` (Right (Fun (TFree (Global "b")) (TFree (Global "b"))))

------------------------------------------------------------------------------

vconst :: Value
vconst  = VLam (\x -> VLam (\_y -> x))

{-
When quote moves underneath a binder, a temp name is created for the bound var,
via constructor Quote (during the quotation process to avoid name clashes).

If the bound var occurs in the body then give the de Bruijn index via
the number of binders passed between introducing and finding the Quote variable.

quote 0 (VLam (λx →     VLam (λy → x)))
=         Lam (quote 1 (VLam (λy →            vfree (Quote 0))))
=         Lam           (Lam (quote        2 (vfree (Quote 0))))
=         Lam           (Lam (neutralQuote 2 (NFree (Quote 0))))
=         Lam           (Lam (Bound 1))
-}
quoteST :: Spec
quoteST  = describe "quoteST" $
  it "quote0 const" $ quote0 vconst `shouldBe` Lam (Lam (Inf (Bound 1)))

------------------------------------------------------------------------------

parseST :: Spec
parseST  = describe "parseST" $ do
  stParse "putStrLn"
          "putStrLn \"x\""
          (PutStrLn "x")
  stParse "id"
          "let id = (\\x -> x) :: Nat -> Nat"
          (Let "id"
           (Ann
             (Lam (Inf (Bound 0)))
             (Fun (TFree (Global "Nat")) (TFree (Global "Nat")))))
  stParse "const"
          "let const = (\\x y -> x) :: Nat -> Nat -> Nat"
          (Let "const"
           (Ann
             (Lam (Lam (Inf (Bound 1))))
             (Fun
               (TFree (Global "Nat"))
               (Fun (TFree (Global "Nat")) (TFree (Global "Nat"))))))

------------------------------------------------------------------------------

handleST :: Spec
handleST  = describe "handleST" $ do
  it "define and apply 'id'" $
    stHandle [ "assume (a :: *) (y :: a)"
             , "let id = (\\x -> x) :: a -> a"
             , "id y" ]
    `shouldBe`
    ( [ APutStrLn "id :: a -> a"
      , APutStrLn "y :: a" ]
    , ( True
      , ""
      , [(Global "it",VNeutral (NFree (Global "y")))
        ,(Global "id",VLam id)]  -- N.B., 'id' (see Eq instance for Value)
      , [(Global "it",HasType (TFree (Global "a")))
        ,(Global "id",HasType (Fun (TFree (Global "a")) (TFree (Global "a"))))
        ,(Global "y",HasType (TFree (Global "a")))
        ,(Global "a",HasKind Star) ] ))
  it "define and apply 'const'" $
    stHandle [ "assume (a :: *) (y :: a)"
             , "assume (b :: *)"
             , "let const = (\\x y -> x) :: (b -> b) -> a -> b -> b"
             , "let id = (\\x -> x) :: b -> b"
             , "const id y" ]
    `shouldBe`
    ( [ APutStrLn "const :: b -> b -> a -> b -> b"
      , APutStrLn "id :: b -> b"
      , APutStrLn "\\x -> x :: b -> b" ]
    , ( True
      , ""
      , [(Global "it",    VLam id)
        ,(Global "id",    VLam id)
        ,(Global "const", VLam id)]
      , [(Global "it",HasType (Fun (TFree (Global "b")) (TFree (Global "b"))))
       , (Global "id",HasType (Fun (TFree (Global "b")) (TFree (Global "b"))))
       , (Global "const",HasType (Fun
                                   (Fun (TFree (Global "b")) (TFree (Global "b")))
                                   (Fun
                                     (TFree (Global "a"))
                                     (Fun (TFree (Global "b")) (TFree (Global "b"))))))
       , (Global "b",HasKind Star)
       , (Global "y",HasType (TFree (Global "a")))
       , (Global "a",HasKind Star) ] ))

------------------------------------------------------------------------------

parseLP :: Spec
parseLP  = describe "parseLP" $ do
  lpParse "putStrLn" "putStrLn \"x\""
          (PutStrLn "x")
  lpParse "id"       "let id = (\\a x -> x) :: forall (a :: *) . a -> a"
          (Let "id"
           (Ann_
             (Lam_ (Lam_ (Inf_ (Bound_ 0))))
             (Inf_ (Pi_
                     (Inf_ Star_)
                     (Inf_ (Pi_
                             (Inf_ (Bound_ 0))
                             (Inf_ (Bound_ 1))))))))
  lpParse "const"
          "let const = (\\a b x y -> x) :: forall (a :: *) (b :: *) . a -> b -> a"
          (Let "const"
           (Ann_
             (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (Bound_ 1))))))
             (Inf_ (Pi_
                    (Inf_ Star_)
                    (Inf_ (Pi_
                            (Inf_ Star_)
                            (Inf_ (Pi_
                                   (Inf_ (Bound_ 1))
                                   (Inf_ (Pi_
                                          (Inf_ (Bound_ 1))
                                          (Inf_ (Bound_ 3))))))))))))

------------------------------------------------------------------------------

stParse :: String -> String -> Stmt ITerm Info -> Spec
stParse testName s expected = it testName $
  P.parse (isparse st) "<interactive>" s `shouldBe` (Right expected)

lpParse :: String -> String -> Stmt ITerm_ CTerm_ -> Spec
lpParse testName s expected = it testName $
  P.parse (isparse lp) "<interactive>" s `shouldBe` (Right expected)

stHandle :: [String] -> ([Action], State Value Info)
stHandle sStmts = rec (True, [], [], []) stmts []
 where
  stmts = case P.parse (P.many (isparse st)) "<interactive>" (unlines sStmts) of
    Left  e  -> error ("stHandle: " ++ show e)
    Right ss -> ss
  rec state       []  as = (as, state)
  rec state (stmt:ss) as =
        let (actions, state') = handleStmtPure st state stmt
         in rec state' ss (as ++ actions)
