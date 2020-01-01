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
  parseST
  evalST
  handleST
  parseLP

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
               (Fun
                 (TFree (Global "Nat"))
                 (TFree (Global "Nat"))))))

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

evalST :: Spec
evalST  = describe "evalST" $ do
  it "eval term1" $ test_eval1 `shouldBe` (Inf (Free (Global "y")))
  it "eval term2" $ test_eval2 `shouldBe` (Lam (Inf  (Bound  0)))
  it "type term1" $ test_type1 `shouldBe` (Right (TFree (Global "a")))
  it "type term2" $ test_type2 `shouldBe` (Right (Fun (TFree (Global "b")) (TFree (Global "b"))))

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
        ,(Global "id",HasType (Fun
                               (TFree (Global "a"))
                               (TFree (Global "a"))))
        ,(Global "y",HasType (TFree (Global "a")))
        ,(Global "a",HasKind Star) ] ))
  it "define and apply 'const'" $
    stHandle [ "assume (a :: *) (y :: a)"
             , "assume (b :: *)"
             , "let const = (\\x y -> x) :: (b -> b) -> a -> b -> b"
             , "let id = (\\x -> x) :: b -> b", "const id y" ]
    `shouldBe`
    ( [ APutStrLn "const :: b -> b -> a -> b -> b"
      , APutStrLn "id :: b -> b"
      , APutStrLn "\\x -> x :: b -> b" ]
    , ( True
      , ""
      , [(Global "it",    VLam id)
        ,(Global "id",    VLam id)
        ,(Global "const", VLam id)]
      , [(Global "it",HasType (Fun
                               (TFree (Global "b"))
                               (TFree (Global "b"))))
       , (Global "id",HasType (Fun
                               (TFree (Global "b"))
                               (TFree (Global "b"))))
       , (Global "const",HasType (Fun
                                   (Fun
                                     (TFree (Global "b"))
                                     (TFree (Global "b")))
                                   (Fun
                                     (TFree (Global "a"))
                                     (Fun
                                       (TFree (Global "b"))
                                       (TFree (Global "b"))))))
       , (Global "b",HasKind Star)
       , (Global "y",HasType (TFree (Global "a")))
       , (Global "a",HasKind Star) ] ))

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
