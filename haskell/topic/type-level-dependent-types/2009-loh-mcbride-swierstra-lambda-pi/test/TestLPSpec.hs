module TestLPSpec where

------------------------------------------------------------------------------
import           LambdaPi                      hiding (it)
------------------------------------------------------------------------------
import           Test.Hspec
import qualified Text.ParserCombinators.Parsec as P
------------------------------------------------------------------------------

-- TODO : why do lambdas need double backslashes here, but not interactively nor in files?

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
{-
stHandle ["putStrLn \"x\""]
stHandle ["let id = (\\x -> x) :: a -> a"]
-}
spec :: Spec
spec  = do
  parseST
  parseLP

parseST :: Spec
parseST  = describe "parse ST" $ do
  stParse "putStrLn"
          "putStrLn \"x\""
          (PutStrLn "x")
  stParse "id"
          "let id = (\\x -> x) :: a -> a"
          (Let "id"
           (Ann
             (Lam (Inf (Bound 0)))
             (Fun (TFree (Global "a")) (TFree (Global "a")))))
  stParse "const"
          "let const = (\\x y -> x) :: a -> b -> a"
          (Let "const"
           (Ann
             (Lam (Lam (Inf (Bound 1))))
             (Fun (TFree (Global "a"))
               (Fun (TFree (Global "b"))
                 (TFree (Global "a"))))))

parseLP :: Spec
parseLP  = describe "parse LP" $ do
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

{-
let x = "let id    = (\\a x -> x) :: forall (a :: *) . a -> a"
let x = "let const = (\\a b x y -> x) :: forall (a :: *) (b :: *) . a -> b -> a"
parseIO "<interactive>" (isparse st)    x
compilePhrase st (True, [],   [],   []) x

parseIO "<interactive>" (isparse lp)    x
compilePhrase lp (True, [], lpve, lpte) x


parseTest (isparse st) "x"
parseTest (isparse st) x

-}
