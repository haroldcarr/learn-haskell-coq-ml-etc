module TestLPSpec where

import           LambdaPi                      hiding (it)
------------------------------------------------------------------------------
import           Test.Hspec
import qualified Text.ParserCombinators.Parsec as P

lParse :: String -> String -> Stmt ITerm_ CTerm_ -> Spec
lParse testName s expected = it testName $
  P.parse (isparse lp) "<interactive>" s `shouldBe` (Right expected)

spec :: Spec
spec  = describe "parse LP" $ do
  lParse "putStrLn" "putStrLn \"x\""
         (PutStrLn "x")
  lParse "id"       "let id = (\\a x -> x) :: forall (a :: *) . a -> a"
         (Let "id" (Ann_
                     (Lam_ (Lam_ (Inf_ (Bound_ 0))))
                     (Inf_ (Pi_
                            (Inf_ Star_)
                            (Inf_ (Pi_
                                    (Inf_ (Bound_ 0))
                                    (Inf_ (Bound_ 1))))))))

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
