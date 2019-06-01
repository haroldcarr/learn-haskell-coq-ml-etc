{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch07Spec where

import           Ch07_Untyped_Lambda_Calculus
import           Protolude
import           Test.Hspec

spec :: Spec
spec  = describe "ch07" $ do
  printAndIndexing
  termShiftTest
  termSubstTopTest
  evalTest

------------------------------------------------------------------------------

printAndIndexing :: Spec
printAndIndexing  = describe "printAndIndexing" $ do
  it "print" $
    printTm []  (TmApp (TmAbs "x" (TmVar "x" 0))
                       (TmAbs "y" (TmVar "y" 0)))
    `shouldBe`
    "((lambda x . x) (lambda y . y))"

  it "rmNames 1" $
    rmNames []   (TmApp (TmAbs "x" (TmVar "x" 9))
                       (TmAbs "y" (TmVar "y" 9)))
    `shouldBe`
    (Right  $     TmApp (TmAbs "x" (TmVar "x" 0))
                       (TmAbs "y" (TmVar "y" 0)))

  it "rmNames 2" $
    rmNames []   (TmAbs "a" (TmAbs "b" (TmAbs "c" (TmVar "a" 9))))
    `shouldBe`
    (Right  $     TmAbs "a" (TmAbs "b" (TmAbs "c" (TmVar "a" 2))))

  it "rmNames 3" $
    rmNames []   (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" 9)
                                                         (TmApp (TmVar "y" 9)
                                                                (TmVar "z" 9))))))
    `shouldBe`
    (Right  $     TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" 2)
                                                         (TmApp (TmVar "y" 1)
                                                                (TmVar "z" 0))))))

------------------------------------------------------------------------------

termShiftTest :: Spec
termShiftTest  = describe "termShift" $ do
  it "termShift 1" $
    termShift 2  (TmApp (TmAbs "x" (TmVar "x" 1))
                        (TmAbs "y" (TmVar "y" 1)))
    `shouldBe`
                  TmApp (TmAbs "x" (TmVar "x" 3))
                        (TmAbs "y" (TmVar "y" 3))

  it "termShift 2" $
    termShift 2  (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "a" 10)
                                                         (TmApp (TmVar "b" 12)
                                                                (TmVar "x"  2))))))
    `shouldBe`
                  TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "a" 12)
                                                         (TmApp (TmVar "b" 14)
                                                                (TmVar "x"  2)))))

termSubstTopTest :: Spec
termSubstTopTest  = describe "termSubstTopTest" $ do
  it "termSubstTop 1" $
    -- eval1 given (TmApp (TmAbs "x" (TmVar "x" 0))
    --                    (TmAbs "z" (TmVar "z" 0)))
    -- calls:
    termSubstTop (TmAbs "z" (TmVar "z" 0))
                 (TmVar "x" 0)
    `shouldBe`    TmAbs "z" (TmVar "z" 0)

  it "termSubstTop 2" $
    -- eval1 given (TmApp (TmAbs "x" (TmAbs "a" (TmVar "x" 1)))
    --                    (TmAbs "z" (TmVar "z" 0)))
    -- calls:
    termSubstTop (TmAbs "z" (TmVar "z" 0))
                 (TmAbs "a" (TmVar "x" 1))
    `shouldBe`    TmAbs "a" (TmAbs "z" (TmVar "z" 0))

------------------------------------------------------------------------------

evalTest :: Spec
evalTest  = describe "eval" $ do
  it "eval1 1" $
    eval1    (TmApp (TmAbs "x" (TmVar "x" 0))  -- same a termSubstTop 1 above
                    (TmAbs "z" (TmVar "z" 0)))
    `shouldBe`
    (Right $         TmAbs "z" (TmVar "z" 0))

  it "eval1 2" $
    eval1    (TmApp (TmAbs "x" (TmAbs "a" (TmVar "x" 1)))   -- same a termSubstTop 2 above
                               (TmAbs "z" (TmVar "z" 0)))
    `shouldBe`
    (Right $                    TmAbs "a" (TmAbs "z" (TmVar "z" 0)))

  it "eval1 3" $
    eval1    (TmApp (TmAbs "x" (TmVar "x" 0))
                    (TmApp (TmAbs "y" (TmVar "y" 0))
                           (TmAbs "z" (TmVar "z" 0))))
    `shouldBe`
    (Right $  TmApp (TmAbs "x" (TmVar "x" 0))  -- Note: it reduced inner application
                    (TmAbs "z" (TmVar "z" 0)))

  it "eval1 4" $
    eval1    (TmApp (TmAbs "x" (TmVar "x" 0))
                    (TmAbs "z" (TmVar "z" 0)))
    `shouldBe`
    (Right $         TmAbs "z" (TmVar "z" 0))


  it "eval1 5" $
    eval1           (TmAbs "z" (TmVar "z" 0))
    `shouldBe`
    Left Eval1NoMatch

  it "eval 1" $
    eval     (TmApp (TmAbs "x" (TmVar "x" 0))
                    (TmApp (TmAbs "y" (TmVar "y" 0))
                           (TmAbs "z" (TmVar "z" 0))))
    `shouldBe`
                     TmAbs "z" (TmVar "z" 0)

