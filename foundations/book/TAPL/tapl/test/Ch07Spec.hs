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
  termSubstTest
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
  it "termShift 1" $ do
    -- all free
    termShift 2  (TmVar "y" 0) `shouldBe` TmVar "y" 2
    termShift 2  (TmVar "y" 1) `shouldBe` TmVar "y" 3
    termShift 2  (TmVar "y" 2) `shouldBe` TmVar "y" 4
    termShift 2  (TmVar "y" 3) `shouldBe` TmVar "y" 5

  it "termShift 2" $ do
    -- no free
    termShift 2  (TmAbs "y" (TmVar "y" 0)) `shouldBe` TmAbs "y" (TmVar "y" 0)
    -- free
    termShift 2  (TmAbs "y" (TmVar "a" 1)) `shouldBe` TmAbs "y" (TmVar "a" 3)
    -- no free
    termShift 2  (TmAbs "y" (TmAbs "z" (TmVar "y" 1))) `shouldBe`
                  TmAbs "y" (TmAbs "z" (TmVar "y" 1))
    -- free
    termShift 2  (TmAbs "y" (TmAbs "z" (TmVar "a" 2))) `shouldBe`
                  TmAbs "y" (TmAbs "z" (TmVar "a" 4))

  it "termShift 3" $ do
    -- no free vars to shift
    termShift 2  (TmApp (TmAbs "x" (TmVar "x" 0))
                        (TmAbs "y" (TmVar "y" 0))) `shouldBe`
                  TmApp (TmAbs "x" (TmVar "x" 0))
                        (TmAbs "y" (TmVar "y" 0))

    -- shift free vars
    termShift 2  (TmApp (TmAbs "a" (TmVar "x" 1))
                        (TmAbs "b" (TmVar "y" 2))) `shouldBe`
                  TmApp (TmAbs "a" (TmVar "x" 3))
                        (TmAbs "b" (TmVar "y" 4))

  it "termShift 4" $ do
    -- no free vars to shift
    termShift 2  (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" 2)
                                                         (TmApp (TmVar "y" 1)
                                                                (TmVar "z" 0)))))) `shouldBe`
                  TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" 2)
                                                         (TmApp (TmVar "y" 1)
                                                                (TmVar "z" 0)))))

    -- shift free vars (but not bound "x")
    termShift 2  (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "a" 4)
                                                         (TmApp (TmVar "b" 3)
                                                                (TmVar "x" 2)))))) `shouldBe`
                  TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "a" 6)
                                                         (TmApp (TmVar "b" 5)
                                                                (TmVar "x" 2)))))

termSubstTest :: Spec
termSubstTest  = describe "termSubstTest" $ do
  it "termSubstTest 1" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmVar "x" 0) `shouldBe`
                 TmVar "x" 0

  it "termSubstTest 2" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmVar "x" 1) `shouldBe`
                 TmVar "x" 1

  it "termSubstTest 3" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmVar "x" 2) `shouldBe`
                 TmAbs "NEW" (TmVar "NEW" 0)

  it "termSubstTest 4" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmApp (TmAbs "x" (TmVar "x" 0))   (TmAbs "z" (TmVar "z" 0))) `shouldBe`
                 TmApp (TmAbs "x" (TmVar "x" 0))   (TmAbs "z" (TmVar "z" 0))

  it "termSubstTest 5" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmApp (TmAbs "x" (TmVar "a" 1))   (TmAbs "z" (TmVar "b" 2))) `shouldBe`
                 TmApp (TmAbs "x" (TmVar "a" 1))   (TmAbs "z" (TmVar "b" 2))

  it "termSubstTest 6" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmApp (TmAbs "x" (TmVar "a" 2))   (TmAbs "z" (TmVar "b" 3))) `shouldBe`
                 TmApp (TmAbs "x" (TmVar "a" 2))   (TmAbs "z" (TmAbs "NEW" (TmVar "NEW" 0)))

  it "termSubstTest 7" $
    termSubst 2 (TmAbs "NEW" (TmVar "NEW" 0))
                (TmApp (TmAbs "x" (TmVar "a" 3))   (TmAbs "z" (TmVar "b" 4))) `shouldBe`
                 TmApp (TmAbs "x" (TmAbs "NEW" (TmVar "NEW" 0)))
                                                   (TmAbs "z" (TmVar "b" 4))


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
    (Right $  TmApp (TmAbs "x" (TmVar "x" 0))
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

  it "eval 2" $
    eval (TmApp (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" 2)
                                                        (TmApp (TmVar "y" 1)
                                                               (TmVar "z" 0))))))
                (TmAbs "a" (TmVar "a" 0)))
    `shouldBe`
                            TmAbs "y" (TmAbs "z" (TmApp (TmAbs "a" (TmVar "a" 0))
                                                        (TmApp (TmVar "y" 1)
                                                               (TmVar "z" 0))))
