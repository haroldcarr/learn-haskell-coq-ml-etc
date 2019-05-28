{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Ch06Spec where

import           Ch06_Nameless_Rep_of_Terms
import           Protolude
import           Test.Hspec

spec :: Spec
spec = describe "ch06" $ do

  it "rmNames 1" $
    rmNames [] (TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar "x" (-1))
                                                   (TmVar "y" (-1)))))
                      (TmAbs "x" (TmVar "x" (-1))))
    `shouldBe`
    (Right $    TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar "x"   1 )
                                                   (TmVar "y"   0 ))))
                      (TmAbs "x" (TmVar "x"   0 )))

  it "mkNames 1" $
    mkNames [] (TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar " "   1 )
                                                   (TmVar " "   0 ))))
                      (TmAbs "x" (TmVar "x"   0 )))
    `shouldBe`
    (Right $    TmApp (TmAbs "x" (TmAbs "y" (TmApp (TmVar "x"   1 )
                                                   (TmVar "y"   0 ))))
                      (TmAbs "x" (TmVar "x"   0 )))

  it "rmNames 2" $
    rmNames [] (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" (-1))
                                                       (TmApp (TmVar "y" (-1))
                                                              (TmVar "z" (-1)))))))
    `shouldBe`
    (Right $    TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x"   2 )
                                                       (TmApp (TmVar "y"   1 )
                                                              (TmVar "z"   0 ))))))

  it "mkName . rmNames 2" $
   (rmNames [] (TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x" (-1))
                                                       (TmApp (TmVar "y" (-1))
                                                              (TmVar "z" (-1)))))))
    >>=
    mkNames [])
    `shouldBe`
    (Right $    TmAbs "x" (TmAbs "y" (TmAbs "z" (TmApp (TmVar "x"   2 )
                                                       (TmApp (TmVar "y"   1 )
                                                              (TmVar "z"   0 ))))))

