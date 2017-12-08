{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HmfParserSpec where

import           Common
import           HMF
------------------------------------------------------------------------------
import           Control.Monad.Free
import           Control.Monad.State
import           Test.Hspec

spec :: Spec
spec = describe "HmfParserSpec" xxx

xxx = do
  it "a1" $
    pp "flushPage [11,22,33];"
    `shouldBe`
    Right ("",     ([[1,2,3], [4,5,6]], 1, 0, [[11,22,33]], []))
  it "a2" $
    pp "pageMisses;"
    `shouldBe`
    Right ("",     ([[4,5,6]], 0, 1, [], [[1,2,3]]))
  it "a3" $
    pp " flushPage [11,22,33] ;   pageMisses ; flushPage [111,222,333];\n"
    `shouldBe`
    Right ("",   ([[4,5,6]], 2, 1, [[111,222,333], [11,22,33]], [[1,2,3]]))
  it "a4" $
    pp "pageMisses;\nflushPage [111,222,333]"
    `shouldBe`
    Right ("",      ([[4,5,6]], 1, 1, [[111,222,333]], [[1,2,3]]))
  it "a5" $
    pp "pageMisses\nxxx"
    `shouldBe`
    Left "xxx [] endOfInput"
  it "a6" $
    pp "xxx"
    `shouldBe`
    Left "xxx [] string"
  it "a7" $
    pp " flushPage [11,22,33] ;   pageXisses ; flushPage [111,222,333];\n"
    `shouldBe`
    Left "pageXisses ; flushPage [111,222,333];\n [] endOfInput"

pp s = do
  (u, r) <- parseFully (parseHmf s)
  let a = xhmf r
  return (u, a)

--                         in/PM    nFP  nPM  out/FP   out/PM
analyzeHmf :: HmfCmd a -> ([[Int]], Int, Int, [[Int]], [[Int]])
analyzeHmf t = execState
        (a t) ( [[1,2,3],[4,5,6]] -- input to PM
              , 0                 -- num FP
              , 0                 -- num PM
              , []                -- output of FP
              , []                -- output of PM
              )
 where
  a = foldFree $ \case
    FlushPage ps next -> do
      (    ipm, nfp,   npm,    ofp,   opm ) <- get
      put (ipm, nfp+1, npm, ps:ofp,   opm )
      return next
    PageMisses next -> do
      (  i:ipm, nfp,   npm,    ofp,   opm ) <- get
      put (ipm, nfp,   npm+1,  ofp, i:opm )
      return  (next i)

xhmf :: Sig Ty HmfCmd -> ([[Int]], Int, Int, [[Int]], [[Int]])
xhmf (Sig _ f) = analyzeHmf f
