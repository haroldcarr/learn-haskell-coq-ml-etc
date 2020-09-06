{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------------------
import           Use
------------------------------------------------------------------------------
import           Criterion.Main
------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ bgroup "RWS"
      [ bench "top1" $ whnfIO (top1 False)
      , bench "top2" $ whnfIO (top2 False)
      , bench "top3" $ whnfIO (top3 False)
      ]
    ]
