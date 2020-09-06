{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

------------------------------------------------------------------------------
import           UseRWSIO
import           UseRWSTIO
import           UseRWST
------------------------------------------------------------------------------
import           Criterion.Main
------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ bgroup "RWS"
      [ bench "runMonadRWSInts" $ whnfIO (runMonadRWSInts False)
      , bench "runRWSTIOInts"   $ whnfIO (runRWSTIOInts   False)
      , bench "runRWSTInts"     $ whnfIO (runRWSTInts     False)
      , bench "runMonadRWSData" $ whnfIO (runMonadRWSData False)
      , bench "runRWSTData"     $ whnfIO (runRWSTData     False)
      ]
    ]
