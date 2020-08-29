{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults      #-}

{-# LANGUAGE BangPatterns #-}

------------------------------------------------------------------------------
import           Use
------------------------------------------------------------------------------
import           Control.Monad.State.Strict
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
    , bgroup "fib"
      [ bench "fib0" $ whnf fib0 10
      , bench "fib1" $ whnf fib1 10
      , bench "fib2" $ whnf fib2 10
      , bench "fib3" $ whnf fib3 10
      , bench "fib4" $ whnf fib4 10
      ]
    ]

fib0 n | n == 0    = 0
       | n == 1    = 1
       | otherwise = fib0 (n-1) + fib0 (n-2)

fib1 n0 = go n0 (0,1)
 where
  go !n (!a,!b) | n == 0    = a
                | otherwise = go (n-1) (b, a+b)

fib2 n = flip evalState (0,1) $ do
  forM_ [0..(n-1)] $ \_ -> do
    (a,b) <- get
    put (b,a+b)
  (a,_) <- get
  return a

fib3 n = fibs!!n
 where
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib4 = fst . fib4aux
 where
  fib4aux 0 = (1,1)
  fib4aux 1 = (1,2)
  fib4aux n | even n    = (a*a + b*b, c*c - a*a)
            | otherwise = (c*c - a*a, b*b + c*c)
   where
    (a,b) = fib4aux (n `div` 2 - 1)
    c     = a + b

