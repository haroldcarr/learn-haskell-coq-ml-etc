module SimpleHCPN where

import System.IO
import System.Random
import Text.Show.Functions

type TransAct      marking = marking -> [] marking
data Trans    info marking = Trans {name::String
                                   ,info::info
                                   ,action::TransAct marking
                                   }
                                   deriving (Show)
data Net      info marking = Net {trans::[Trans info marking]}
                             deriving (Show)

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = [(x,xs)]++[(y,x:ys) | (y,ys) <- select xs]

enabled :: Net i m -> m -> [] ((String,i),m)
enabled net marking = [ ((n,i),m)
                      | (n,i,ms) <- [ (name t, info t, action t marking) 
                                    | t <- trans net 
                                    ] 
                      , m <- ms
                      ]

choose :: [a] -> IO a
choose enabledTs = do n <- getStdRandom (randomR (0,length enabledTs - 1)) 
                      return (enabledTs!!n)

run :: Show m => Net i m -> m -> IO ()
run net marking = 
  print marking >>
  if null enabledTs
    then putStrLn "no more enabled transitions!"
    else do trans <- choose enabledTs
            putStrLn (fst $ fst trans)
            run net (snd trans)
  where
    enabledTs = enabled net marking

runT :: Show m => String -> Net i m -> m -> IO ()
runT traceFile net marking = do
  t <- openFile traceFile WriteMode
  runWithTrace t net marking
  hClose t
  where 
    runWithTrace t net marking = do
      hPutStrLn t $ show marking 
      if null enabledTs
       then hPutStrLn t "no more enabled transitions!"
       else do trans <- choose enabledTs
               hPutStrLn t (fst $ fst trans)
               runWithTrace t net (snd trans)
      where
      enabledTs = enabled net marking

