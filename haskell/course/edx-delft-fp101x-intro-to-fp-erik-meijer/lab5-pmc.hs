module Lab5 where

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom _)   = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop       = "stop"

-- ===================================
-- Ex. 0
-- ===================================

actionU :: ((a -> Action) -> Action) -> Action
actionU  f = f (\_ -> Stop)

action :: Concurrent a -> Action
action (Concurrent f) = f (\_ -> Stop)

-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
stop = Concurrent (\_ -> Stop)

-- ===================================
-- Ex. 2
-- ===================================

atomU :: IO a -> ((a -> Action) -> Action)
atomU ioa =             \c -> Atom (ioa >>= \a -> return $ c a)

atom  :: IO a -> Concurrent a
atom  ioa = Concurrent (\c -> Atom (ioa >>= \a -> return $ c a))

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork f = Concurrent (\c -> Fork (action f) (c ()))

par :: Concurrent a -> Concurrent a -> Concurrent a
par (Concurrent f1) (Concurrent f2) = Concurrent (\c -> Fork (f1 c) (f2 c))

-- ===================================
-- Ex. 4
-- ===================================

cb ::       ((a -> Action) -> Action)  ->
      (a -> ((b -> Action) -> Action)) ->
            ((b -> Action) -> Action)
cb m f = \c -> m (\x -> (f x) c)

instance Monad Concurrent where
    (Concurrent m) >>= f = Concurrent $ \c -> m (\x -> let (Concurrent fx) = f x in fx c)
    return x = Concurrent (\c -> c x)

-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin xs0 =
    case xs0 of
        [] -> return ()
        ((Atom ioa)   : xs) -> ioa >>= \x -> roundRobin (xs++[x])
        ((Fork a1 a2) : xs) -> roundRobin (a1:a2:xs)
        (Stop         : xs) -> roundRobin xs

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331)
         loop $ genRandom 42
         atom (putStrLn "")

-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [ 1, 96, 36, 11, 42, 47,  9,  1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35,  6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83,  9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs

{-
-- Exercise 0
action (Concurrent (\a -> Stop))
=> stop

-- Exercise 1
:t action (Concurrent (\a -> Stop))
action (Concurrent (\a -> Stop)) :: Action

-- Exercise 2
action (Concurrent (\a -> Fork Stop $ Fork Stop Stop))
=> fork stop fork stop stop

-- Exercise 3
action (Concurrent (\a -> Atom $ putStr "Haskell"))
=>  Couldn't match type `()' with `Action'
    Expected type: IO Action
      Actual type: IO ()

-- Exercise 4
action (Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop))
=> atom

-- Exercise 5
:t Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop)
=> Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop) :: Concurrent a

-- Exercise 6
action stop
=> stop

-- Exercise 7
stop
=> No instance for (Show (Concurrent a0))
      arising from a use of `print'

-- Exercise 8
action . atom . putStrLn $ "Haskell"
=> atom

-- Exercise 9
action $ atom undefined
=> atom

-- Exercise 10
atom . putStrLn $ "Haskell"
=>   No instance for (Show (Concurrent ()))
      arising from a use of `print'

-- Exercise 11
action $ fork stop
=> fork stop stop

-- Exercise 12
action (fork (atom (putStr "Hacker")))
=> fork atom stop

-- Exercise 13
:t action (fork (atom (putStr "Hacker")))
=> action (fork (atom (putStr "Hacker"))) :: Action

-- Exercise 14
action (fork undefined)
=> fork *** Exception: Prelude.undefined

-- Exercise 15
action $ par stop stop
=> fork stop stop

-- Exercise 16
action (par (atom (putStr "think")) (atom (putStr "hack")))
=> fork atom atom

-- Exercise 17
action (par stop $ fork stop)
=> fork stop fork stop stop

-- Exercise 18
action $ par (atom $ putChar 'x') (fork stop)
=> fork atom fork stop stop

-- Exercise 19
action (stop >>= (\c -> stop))
=> stop

-- Exercise 20
action (atom (putStrLn "whatever...") >>= stop)
=>  Couldn't match expected type `() -> Concurrent a0'
                with actual type `Concurrent a1'
    In the second argument of `(>>=)', namely `stop'
    In the first argument of `action', namely

-- Exercise 21
stop >>= stop
=>  Couldn't match expected type `a0 -> Concurrent b0'
                with actual type `Concurrent a1'
    In the second argument of `(>>=)', namely `stop'

-- Exercise 22
:t stop >>= stop
=>  Couldn't match expected type `a0 -> Concurrent b0'
                with actual type `Concurrent a1'
    In the second argument of `(>>=)', namely `stop'

-- Exercise 23
action (fork stop >>= \_ -> fork stop)
=> fork stop fork stop stop

-- Exercise 24
run ex0
=> 183969836351184424447619541356283739

-- Exercise 25
run ex1
Haskell177173719217361422167291191835716587475
-}
