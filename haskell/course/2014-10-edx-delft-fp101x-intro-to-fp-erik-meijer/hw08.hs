{-
Created       : 2014 Nov 25 (Tue) 18:20:43 by Harold Carr.
Last Modified : 2014 Dec 01 (Mon) 11:47:29 by Harold Carr.
-}

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- HOMEWORK 8

------------------------------------------------------------------------------
-- EXERCISE 2

putStr'    :: String -> IO ()
putStr' []  = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStrLn0,putStrLn1,putStrLn2,putStrLn4,putStrLn5 :: String -> IO ()
putStrLn0 [] = putChar '\n'
putStrLn0 xs = putStr' xs >> putStrLn0 ""
putStrLn1 [] = putChar '\n'
putStrLn1 xs = putStr' xs >> putChar '\n'
putStrLn2 [] = putChar '\n'
putStrLn2 xs = putStr' xs >>= \x ->  putChar '\n'
-- putStrLn3 [] = putChar '\n'
-- putStrLn3 xs = putStr' xs >> \x ->  putChar '\n'
putStrLn4 [] = putChar '\n'
putStrLn4 xs = putStr' xs >>  putStr' "\n"
putStrLn5 [] = putChar '\n'
putStrLn5 xs = putStr' xs >>  putStrLn5 "\n"
-- putStrLn6 [] = return ""
-- putStrLn6 xs = putStrLn6 xs >>  putStr' "\n"

{-
putStrLn   []
putStrLn   "hello world"
putStrLn0  []
putStrLn0  "hello world"
putStrLn1  []
putStrLn1  "hello world"
putStrLn2  []
putStrLn2  "hello world"
putStrLn4  []
putStrLn4  "hello world"
putStrLn5  []
putStrLn5  "hello world" -- bottom
-}

------------------------------------------------------------------------------
-- EXERCISE 5

interact5 :: (String -> String) -> IO ()
interact5 f = do input <- getLine
                 putStrLn (f input)

ir5 = interact5 reverse

sequence_1,sequence_3,sequence_4,sequence_6 :: Monad m => [m a] -> m ()

sequence_1 [] = return ()
sequence_1 (m : ms) = (foldl (>>) m ms) >> return ()

-- sequence_2 ms = foldl (>>) (return ()) ms

sequence_3 [] = return ()
sequence_3 (m : ms) = m >> sequence_3 ms

sequence_4 [] = return ()
sequence_4 (m : ms) = m >>= \_ ->  sequence_4 ms

-- sequence_5 ms = foldl (>>=) (return ()) ms

sequence_6 ms = foldr (>>) (return ()) ms

-- sequence_7 ms = foldl (>>) (return []) ms

-- sequence_1 [ir5, ir5, ir5]
-- sequence_3 [ir5, ir5, ir5]
-- sequence_4 [ir5, ir5, ir5]
-- sequence_6 [ir5, ir5, ir5]

------------------------------------------------------------------------------
-- EXERCISE 6

interact6 :: (String -> String) -> IO String
interact6 f = do input <- getLine
                 let fi = f input
                 putStrLn fi
                 return fi

ir6 = interact6 reverse

sequence0 :: Monad m => [m a] -> m [a]
sequence0 [] = return []
sequence0 (m : ms) = m >>= \a -> do as <- sequence0 ms
                                    return (a : as)
{-
sequence1 :: Monad m => [m a] -> m [a]
sequence1 ms = foldr func (return ()) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do x <- m
                    xs <- acc
                    return (x : xs)

sequence2 :: Monad m => [m a] -> m [a]
sequence2 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = m : acc

sequence3 :: Monad m => [m a] -> m [a]
sequence3 [] = return []
sequence3 (m : ms) return (a : as)
  where
    a <- m
    as <- sequence3 ms
-}
sequence4 :: Monad m => [m a] -> m [a]
sequence4 ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do x <- m
                    xs <- acc
                    return (x : xs)
{-
sequence5 :: Monad m => [m a] -> m [a]
sequence5 [] = return []
sequence5 (m : ms) = m >> \a -> do as <- sequence5 ms
                                   return (a : as)

sequence6 :: Monad m => [m a] -> m [a]
sequence6 [] = return []
sequence6 (m : ms) m >>= \a -> as <- sequence6 ms
                               return (a : as)
-}
sequence7 :: Monad m => [m a] -> m [a]
sequence7 [] = return []
sequence7 (m : ms) = do a <- m
                        as <- sequence7 ms
                        return (a : as)

-- sequence0 [ir6,ir6,ir6]
-- sequence4 [ir6,ir6,ir6]
-- sequence7 [ir6,ir6,ir6]

------------------------------------------------------------------------------
-- EXERCISE 7

i7 :: String -> IO Int
i7 s = do putStrLn s
          input <- getLine
          return (read input)

mapM0 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM0 f as = sequence7 (map f as)

mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM1 f [] = return []
mapM1 f (a : as) = f a >>= \b -> mapM1 f as >>= \bs -> return (b : bs)

-- mapM2 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM2 f as = sequence_6 (map f as)

-- mapM3 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM3 f [] = return []
-- mapM3 f (a : as) = f a >> \b -> mapM1 f as >> \bs -> return (b : bs)

-- mapM4 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM4 f [] = return []
-- mapM4 f (a : as) = do f a -> b

mapM5 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM5 f [] = return []
mapM5 f (a : as) = do b <- f a
                      bs <- mapM5 f as
                      return (b : bs)

mapM6 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM6 f [] = return []
mapM6 f (a : as) = f a >>= \b -> do bs <- mapM6 f as
                                    return (b : bs)

mapM7 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM7 f [] = return []
mapM7 f (a : as) = f a >>= \b -> do bs <- mapM7 f as
                                    return (bs ++ [b])

-- mapM0 i7 ["one","two"]
-- mapM1 i7 ["one","two"]
-- mapM5 i7 ["one","two"]
-- mapM6 i7 ["one","two"]
-- mapM7 i7 ["one","two"] -- backwards

------------------------------------------------------------------------------
-- EXERCISE 8

p8 :: String -> IO Bool
p8 a = do putStrLn a
          x <- getLine
          case x of
              "y" -> return True
              _   -> return False


filterM1 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM1 _ [] = return []
filterM1 p (x : xs) = do flag <- p x
                         ys <- filterM1 p xs
                         if flag then return (x : ys) else return ys

filterM3 :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM3 _ [] = return []
filterM3 p (x : xs) = do flag <- p x
                         ys <- filterM3 p xs
                         if flag then return ys else return (x : ys)

-- filterM1 p8 ["1","4","5"]
-- filterM3 p8 ["1","4","5"] -- right type, wrong answer

------------------------------------------------------------------------------
-- EXERCISE 9

foldl0        :: (a -> b ->   a) -> a -> [b] ->   a
foldl0    f z0 xs0 = lgo z0 xs0
  where
    lgo z []     = z
    lgo z (x:xs) = lgo (f z x) xs

foldLeftM     :: Monad m =>
                 (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f z0 xs0 = lgo z0 xs0
  where
    lgo z []     = return z
    lgo z (x:xs) = do a <- f z x
                      lgo a xs

{-
foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r

h :      []   ++ [h] =>     [h,h]
a :   [h,h]   ++ [a] =>   [a,h,h,a]
k : [a,h,h,a] ++ [k] => [k,a,h,h,a,k]

putChar prints in order given
iterations store backward and forward
so: "haskelllleksahhaskell"

but question ways "what is the result of evaluating expression"
- the result is IO ()
- the side effects are printing the string
-}
------------------------------------------------------------------------------
-- EXERCISE 10

foldr0     :: (a -> b ->   b) -> b -> [a] ->   b
foldr0     k z = go
  where
    go []     = z
    go (y:ys) = y `k` go ys

foldRightM :: Monad m =>
              (a -> b -> m b) -> b -> [a] -> m b
foldRightM k z = go
  where
    go []     = return z
    go (y:ys) = (go ys) >>= \b -> y `k` b

{-
foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r
-}

------------------------------------------------------------------------------
-- EXERCISE 11

liftM0 :: Monad m => (a -> b) -> m a -> m b
liftM0 f m = do x <- m
                return (f x)

-- liftM1 :: Monad m => (a -> b) -> m a -> m b
-- liftM1 f m = m >>= \a -> f a

liftM2 :: Monad m => (a -> b) -> m a -> m b
liftM2 f m = m >>= \a -> return (f a)

-- liftM3 :: Monad m => (a -> b) -> m a -> m b
-- liftM3 f m = return (f m)

liftM4 :: Monad m => (a -> b) -> m a -> m b
liftM4 f m = m >>= \a -> m >>= \b -> return (f a)

-- liftM6 :: Monad m => (a -> b) -> m a -> m b
-- liftM6 f m = mapM0 f [m]

-- liftM7 :: Monad m => (a -> b) -> m a -> m b
-- liftM7 f m = m >> \a -> return (f a)

-- liftM4 (\x -> (read x) ::Int ) (getLine)

e11 :: [Test]
e11 = U.tt "e11"
     [ liftM0 (*10) (Just 2)
     , liftM2 (*10) (Just 2)
     , liftM4 (*10) (Just 2) -- but does effects twice
     ]
     (Just 20)

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e11

-- End of file.


