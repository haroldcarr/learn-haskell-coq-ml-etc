{-# LANGUAGE MultiWayIf #-}

{-
Created       : 2015 Apr 21 (Tue) 17:59:08 by Harold Carr.
Last Modified : 2015 Apr 22 (Wed) 18:02:57 by Harold Carr.
-}

module Ch_09_9 where

import           Ch_08_10        hiding (eval, main)

import           Test.HUnit      as T
import           Test.HUnit.Util as U

-- 9.9 Exercises

------------------------------------------------------------------------------
-- 1

getLine1 :: IO String
getLine1 = do
    x <- getChar
    if x == '\n' then
        return []
    else
        do xs <- getLine1
           return (x:xs)

readLine1 :: IO String
readLine1 = do
    x <- getChar
    case x of
        '\n'   -> return []
        '\DEL' -> do putStr "\ESC[1D"
                     xs <- getLine1
                     return xs
        _      -> do xs <- getLine1
                     return (x:xs)

------------------------------------------------------------------------------
-- 2 CALCULATOR : show error (instead of just beep) : see modification to "eval"

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC["++ show y ++ ";" ++ show x ++ "H")

writeat :: Pos -> String -> IO ()
writeat p xs = do
    goto p
    putStr xs

seqn :: [IO a] -> IO ()
seqn [] = return ()
seqn (x:xs) = do x
                 seqn xs

box :: [String]
box = [ "+---------------+"
      , "|               |"
      , "+---+---+---+---+"
      , "| q | c | d | = |"
      , "| 1 | 2 | 3 | + |"
      , "| 4 | 5 | 6 | - |"
      , "| 7 | 8 | 9 | * |"
      , "| 0 | ( | ) | / |"
      , "+---------------+"
      ]

buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [ writeat (1,y) xs | (y, xs) <- zip [ 1 .. 13 ] box ]

display :: String -> IO ()
display xs = do writeat (3,2) "              "
                writeat (3,2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do display xs
             c <- getChar
             if elem c buttons
                 then process c xs
                 else do beep
                         calc xs

process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"    = quit
    | elem c "dD\BS\DEL" = delete xs
    | elem c "=\n"       = eval xs
    | elem c "cC"        = clear
    | otherwise          = press c xs

quit :: IO ()
quit = goto (1,14)

delete :: String -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval :: String -> IO ()
eval xs = case parse expr xs of
              [(n, "")] -> calc (show n)
              (x)       -> do beep
                              calc $ "NO:" ++ show (head x)

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

------------------------------------------------------------------------------
-- 3 : GAME OF LIFE : modify to only redisply positions that change : see life' and friends

height, width :: Int
height = 5
width = 5

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

showcells :: Board -> IO ()
showcells b = seqn [ writeat p "O" | p <- b ]

showchangedcells :: Board -> Board -> IO ()
showchangedcells nb b = seqn [ maybeWriteAt nb b (x,y) | x <- [ 1 .. width ],
                                                         y <- [ 1 .. height ] ]

maybeWriteAt :: Board -> Board -> Pos -> IO ()
maybeWriteAt nb b p = do
    if |      isEmpty nb p  && not (isEmpty b p) -> writeat p " "
       | not (isEmpty nb p) &&     (isEmpty b p) -> writeat p "O"
       | otherwise                               -> return ()

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [ (x-1,y-1)
                         , (x  ,y-1)
                         , (x+1,y-1)
                         , (x-1,y  )
                         , (x+1,y  )
                         , (x-1,y+1)
                         , (x  ,y+1)
                         , (x+1,y+1)
                         ]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width)  + 1,
              ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [ p | p <- b, elem (liveneighbs b p) [2,3] ]

birthsInefficient, births :: Board -> [Pos]
birthsInefficient b = [ (x,y) | x <- [ 1 .. width ],
                                y <- [ 1 .. height ],
                                isEmpty b (x,y),
                                liveneighbs b (x,y) == 3 ]
births b = [ p | p <- rmdups (concat (map neighbs b)),
                 isEmpty b p,
                 liveneighbs b p == 3 ]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

initialize :: Board -> IO ()
initialize b = do cls
                  showcells b

nextgen :: Board -> IO Board
nextgen b = do let newboard = survivors b ++ births b
               initialize newboard
               return newboard

nextgen' b = do let newboard = survivors b ++ births b
                showchangedcells newboard b
                return newboard

lifeloop :: Board -> (Board -> IO Board) -> IO ()
lifeloop b next = do wait 500000
                     newboard <- next b
                     lifeloop newboard next

life :: Board -> IO ()
life b = do initialize b
            lifeloop b nextgen

life' :: Board -> IO ()
life' b = do initialize b
             lifeloop b nextgen'

wait :: Int -> IO ()
wait n = seqn [ return () | _ <- [ 1 .. n ] ]

-- try: life glider

------------------------------------------------------------------------------
-- 4 : TODO : make editor to interactively create/modify board

------------------------------------------------------------------------------
-- 5 : TODO : make graphics version of calc and life using graphics library (e.g., threepenny-gui)

------------------------------------------------------------------------------
-- 6 : TODO : Implement NIM game

-- End of file.
