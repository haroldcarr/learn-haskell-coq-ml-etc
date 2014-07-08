{-
Created       : by Ruud Koot.
Last Modified : 2014 Jul 08 (Tue) 13:56:12 by Harold Carr.
-}

module Assignment1 where

import           Control.Arrow
import           Data.Char
import           Data.List
import           Data.Maybe

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (lines >>> exercise >>> unlines)

exercise :: [String] -> [String]
exercise = parseTable >>> select "gender" "male"
                      >>> project ["last", "first", "salary"] >>> printTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable = map words

-- | Printing

-- * Exercise 2

-- TODO: use foldr
printLine :: [Int] -> String
printLine [] = "+"
printLine (n:ns) = "+" ++ concat (replicate n "-") ++ printLine ns

-- * Exercise 3

printField :: Int -> String -> String
printField n s =
    let spaces = concat (replicate (n - length s) " ")
    in if all isDigit s
           then spaces ++ s
           else s ++ spaces

-- * Exercise 4
-- TODO : use foldr
printRow :: [(Int, String)] -> String
printRow [] = "|"
printRow ((n,s):xs) = "|" ++ printField n s ++ printRow xs

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths = map (foldr (\ x acc -> max (length x) acc) 0) . transpose

-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows) =
    let cw = columnWidths table
        h  = zip cw (map (map toUpper) header)
        d  = map (zip cw) rows
        l  = printLine cw
    in l :
       printRow h :
       l :
       map printRow d ++
       [l]

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows) =
    let i = fromMaybe (-1) (elemIndex column header)
    in if i == (-1)
           then table
           else header : filter (\r -> r !! i == value) rows

-- * Exercise 8
-- use !!, elemIndex, map, mapMaybe, transpose
project :: [Field] -> Table -> Table
project columns table@(header:_) =
    let cs = mapMaybe (`elemIndex` header) columns
        t1 = transpose table
    in transpose (map (\n -> t1 !! n) cs)

------------------------------------------------------------------------------

rawData :: [String]
rawData = [ "first last gender salary"
           , "Alice Allen female 82000"
           , "Bob Baker male 70000"
           , "Carol Clarke female 50000"
           , "Dan Davies male 45000"
           , "Eve Evans female 275000"
           ]

parsedData :: Table
parsedData = parseTable rawData

t0 :: T.Test
t0 = T.TestList
    [
      U.teq "t000" (parseTable rawData)
                   [ ["first","last","gender","salary"]
                   , ["Alice","Allen","female","82000"]
                   , ["Bob","Baker","male","70000"]
                   , ["Carol","Clarke","female","50000"]
                   , ["Dan","Davies","male","45000"]
                   , ["Eve","Evans","female","275000"]
                   ]
    , U.teq "t001" (printLine [5,6,6,6])
                   "+-----+------+------+------+"
    , U.teq "t002" (printField 6 "first")
                   "first "
    , U.teq "t003" (printField 6 "245")
                   "   245"
    , U.teq "t004" (printRow [(5, "Alice"),(6, "Allen"),(6, "female"),(6, "82000")])
                   "|Alice|Allen |female| 82000|"
    , U.teq "t005" (columnWidths parsedData)
                   [5,6,6,6]
    , U.teq "t006" (printTable   parsedData)
                   [ "+-----+------+------+------+"
                   , "|FIRST|LAST  |GENDER|SALARY|"
                   , "+-----+------+------+------+"
                   , "|Alice|Allen |female| 82000|"
                   , "|Bob  |Baker |male  | 70000|"
                   , "|Carol|Clarke|female| 50000|"
                   , "|Dan  |Davies|male  | 45000|"
                   , "|Eve  |Evans |female|275000|"
                   , "+-----+------+------+------+"
                   ]
    , U.teq "t007" (select "gender" "male" parsedData)
                   [ ["first","last","gender","salary"]
                   , ["Bob","Baker","male","70000"]
                   , ["Dan","Davies","male","45000"]
                   ]
    , U.teq "t008" (project ["last", "first", "salary"] parsedData)
                   [ ["last","first","salary"]
                   , ["Allen","Alice","82000"]
                   , ["Baker","Bob","70000"]
                   , ["Clarke","Carol","50000"]
                   , ["Davies","Dan","45000"]
                   , ["Evans","Eve","275000"]
                   ]
    ]

a1 :: IO T.Counts
a1 = T.runTestTT t0

-- End of file.
