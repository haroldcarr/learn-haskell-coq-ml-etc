{-
Created       : 2014 Oct 26 (Sun) 09:22:44 by Harold Carr.
Last Modified : 2014 Nov 04 (Tue) 09:15:51 by Harold Carr.
-}

module Lab2 where

import           Data.List       (unfoldr)
import           Test.HUnit      as T
import           Test.HUnit.Util as U

------------------------------------------------------------------------------------------------------------------------------
-- Lab 2: Validating Credit Card Numbers
------------------------------------------------------------------------------------------------------------------------------

lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- ===================================
-- Ex. 0
-- ===================================

-- Note: toDigits n should error when n < 0.

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

eval :: Num a => [a] -> a
eval xs = foldl (\x y -> y + (10 * x)) 0 xs

e0 :: [Test]
e0 = U.tt "e0"
     [ and (map (\n -> eval (toDigits n) == n) creditcards)
     , and (map (\n -> all (\d -> d >= 0 && d < 10) (toDigits n)) creditcards)
     ]
     True

-- ===================================
-- Ex. 1
-- ===================================

toDigitsRev :: Integer -> [Integer]
toDigitsRev n0
    | n0 <  0   = error "negative numbers not allowed"
    | n0 == 0   = [0]  -- according to edx course
    | otherwise = unfoldr (\n -> if n <= 0 then Nothing else Just (lastDigit n, dropLastDigit n)) n0

evalRev :: Num b => [b] -> b
evalRev xs = foldr (\x y -> x + (10 * y)) 0 xs

e1 :: [Test]
e1 = U.tt "e1"
     [ and (map (\n -> evalRev (toDigitsRev n) == n) creditcards)
     , and (map (\n -> all (\d -> d >= 0 && d < 10) (toDigitsRev n)) creditcards)
     ]
     True

-- ===================================
-- Ex. 2
-- ===================================

doubleSecond :: [Integer] -> [Integer]
doubleSecond = snd . foldr (\n (b,xs) -> (not b, (if b then 2*n else n) : xs)) (False, [])

e2 :: T.Test
e2 = T.TestList
    [
      U.teq "ds8765"   (doubleSecond   [8,7,6,5]) [16,7,12,5]
    , U.teq "ds123"    (doubleSecond     [1,2,3])     [1,4,3]
      ----------------------
    , U.teq "ds1386"   (doubleSecond   [1,3,8,6])  [2,3,16,6]
    ]


-- ===================================
-- Ex. 3
-- ===================================

sumDigits :: [Integer] -> Integer
sumDigits = foldr (\x acc -> dropLastDigit x + lastDigit x + acc) 0

e3 :: T.Test
e3 = T.TestList
    [
      U.teq "0" (sumDigits  [16,7,12,5]) 22
    , U.teq "1" (sumDigits  [2,3,16,6])  18
    , U.teq "2" (sumDigits  [8,14,6,10]) 20
    , U.teq "3" (sumDigits [3,9,4,15,8]) 30
    ]

-- ===================================
-- Ex. 4
-- ===================================

isValid :: Integer -> Bool
isValid = (== 0) . (`mod` 10) . sumDigits . doubleSecond . toDigits

e4 :: T.Test
e4 = T.TestList
    [
      U.teq "vt" (isValid 4012888888881881) True
    , U.teq "vf" (isValid 4012888888881882) False
    ]


-- ===================================
-- Ex. 5
-- ===================================

numValid :: [Integer] -> Integer
numValid xs = sum . map (\_ -> 1) $ filter isValid xs


creditcards :: [Integer]
creditcards = [ 4716347184862961,
                4532899082537349,
                4485429517622493,
                4320635998241421,
                4929778869082405,
                5256283618614517,
                5507514403575522,
                5191806267524120,
                5396452857080331,
                5567798501168013,
                6011798764103720,
                6011970953092861,
                6011486447384806,
                6011337752144550,
                6011442159205994,
                4916188093226163,
                4916699537435624,
                4024607115319476,
                4556945538735693,
                4532818294886666,
                5349308918130507,
                5156469512589415,
                5210896944802939,
                5442782486960998,
                5385907818416901,
                6011920409800508,
                6011978316213975,
                6011221666280064,
                6011285399268094,
                6011111757787451,
                4024007106747875,
                4916148692391990,
                4916918116659358,
                4024007109091313,
                4716815014741522,
                5370975221279675,
                5586822747605880,
                5446122675080587,
                5361718970369004,
                5543878863367027,
                6011996932510178,
                6011475323876084,
                6011358905586117,
                6011672107152563,
                6011660634944997,
                4532917110736356,
                4485548499291791,
                4532098581822262,
                4018626753711468,
                4454290525773941,
                5593710059099297,
                5275213041261476,
                5244162726358685,
                5583726743957726,
                5108718020905086,
                6011887079002610,
                6011119104045333,
                6011296087222376,
                6011183539053619,
                6011067418196187,
                4532462702719400,
                4420029044272063,
                4716494048062261,
                4916853817750471,
                4327554795485824,
                5138477489321723,
                5452898762612993,
                5246310677063212,
                5211257116158320,
                5230793016257272,
                6011265295282522,
                6011034443437754,
                6011582769987164,
                6011821695998586,
                6011420220198992,
                4716625186530516,
                4485290399115271,
                4556449305907296,
                4532036228186543,
                4916950537496300,
                5188481717181072,
                5535021441100707,
                5331217916806887,
                5212754109160056,
                5580039541241472,
                6011450326200252,
                6011141461689343,
                6011886911067144,
                6011835735645726,
                6011063209139742,
                379517444387209,
                377250784667541,
                347171902952673,
                379852678889749,
                345449316207827,
                349968440887576,
                347727987370269,
                370147776002793,
                374465794689268,
                340860752032008,
                349569393937707,
                379610201376008,
                346590844560212,
                376638943222680,
                378753384029375,
                348159548355291,
                345714137642682,
                347556554119626,
                370919740116903,
                375059255910682,
                373129538038460,
                346734548488728,
                370697814213115,
                377968192654740,
                379127496780069,
                375213257576161,
                379055805946370,
                345835454524671,
                377851536227201,
                345763240913232
              ]

-- ===========================================================================

q0 :: [Test]
q0 = U.t "q0" (toDigits 12321) [1,2,3,2,1]

q1 :: [Integer]
q1 = toDigits (-531)

-- q2 :: [Integer]
-- q2 = toDigits 7.6

q3 :: [Test]
q3 = U.t "q3" (toDigits 0) [0]

-- q4 :: [Integer]
-- q4 = toDigits "123"

q5 :: [Test]
q5 = U.t "q5" (toDigits 666) [6,6,6]

q6 :: [Test]
q6 = U.t "q6" (toDigitsRev 12321) [1,2,3,2,1]

q7 :: [Integer]
q7 = toDigitsRev (-531)

-- q8 :: [Integer]
-- q8 = toDigitsRev 7.6

q9 :: [Test]
q9 = U.t "q9" (toDigitsRev 0) [0]

-- q10 :: [Integer]
-- q10 = toDigits "123"

q11 :: [Test]
q11 = U.t "q11" (toDigitsRev 666) [6,6,6]

q12 :: [Test]
q12 = U.t "q12" (doubleSecond []) []

q13 :: [Test]
q13 = U.t "q13" (doubleSecond [5]) [5]

q14 :: [Test]
q14 = U.t "q14" (doubleSecond [2,5]) [4,5] -- NOTE: course says [2,10]

q15 :: [Integer]
q15 = doubleSecond [1..] -- NOTE: course says [1,4,3,8, ...]

q16 :: [Test]
q16 = U.t "q16" (doubleSecond [1,0,1,0,1]) [1,0,1,0,1]

q17 :: [Test]
q17 = U.t "q17" (sumDigits []) 0

q18 :: [Test]
q18 = U.t "q18" (sumDigits [-12, 12]) 9 -- NOTE: COURSE : SHOULD BE BOTTOM

q19 :: [Test]
q19 = U.t "q19" (sumDigits [0,0,0]) 0

q20 :: [Test]
q20 = U.t "q20" (sumDigits [6,66,666]) 90 -- NOTE: COURSE : 36

q21 :: Integer
q21 = sumDigits [1, 3 ..] -- INFINITE LOOP

q22 :: Bool
q22 = isValid (-12786592316) -- EXCEPTION

-- q23 :: Bool
-- q23 = isValid 231753.65121

q24 :: [Test]
q24 = U.t "q24" (isValid 5256283618614517) True

q25 :: [Test]
q25 = U.t "q25" (isValid 4556945538735694) False

q26 :: [Test]
q26 = U.t "q26" (isValid 0000000000000000) True

q27 :: [Test]
q27 = U.t "q27" (numValid creditcards) 94

------------------------------------------------------------------------------

main :: IO Counts
main =
    T.runTestTT $ T.TestList $ e0 ++ e1 ++ [e2] ++ [e3] ++ [e4] ++
                               q0 ++ q3 ++  q5  ++  q6  ++ q9 ++ q11 ++
                               q12 ++ q13 ++ q14 ++ q16 ++ q17 ++ q18 ++ q19 ++ q20 ++ q24 ++ q25 ++ q26 ++ q27

-- End of file.
