{-# OPTIONS_GHC -Wall #-}

module HW02_HC_LogAnalysis where

import           Data.List        (foldl', mapAccumR)
import           HW02_Log
import           System.IO.Unsafe (unsafePerformIO)
import qualified Test.HUnit       as T
import qualified Test.HUnit.Util  as U

------------------------------------------------------------------------------
-- Exercise 1

-- I|W               TimeStamp::Int Msg::String
-- E   Severity::Int TimeStamp::Int Msg::String

parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = let    (ts, msg) =  parseTsMsg [] xs in LogMessage Info      ts msg
parseMessage ('W':' ':xs) = let    (ts, msg) =  parseTsMsg [] xs in LogMessage Warning   ts msg
parseMessage ('E':' ':xs) = let (s, ts, msg) = parseSTsMsg [] xs in LogMessage (Error s) ts msg
parseMessage r@_      = Unknown r

parseTsMsg :: String -> String -> (Int, String)
parseTsMsg _   []     = error "impossible"
parseTsMsg acc (x:xs) = if x == ' '
                        then ((read (reverse acc)) :: Int, xs)
                        else parseTsMsg (x:acc) xs

parseSTsMsg :: String -> String -> (Int, Int, String)
parseSTsMsg _   []     = error "impossible"
parseSTsMsg acc (x:xs) = if x == ' '
                         then let (ts, msg) = parseTsMsg [] xs in ((read (reverse acc)) :: Int, ts, msg)
                         else parseSTsMsg (x:acc) xs

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

testData :: [LogMessage]
testData =  [ LogMessage Info 5053 "pci_id: con ing!"
            , LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
            , LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
            , LogMessage Info 4076 "verse.'"
            , LogMessage Info 4764 "He trusts to you to set them free,"
            , LogMessage Info 858 "your pocket?' he went on, turning to Alice."
            , LogMessage Info 898 "would be offended again."
            , LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)"
            , LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And"
            , LogMessage Info 3899 "hastily."
            ]

e1 :: T.Test
e1 = T.TestList
    [
      U.teq "E" (parseMessage "E 2 562 help help")
                (LogMessage (Error 2) 562 "help help")
    , U.teq "I" (parseMessage "I 29 la la la")
                (LogMessage Info 29 "la la la")
    , U.teq "U" (parseMessage "This is not in the right format")
                (Unknown "This is not in the right format")
    , U.teq "p" (parse "E 2 562 help help\nI 29 la la la\nThis is not in the right format")
                [ LogMessage (Error 2) 562 "help help"
                , LogMessage Info 29 "la la la"
                , Unknown "This is not in the right format"
                ]
    , U.teq "u" (unsafePerformIO (testParse parse 10 "src/HW02_error.log")) testData
    ]

------------------------------------------------------------------------------
-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _)           mt                                          = mt
insert _                     (Node _ (Unknown _) _)                      = error "impossible, but type checker complains"
insert l@_                   Leaf                                        = Node Leaf l Leaf
insert l@(LogMessage _ ts _) n@(Node left l'@(LogMessage _ ts' _) right) =
    case compare ts ts' of
        LT -> (Node (insert l left)  l'           right)
        EQ -> n
        GT -> (Node           left   l' (insert l right))

i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 :: MessageTree
i1 = insert (testData !! 0) Leaf
i2 = insert (testData !! 1) i1
i3 = insert (testData !! 2) i2
i4 = insert (testData !! 3) i3
i5 = insert (testData !! 4) i4
i6 = insert (testData !! 5) i5
i7 = insert (testData !! 6) i6
i8 = insert (testData !! 7) i7
i9 = insert (testData !! 8) i8
i10= insert (testData !! 9) i9

e2 :: T.Test
e2 = T.TestList
    [
      U.teq  "i1"  i1 (Node Leaf (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i2"  i2 (Node (Node Leaf (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") Leaf) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i3"  i3 (Node (Node (Node Leaf (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") Leaf) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") Leaf) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i4"  i4 (Node (Node (Node Leaf (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node Leaf (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") Leaf) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i5"  i5 (Node (Node (Node Leaf (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node Leaf (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") (Node Leaf (LogMessage Info 4764 "He trusts to you to set them free,") Leaf)) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i6"  i6 (Node (Node (Node (Node Leaf (LogMessage Info 858 "your pocket?' he went on, turning to Alice.") Leaf) (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node Leaf (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") (Node Leaf (LogMessage Info 4764 "He trusts to you to set them free,") Leaf)) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i7"  i7 (Node (Node (Node (Node Leaf (LogMessage Info 858 "your pocket?' he went on, turning to Alice.") (Node Leaf (LogMessage Info 898 "would be offended again.") Leaf)) (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node Leaf (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") (Node Leaf (LogMessage Info 4764 "He trusts to you to set them free,") Leaf)) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i8"  i8 (Node (Node (Node (Node Leaf (LogMessage Info 858 "your pocket?' he went on, turning to Alice.") (Node Leaf (LogMessage Info 898 "would be offended again.") Leaf)) (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node (Node Leaf (LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)") Leaf) (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") (Node Leaf (LogMessage Info 4764 "He trusts to you to set them free,") Leaf)) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "i9"  i9 (Node (Node (Node (Node (Node Leaf (LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And") Leaf) (LogMessage Info 858 "your pocket?' he went on, turning to Alice.") (Node Leaf (LogMessage Info 898 "would be offended again.") Leaf)) (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node (Node Leaf (LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)") Leaf) (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") (Node Leaf (LogMessage Info 4764 "He trusts to you to set them free,") Leaf)) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq "i10" i10 (Node (Node (Node (Node (Node Leaf (LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And") Leaf) (LogMessage Info 858 "your pocket?' he went on, turning to Alice.") (Node Leaf (LogMessage Info 898 "would be offended again.") Leaf)) (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") (Node (Node Leaf (LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)") (Node Leaf (LogMessage Info 3899 "hastily.") Leaf)) (LogMessage Info 4076 "verse.'") Leaf)) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") (Node Leaf (LogMessage Info 4764 "He trusts to you to set them free,") Leaf)) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    ]

------------------------------------------------------------------------------
-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf -- if implemented with foldr then will not be structurally equal i10

e3 :: T.Test
e3 = T.TestList
    [
      U.teq "build0" (build testData) i10
    ]

------------------------------------------------------------------------------
-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder mt0 = walk mt0 []
  where
    walk Leaf                             acc = acc
    walk (Node l lm@(LogMessage _ _ _) r) acc = walk l acc ++ [lm] ++ walk r acc
    walk (Node _    (Unknown _)        _) _   = error "impossible"

e4 :: T.Test
e4 = T.TestList
    [
      U.teq "inOrder0" (inOrder i10) [
                                      LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And"
                                     ,LogMessage Info 858 "your pocket?' he went on, turning to Alice."
                                     ,LogMessage Info 898 "would be offended again."
                                     ,LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled"
                                     ,LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)"
                                     ,LogMessage Info 3899 "hastily."
                                     ,LogMessage Info 4076 "verse.'"
                                     ,LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)"
                                     ,LogMessage Info 4764 "He trusts to you to set them free,"
                                     ,LogMessage Info 5053 "pci_id: con ing!"
                                     ]
    ]

------------------------------------------------------------------------------
-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms =
    let (r, _) = mapAccumR f [] (inOrder (build lms))
                   where
                     f acc lm@(LogMessage (Error s) _ msg) = if s >= 50 then (msg:acc, lm) else (acc, lm)
                     f acc lm                              = (acc, lm)
    in r

e5 :: T.Test
e5 = T.TestList
    [
      U.teq "www0" (unsafePerformIO (testWhatWentWrong parse whatWentWrong "src/HW02_sample.log"))
                   [ "Way too many pickles"
                   , "Bad pickle-flange interaction detected"
                   , "Flange failed!"
                   ]
    ]

------------------------------------------------------------------------------
-- Exercise 6 - TODO

------------------------------------------------------------------------------
hw02 :: IO T.Counts
hw02 = do
    _ <- T.runTestTT e1
    _ <- T.runTestTT e2
    _ <- T.runTestTT e3
    _ <- T.runTestTT e4
    T.runTestTT e5

-- End of file.


