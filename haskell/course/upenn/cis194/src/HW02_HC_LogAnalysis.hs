{-# OPTIONS_GHC -Wall #-}

module HW02_HC_LogAnalysis where

import           HW02_Log
import           System.IO.Unsafe (unsafePerformIO)
import qualified Test.HUnit       as T
import qualified Test.HUnit.Util  as U

-- Exercise 1

-- I|W               TimeStamp::Int Msg::String
-- E   Severity::Int TimeStamp::Int Msg::String

parseMessage :: String -> LogMessage
parseMessage ('I':' ':xs) = let    (ts, msg) =  parseTsMsg [] xs in LogMessage Info      ts msg
parseMessage ('W':' ':xs) = let    (ts, msg) =  parseTsMsg [] xs in LogMessage Warning   ts msg
parseMessage ('E':' ':xs) = let (s, ts, msg) = parseSTsMsg [] xs in LogMessage (Error s) ts msg
parseMessage r@_      = Unknown r

parseTsMsg :: String -> String -> (Int, String)
parseTsMsg acc (x:xs) = if x == ' '
                        then ((read (reverse acc)) :: Int, xs)
                        else parseTsMsg (x:acc) xs

parseSTsMsg :: String -> String -> (Int, Int, String)
parseSTsMsg acc (x:xs) = if x == ' '
                         then let (ts, msg) = parseTsMsg [] xs in ((read (reverse acc)) :: Int, ts, msg)
                         else parseSTsMsg (x:acc) xs

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

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
    , U.teq "u" (unsafePerformIO (testParse parse 10 "src/HW02_error.log")) [LogMessage Info 5053 "pci_id: con ing!",LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)",LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled",LogMessage Info 4076 "verse.'",LogMessage Info 4764 "He trusts to you to set them free,",LogMessage Info 858 "your pocket?' he went on, turning to Alice.",LogMessage Info 898 "would be offended again.",LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)",LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And",LogMessage Info 3899 "hastily."]
    ]

-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert _ (Node _ (Unknown _) _) = error "impossible, but type checker complains"
insert lm@_ Leaf = Node Leaf lm Leaf
insert l@(LogMessage _ ts _) n@(Node left l'@(LogMessage _ ts' _) right) =
    case compare ts ts' of
        LT -> (Node (insert l left)  l'           right)
        EQ -> n
        GT -> (Node           left   l' (insert l right))

i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 :: MessageTree
i1 = insert (LogMessage Info 5053 "pci_id: con ing!") Leaf
i2 = insert (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") i1
i3 = insert (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") i2
i4 = insert (LogMessage Info 4076 "verse.'") i3
i5 = insert (LogMessage Info 4764 "He trusts to you to set them free,") i4
i6 = insert (LogMessage Info 858 "your pocket?' he went on, turning to Alice.") i5
i7 = insert (LogMessage Info 898 "would be offended again.") i6
i8 = insert (LogMessage Info 3753 "pci 0x18fff steresocared, overne: 0000 (le wailan0: ressio0/derveld fory: alinpu-all)") i7
i9 = insert (LogMessage Info 790 "those long words, and, what's more, I don't believe you do either!' And") i8
i10= insert (LogMessage Info 3899 "hastily.") i9

e2 :: T.Test
e2 = T.TestList
    [
      U.teq "1" i1 (Node Leaf (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq "2" i2 (Node (Node Leaf (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") Leaf) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq "3" i3 (Node (Node (Node Leaf (LogMessage Warning 3654 "e8] PGTT ASF! 00f00000003.2: 0x000 - 0000: 00009dbfffec00000: Pround/f1743colled") Leaf) (LogMessage Info 4681 "ehci 0xf43d000:15: regista14: [0xbffff 0xfed nosabled 00-02] Zonseres: brips byted nored)") Leaf) (LogMessage Info 5053 "pci_id: con ing!") Leaf)
    , U.teq  "4"  i4 Leaf
    , U.teq  "5"  i5 Leaf
    , U.teq  "6"  i6 Leaf
    , U.teq  "7"  i7 Leaf
    , U.teq  "8"  i8 Leaf
    , U.teq  "9"  i9 Leaf
    , U.teq "10" i10 Leaf
    ]

main :: IO T.Counts
main = do
    _ <- T.runTestTT e1
    T.runTestTT e2

-- End of file.


