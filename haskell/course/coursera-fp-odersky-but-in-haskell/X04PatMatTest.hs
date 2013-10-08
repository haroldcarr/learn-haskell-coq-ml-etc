{-
Created       : 2013 Oct 07 (Mon) 14:42:02 by carr.
Last Modified : 2013 Oct 07 (Mon) 18:43:47 by carr.
-}

module X04PatMatTest where

import AssertError
import Test.HUnit
import X04PatMat

t1 = Fork (Leaf 'a' 2) (Leaf 'b' 3) ['a','b'] 5
t2 = Fork (Fork (Leaf 'a' 2) (Leaf 'b' 3) ['a','b'] 5) (Leaf 'd' 4) ['a','b','d'] 9
hwBangsChars = "hello, world!!!"
leaflist1 = [Leaf 'e' 1, Leaf 't' 2, Leaf 'x' 4]
leaflist2 = [Leaf 'e' 1, Leaf 't' 4, Leaf 'x' 4]
leaflist3 = [Leaf 'e' 1, Leaf 't' 3, Leaf 'x' 4]
hwTree  = createCodeTree hwBangsChars

frenchCode = createCodeTreeFromUnorderPairs [('s', 121895),
                                             ('d',  56269),
                                             ('x',  5928),
                                             ('j',  8351),
                                             ('f',  16351),
                                             ('z',  2093),
                                             ('k',  745),
                                             ('w',  1747),
                                             ('y',  4725),
                                             ('h',  11298),
                                             ('q',  20889),
                                             ('o',  82762),
                                             ('l',  83668),
                                             ('m',  45521),
                                             ('p',  46335),
                                             ('u',  96785),
                                             ('r',  100500),
                                             ('c',  50003),
                                             ('v',  24975),
                                             ('g',  13288),
                                             ('b',  13822),
                                             ('n',  108812),
                                             ('t',  111103),
                                             ('e',  225947),
                                             ('i',  115465),
                                             ('a', 117110)]

secret = [0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1]

decodedSecret = decode frenchCode secret

ds  = ['h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l']

baseTests = TestList
    [TestCase $ assertEqual "weight of a larger tree" 5    (weight t1)
    ,TestCase $ assertEqual "chars of a larger tree" "abd" (chars t2)
    ,TestCase $ assertEqual "times(\"hello, world\")" [('h',1), ('e',1), ('l',3), ('o',2), (',',1), (' ',1), ('w',1), ('r',1), ('d',1) , ('!',3)]
                                                           (times hwBangsChars)
    ,TestCase $ assertEqual "makeOrderedLeafList for some frequency table"
                                                 [Leaf 'e' 1, Leaf 't' 2, Leaf 'x' 3]
                                                 (makeOrderedLeafList [('t', 2), ('e', 1), ('x', 3)])
    ,TestCase $ assertEqual "makeOrderedLeafList hwBangs"
                                                 [Leaf 'h' 1, Leaf 'e' 1, Leaf ',' 1, Leaf ' ' 1, Leaf 'w' 1, Leaf 'r' 1, Leaf 'd' 1, Leaf 'o' 2, Leaf 'l' 3, Leaf '!' 3]
                                                 (makeOrderedLeafList $ times hwBangsChars)
    ,TestCase $ assertEqual "combine of some leaf list"
                            [Fork (Leaf 'e' 1)
                                  (Leaf 't' 2)
                                  ['e', 't']
                                  3,
                             Leaf 'x' 4]
                            (combine leaflist1)
    ,TestCase $ assertEqual "combine 2"
                            [Leaf 'x' 4,
                             Fork (Leaf 'e' 1)
                                  (Leaf 't' 4)
                                  ['e', 't']
                                  5]
                            (combine leaflist2)
    ,TestCase $ assertEqual "combine 3"
                            [Fork (Leaf 'e' 1)
                                  (Leaf 't' 3)
                                  ['e', 't']
                                  4,
                             Leaf 'x' 4]
                            (combine leaflist3)
    ,TestCase $ assertEqual "createCodeTree"
                            (Fork (Fork (Leaf 'l' 3)
                                        (Leaf '!' 3)
                                        ['l', '!']
                                        6)
                                  (Fork (Fork (Fork (Leaf ',' 1)
                                                    (Leaf ' ' 1)
                                                    [',', ' ']
                                                    2)
                                              (Fork (Leaf 'h' 1)
                                                    (Leaf 'e' 1)
                                                    ['h', 'e']
                                                    2)
                                              [',', ' ', 'h', 'e']
                                              4)
                                        (Fork (Leaf 'o' 2)
                                              (Fork (Leaf 'd' 1)
                                                    (Fork (Leaf 'w' 1)
                                                          (Leaf 'r' 1)
                                                          ['w', 'r']
                                                          2)
                                                    ['d', 'w', 'r']
                                                    3)
                                               ['o', 'd', 'w', 'r']
                                               5)
                                        [',', ' ', 'h', 'e', 'o', 'd', 'w', 'r']
                                        9)
                                   ['l', '!', ',', ' ', 'h', 'e', 'o', 'd', 'w', 'r']
                                   15)
                            hwTree
    ,TestCase $ assertEqual "encode hwBangsChars"
                            [1, 0, 1, 0,    -- h
                             1, 0, 1, 1,    -- e
                             0, 0,          -- l
                             0, 0,          -- l
                             1, 1, 0,       -- o
                             1, 0, 0, 0,    -- ,
                             1, 0, 0, 1,    -- ' '
                             1, 1, 1, 1, 0, -- w
                             1, 1, 0,       -- o
                             1, 1, 1, 1, 1, -- r
                             0, 0,          -- l
                             1, 1, 1, 0,    -- d
                             0, 1,          -- !
                             0, 1,          -- !
                             0, 1]          -- !
                             (encode hwTree hwBangsChars)
     ,TestCase $ assertEqual "decodedSecret 1" ds decodedSecret
     ,TestCase $ assertEqual "decodedSecret 2" secret (encode frenchCode ds)
     ,TestCase $ assertEqual "decodedSecret 3" ['l'] (decode frenchCode [0, 1, 0, 1])

     ,TestCase $ assertEqual "decode and encode a very short text should be identity"
                                               "ab"  (decode t1 (encode t1 "ab"))
    ]


text  = "this is an example of a huffman tree"
pairs = makeOrderedLeafList $ times text
wTree  = createCodeTree text

wikiTests = TestList
    [TestCase $ assertEqual "http://en.wikipedia.org/wiki/Huffman_coding 1"
                            [Leaf 'x' 1, Leaf 'p' 1, Leaf 'l' 1, Leaf 'o' 1, Leaf 'u' 1, Leaf 'r' 1,
                             Leaf 't' 2, Leaf 'h' 2, Leaf 'i' 2, Leaf 's' 2, Leaf 'n' 2, Leaf 'm' 2,
                             Leaf 'f' 3,
                             Leaf 'a' 4, Leaf 'e' 4,
                             Leaf ' ' 7]
                            pairs
    ,TestCase $ assertEqual "http://en.wikipedia.org/wiki/Huffman_coding 2"
                            text
                            (decode wTree (encode wTree text))
    ]

qTable = convert $ createCodeTree hwBangsChars

quickTests = TestList
    [TestCase $ assertEqual "quick"
                            [('l', [0, 0]),
                             ('!', [0, 1]),
                             (',', [1, 0, 0, 0]),
                             (' ', [1, 0, 0, 1]),
                             ('h', [1, 0, 1, 0]),
                             ('e', [1, 0, 1, 1]),
                             ('o', [1, 1, 0]),
                             ('d', [1, 1, 1, 0]),
                             ('w', [1, 1, 1, 1, 0]),
                             ('r', [1, 1, 1, 1, 1])]
                            qTable
    ,TestCase $ assertEqual "quick 2"
                            (encode      hwTree hwBangsChars)
                            (quickEncode hwTree hwBangsChars)
    ]

main = do
    runTestTT baseTests
    runTestTT wikiTests
    runTestTT quickTests

-- End of file.
