{-
Created       : 2013 Oct 07 (Mon) 14:41:15 by carr.
Last Modified : 2013 Nov 04 (Mon) 21:15:22 by carr.
-}

module X06ForCompTest where

import Test.HUnit
import Test.HUnit.Util -- https://github.com/haroldcarr/test-hunit-util
import qualified Data.Map.Strict as M
import System.IO.Unsafe
import X06ForComp

dictionaryByOccurrences' = unsafePerformIO dictionaryByOccurrences

abba = [('a', 2), ('b', 2)]
abbacomb = [[]
           ,[('b',1)]
           ,[('b',2)]
           ,[('a',1)]
           ,[('a',2)]
           ,[('a',1),('b',1)]
           ,[('a',2),('b',1)]
           ,[('a',1),('b',2)]
           ,[('a',2),('b',2)]
           ]
{-
abbacomb = [[]
           ,[('a',1)]
           ,[('a',2)]
           ,[('b',1)]
           ,[('a',1), ('b',1)]
           ,[('a',2), ('b',1)]
           ,[('b',2)]
           ,[('a',1), ('b',2)]
           ,[('a',2), ('b',2)]
           ]
-}

sentence = ["Linux", "rulez"]
anas     = [["Zulu","Lin","Rex"]
           ,["Zulu","nil","Rex"]
           ,["Zulu","Rex","Lin"]
           ,["Zulu","Rex","nil"]
           ,["null","Uzi","Rex"]
           ,["null","Rex","Uzi"]
           ,["Uzi","null","Rex"]
           ,["Uzi","Rex","null"]
           ,["Lin","Zulu","Rex"]
           ,["Lin","Rex","Zulu"]
           ,["nil","Zulu","Rex"]
           ,["nil","Rex","Zulu"]
           ,["Linux","rulez"]
           ,["Rex","Zulu","Lin"]
           ,["Rex","Zulu","nil"]
           ,["Rex","null","Uzi"]
           ,["Rex","Uzi","null"]
           ,["Rex","Lin","Zulu"]
           ,["Rex","nil","Zulu"]
           ,["rulez","Linux"]
           ]
{-
anas =     [["Rex", "Lin", "Zulu"]
           ,["nil", "Zulu", "Rex"]
           ,["Rex", "nil", "Zulu"]
           ,["Zulu", "Rex", "Lin"]
           ,["null", "Uzi", "Rex"]
           ,["Rex", "Zulu", "Lin"]
           ,["Uzi", "null", "Rex"]
           ,["Rex", "null", "Uzi"]
           ,["null", "Rex", "Uzi"]
           ,["Lin", "Rex", "Zulu"]
           ,["nil", "Rex", "Zulu"]
           ,["Rex", "Uzi", "null"]
           ,["Rex", "Zulu", "nil"]
           ,["Zulu", "Rex", "nil"]
           ,["Zulu", "Lin", "Rex"]
           ,["Lin", "Zulu", "Rex"]
           ,["Uzi", "Rex", "null"]
           ,["Zulu", "nil", "Rex"]
           ,["rulez", "Linux"]
           ,["Linux", "rulez"]
           ]
-}
tests = TestList
    [teq "wordOccurrences: abcd"            (wordOccurrences "abcd")                                           [('a', 1), ('b', 1), ('c', 1), ('d', 1)]
    ,teq "wordOccurrences: Robert"          (wordOccurrences "Robert")                                         [('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)]
    ,teq "sentenceOccurrences: abcd e"      (sentenceOccurrences ["abcd", "e"])                                [('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)]
    ,teq "dictionaryByOccurrences.get: eat" (M.lookup [('a', 1), ('e', 1), ('t', 1)] dictionaryByOccurrences') (Just ["ate","eat","tea"]) -- .map(_.toSet) === Some(Set("ate", "eat", "tea"))
    ,teq "word anagrams: married"           (wordAnagrams "married" dictionaryByOccurrences')                  ["admirer","married"] -- ["married", "admirer"]
    ,teq "word anagrams: player"            (wordAnagrams "player" dictionaryByOccurrences')                   ["parley", "pearly", "player", "replay"]
    ,teq "subtract: lard - r"               (subtract' [('a', 1), ('d', 1), ('l', 1), ('r', 1)]  [('r', 1)] )  [('a', 1), ('d', 1), ('l', 1)]
    ,teq "combinations: []"                 (combinations [])                                                  ([[]] :: [[(Char, Integer)]])
    ,teq "combinations: abba"               (combinations abba)                                                abbacomb
    ,teq "sentence anagrams: []"            (sentenceAnagrams []       dictionaryByOccurrences')               [[]]
    ,teq "sentence anagrams: Linux rulez"   (sentenceAnagrams sentence dictionaryByOccurrences')               anas
    ]

main = runTestTT tests

-- End of file.


