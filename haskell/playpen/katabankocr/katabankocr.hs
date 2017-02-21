{-
http://joaopizani.hopto.org/en/2012/01/haskell-kata-bankocr/
http://codingdojo.org/cgi-bin/wiki.pl?KataBankOCR

Problem description: OCR

Input: stream containing digits from 0 to 9 in "LED-display-like" format:
    _  _     _  _  _  _  _
  | _| _||_||_ |_   ||_||_|
  ||_  _|  | _||_|  ||_| _|

Input has only one line of "7-segment" digits,
where each digit has a height of 3 "segments" (lines)
      and a width also of 3 "segments" (columns).

Program reads stream and prints the corresponding number on standard output.
For the above: "123456789".

(There are additional usage scenarios --- not done here:
- recognizing several lines of digits,
- validating the numbers
- using error-correcting codes to return good numbers even with "dirty" input.)

Solution is a Unix filter using "interact":

interact ∷ (String → String) → IO ()

"interact" connects standard input -> user function -> standard output.

- "lines" ("Prelude") is String → [String].
  Takes a big String and breaks it down to a list of smaller Strings, one per line.

- "transpose" ("Prelude"), works on lists of lists (matrices):
  turning lines into columns and vice-versa.

- "dummy": there is a blank column between any two digits.  We can
  treat each digit as having 4 columns – EXCEPT FOR THE FIRST ONE.  To
  avoid corner case, force first digit to have 4 columns by adding a
  "dummy" column to it. Strategy: transforming a corner case into a
  normal one and then treating all cases equally.

- "chunk" ("Data.List.Split") returns same-sized groups of elements
  from its input list.

- "flip" ("Prelude") evaluates the function, flipping the order of arguments: (a -> b -> c) -> b -> a -> c

- "lookup" returns a value from an associative table: [ (key, value) ]

- "fromJust" for Maybe Char -> Char because WE ARE SURE that all
  digits are in the table (otherwise crash).

# the spaces shown at the end of 6 are critical
cat katabankOCR-256
 _   _   _ 
 _| |_  |_ 
|_   _| |_|

cat katabankOCR-256        | ./katabankocr
256
cat katabankOCR-2          | ./katabankocr
cat katabankOCR-0123456789 | ./katabankocr

for Data.List.Split:

cabal install split
ghc --make katabankOCR.hs
cat katabankOCR-256 | ./katabankOCR

-- 2
let x = [" _ ", " _|", "|_ "]
-- 256
let x = [" _   _   _ "," _| |_  |_ ","|_   _| |_|"]
lookup x dTable
let transX = transpose x
let dummy = "    "
let plusDummy = dummy : transX
let c4 = chunk 4 plusDummy
let headViaMap = head c4
let tailHC4 = tail headViaMap
let transTail = transpose tailHC4
let flipLookup = (flip lookup) dTable transTail
fromJust flipLookup
-}

import Data.List
import Data.Maybe
import Data.List.Split

main = interact (\input -> parse input ++ "\n")

-- first transpose is to turn the 3 lines into columns
-- then add the "missing" column at the front.
-- then break into 4 column groupings (the numbers with empty column at beginning of each)
-- then, for each group
--    remove the empty column before each number
--      (alternatively, could have left it there and added space in dTable definitions)
--    transpose back to lines (not strictly necessary, but makes definition of dTable visually intuitive)
-- then do the lookup (using flip to enable point-free style throughout definition of digit)
parse input = map (digit . transpose . tail) (chunk 4 $ dummyColumn : columns input)
    where digit = fromJust . (flip lookup) dTable
          columns  = transpose . lines
          dummyColumn = "    "

dTable = [
           ([" _ ",
             "| |",
             "|_|"], '0'),
           (["   ",
             "  |",
             "  |"], '1'),
           ([" _ ",
             " _|",
             "|_ "], '2'),
           ([" _ ",
             " _|",
             " _|"], '3'),
           (["   ",
             "|_|",
             "  |"], '4'),
           ([" _ ",
             "|_ ",
             " _|"], '5'),
           ([" _ ",
             "|_ ",
             "|_|"], '6'),
           ([" _ ",
             "  |",
             "  |"], '7'),
           ([" _ ",
             "|_|",
             "|_|"], '8'),
           ([" _ ",
             "|_|",
             " _|"], '9') ]

-- End of file.
