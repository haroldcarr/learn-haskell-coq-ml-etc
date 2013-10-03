module Parse where

import PNM
import Control.Applicative ((<$>))
import Char (chr)

-- TODO "fix" imports
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy       as L
import Data.Char (isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

{- implicit state 239/279
Many functions above similar types: ByteString as last argument and returns Maybe pair.
Hard to change the state passed around because of using pairs.
So make state explicit.  And use record accessors instead of pattern matching pair.
-}

data ParseState = ParseState {
      string :: L.ByteString -- current residual string
    , offset :: Int64        -- where we are in input
    } deriving (Show)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- to report an error message if parsing fails
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

{-
Do not expose the impl of parser to users.
Hide details of parser type using a newtype declaration.
newtype is a compile-time wrapper around a function, no runtime overhead.
When we want to use the function, we will apply the runParser accessor.
Do not export the Parse value constructor from our module:
ensures that nobody will be able to create a parser,
nor will they be able to inspect its internals via pattern matching.
-}

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a, ParseState)
}

{- Identity Parser 240/280

Does not touch parse state.
Uses its argument as the result.
We wrap the body of the function in our Parse type to satisfy the type checker.

To call: remove Parse wrapper to get function inside via using "runParse".
Also need to construct a ParseState.
Finally, separate the result of the parse from the final ParseState
-}

identity :: a -> Parse a
identity    a  = Parse (\s -> Right (a, s))

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left  err         -> Left err
        Right (result, _) -> Right result

-- record syntax, updates and pattern matching 241/281
-- creates a new ParseState identical to initState but with new offset field value

modifyOffset :: ParseState -> Int64     -> ParseState
modifyOffset    initState     newOffset  = initState { offset = newOffset }

{-
parse a single byte 242/282
==> chains parsers together
does not take parse state as arg
uses getState to get it, putState to replace it
get/putState only used in places that need state
state passed around implicitly, so easy to change
effectively passing ParseState down chain of Parse values in a hidden argument
-}
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
        Nothing ->
            bail "no more input"
        Just (byte,remainder) ->
            putState newState ==> \_ ->
            identity byte
          where newState  = initState { string = remainder,
                                        offset = newOffset }
                newOffset = offset initState + 1

-- get current parsing state
getState :: Parse ParseState
getState  = Parse (\s -> Right (s, s))

-- replace current parsing state
putState :: ParseState -> Parse ()
putState    s           = Parse (\_ -> Right ((), s))

-- report parse error
bail :: String -> Parse a
bail    err     = Parse $ \s -> Left $
                  "byte offset " ++ show (offset s) ++ ": " ++ err

-- chaining parsers together
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
  where chainedParser initState =
            case runParse firstParser initState of
                Left errMessage ->
                    Left errMessage
                Right (firstResult, newState) ->
                    runParse (secondParser firstResult) newState

-- functor instance for Parse 250/290
-- Functor is generalized way to map over a parameterized type
-- function to be "fmap"ped should be applied to the current result of a parse
-- and leave the parse state untouched

instance Functor Parse where
    fmap f parser = parser ==> \result -> identity (f result)

{- Using functors for parsing 251/291

Functors let us write tidy, expressive code.

In "parseByte" above, we will want to work with ASCII characters instead
of Word8 values.  Could write "parseChar" of similar structure.
Avoid code duplication by taking advantage of the functor nature of Parse.
Our functor takes the result of a parse and applies a function to it.
we need a function that turns a Word8 into a Char
-}

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

{-
Use functors to write compact “peek” function.
Returns Nothing if at end of input string.
Otherwise returns the next character without consuming it.

Note two calls to fmap, one disguised as (<$>).
Necessary because the type Parse (Maybe a) is a functor within a functor.
Have to lift function twice to "get it into" the inner functor.
-}

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

-- generic combinator: Parse analogue of takeWhile
-- using functors (doubled up, when necessary)
parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
                   then parseByte ==> \b ->
                        (b:) <$> parseWhile p
                   else identity []

-- Same function that does not use functors.
-- Easier to read when not familiar with functors.
-- But use of functors common in Haskell, so get used to it
parseWhileVerbose p =
    peekByte ==> \mc ->
    case mc of
        Nothing -> identity []
        Just c | p c ->
                     parseByte ==> \b ->
                     parseWhileVerbose p ==> \bs ->
                     identity (b:bs)
               | otherwise ->
                     identity []

-- Rewriting PGM parser 252/292

parseRawPGM =
    parseWhileWith w2c notWhite ==> \header  -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header"           ==>&
    parseNat                    ==> \width   -> skipSpaces ==>&
    parseNat                    ==> \height  -> skipSpaces ==>&
    parseNat                    ==> \maxGrey -> parseByte  ==>&
    parseBytes (width * height) ==> \bitmap  -> identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
               then bail "no more input"
               else let n = read digits
                    in if n < 0
                           then bail "integer overflow"
                           else identity n

-- chains parsers (like ==>) but the righthand side ignores the result from the left.
(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

-- Check a property and abort parsing with a useful error message.
assert :: Bool -> String -> Parse ()
assert True    _ = identity ()
assert False err = bail err

-- Few functions now reference parsing state.
-- All of state management in parseRawPGM is hidden.
-- But the next needs state.

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n'     = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st'    = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h

