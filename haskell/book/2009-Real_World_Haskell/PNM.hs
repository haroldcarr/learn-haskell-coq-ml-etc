module PNM where

{-
netpbm suite

Grayscale Files: PGM (portable gray map).
- plain (P2) format: encoded as ASCII
- raw   (P5) format: mostly binary

Starts with header
- MAGIC string: "P2" or "P5"
- whitespace
- three numbers: the width, height, and maximum gray value (as ASCII decimal)

Image data
- raw : string of binary values
- plain : values represented as ASCII decimal numbers separated by single-space

Raw file can contain a sequence of images, each with its own header.
A plain file contains only one image.

Use ByteString to store graymap data, because it’s compact.

Since the header of a PGM file is ASCII text but its body is binary,
we import both the text- and binary-oriented ByteString modules.
-}

-- lazy/strict ByteString doesn't matter here
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy       as L
import Data.Char (isSpace)
import Data.Int (Int64)

data Greymap = Greymap {
      greyWidth  :: Int
    , greyHeight :: Int
    , greyMax    :: Int
    , greyData   :: L.ByteString
    } deriving (Eq)

-- do not automatically derive Show (potentially data too big)
-- instead just show header (and don't define Read, since not printing data)
instance Show Greymap where
    show (Greymap w h m _) = "Greymap"
                             ++ " " ++ show w ++ "x" ++ show h
                             ++ " " ++ show m

-- returns single parsed Greymap and string that remains after parsing
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString

matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)

getNat s = case L8.readInt s of
    Nothing -> Nothing
    Just (num,rest)
        | num <= 0  -> Nothing
        | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)

getBytes n str =
    let count           = fromIntegral n
        both@(prefix,_) = L.splitAt count str
    in if L.length prefix < count
           then Nothing
           else Just both

{- getting rid of boilerplate 238/278
Every step in "ladder" of parseP5 deconstructs a Maybe and either fails or passes the unwrapped result to a function.
Abstract into: value as left argument, function as right.
If not Nothing then apply function to value wrapped in Just.
Defined as operator to enable chaining functions together.
No fixity declaration, so defaults to infixl 9
(left-associative, strongest operator precedence):
a >>? b >>? c evaluated from left to right as (a >>? b) >>? c)
-}

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

-- to left of  >>? is a Maybe
-- to right of >>? is a function that returns Maybe
parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s       >>? -- read and match header
    \s -> skipSpace ((), s)            >>?
    (getNat . snd)                     >>? -- read width::int from stream
    skipSpace                          >>?
    \(width,   s) -> getNat s          >>? -- read height::int from stream
    skipSpace                          >>?
    \(height,  s) -> getNat s          >>? -- read maxGrey::int from stream
    \(maxGrey, s) -> getBytes 1 s      >>? -- TODO: this is reading and ignoring
    (getBytes (width * height) . snd)  >>? -- read data
    \(bitmap,  s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace    (a, s)             = Just (a, L8.dropWhile isSpace s)

