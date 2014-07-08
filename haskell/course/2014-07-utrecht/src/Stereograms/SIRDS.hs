{-
Created       : by Andres Loh.
Last Modified : 2014 Jul 08 (Tue) 07:23:18 by Harold Carr.
-}

module Stereograms.SIRDS where

import           Data.Char
import           Data.Map        (Map (..))
import qualified Data.Map        as M
import           System.Random
                           -- qualification
import           System.IO

import qualified Test.HUnit      as T
import qualified Test.HUnit.Util as U

-- --------------------------- --
-- Writing binary files        --
-- --------------------------- --

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile f x =
  do
    h <- openBinaryFile f WriteMode
    hPutStr h x
    hClose h

-- --------------------------- --
-- Type synonyms and datatypes --
-- --------------------------- --

type Color     = Int                  -- 0 to 255
data RGB       = RGB Color Color Color
                 deriving (Show, Eq)  -- makes it possible to use the show
                                      -- and (==) functions on an RGB
type Image     = [[RGB]]

data Link      = Linked Int Int | Unlinked Int
                 deriving (Show, Eq)  -- makes it possible to use the show
                                      -- and (==) functions on a Link
data Dir       = L | R
                 deriving (Show, Eq, Ord)  -- as above, plus ordering which
                                           -- is required for finite map
                                           -- keys
type Links     = Map (Int, Dir) Int

type Height    = Double
type HeightMap = [[Height]]

-- Some predefined colors.
red, green, blue :: RGB
red    = RGB 255 0 0
green  = RGB 0 255 0
blue   = RGB 0 0 255

gray :: Height -> RGB
gray c = let x = round (c * 255) in RGB x x x

-- The classic tODO function.
tODO :: a -> a
tODO = id

-- ------------ --
-- (PPM) Images --
-- ------------ --

validColor :: Color -> Bool
validColor c = 0 <= c && c <= 255

validRGB :: RGB -> Bool
validRGB (RGB r g b) = validColor r && validColor g && validColor b

validImage :: Image -> Maybe (Int, Int)
validImage [] = Just (0,0)
validImage i@(r:_) =
    let y = length i
        x = length r
        vr = and (map (\r' -> length r' == x) i)
        vi = and [ validRGB p | r' <- i, p <- r' ]
    in if vr && vi then Just (x, y) else Nothing

ppmHeader :: (Int,Int) -> String
ppmHeader (x,y) = "P6 " ++ show x ++ " " ++ show y ++ " 255\n"

encodeRGB :: RGB -> String
encodeRGB (RGB r g b) = map chr [r,g,b]

ppmData :: Image -> String
ppmData i = concat [ encodeRGB p | r <- i, p <- r ]

tp :: RGB
tp = RGB 0 0 0
tr :: [RGB]
tr = [tp,tp,tp]

t0 :: T.Test
t0 = T.TestList
    [
      U.teq "t000" (validColor (-1))                      False
    , U.teq "t001" (validColor 1)                         True
    , U.teq "t002" (validRGB  (RGB 1 2 256))              False
    , U.teq "t003" (validRGB  (RGB 1 2 3))                True
    , U.teq "t004" (validImage [tr,tr,tr,[tp]])           Nothing
    , U.teq "t005" (validImage [tr,tr,tr,[RGB (-1) 0 0]]) Nothing
    , U.teq "t006" (validImage [tr,tr,tr,tr])             (Just (3,4))
    , U.teq "t007" (ppmHeader (2014, 768))                "P6 2014 768 255\n"
    , U.teq "t008" (encodeRGB (RGB 65 66 67))             "ABC"
    , U.teq "t009" (encodeRGB (RGB  0  1  2))             "\NUL\SOH\STX"
    , U.teq "t010" (ppmData [[RGB 65 66 67, RGB 68 69 70], [RGB 71 72 73, RGB 74 75 76]])
                  "ABCDEFGHIJKL"
    ]

writePPM :: FilePath -> Image -> IO ()
writePPM path i =
    case (validImage i) of
        Nothing -> putStrLn "invalid image"
        Just xy -> writeBinaryFile path (ppmHeader xy ++ ppmData i)

------------------------------------------------------------------------------

-- ----- --
-- Links --
-- ----- --

-- The function validLink checks if a given Link is valid, i.e.,
-- if the invariant holds: a valid links is either an Unlinked
-- point or it is a Linked pair of points where the left point
-- is strictly smaller than the right point. This function is
-- given.
validLink :: Link -> Bool
validLink (Linked x y) = x < y
validLink (Unlinked _) = True

-- The `better' operator for links.
-- TODO : INVARIANT : first element is smallest
(>%>) :: Link -> Link -> Bool
(Unlinked _)     >%> (Unlinked _)  = False -- result does not matter
(Unlinked _)     >%> (Linked _  _) = False
(Linked _  _)    >%> (Unlinked _)  = True
(Linked llx lrx) >%> (Linked rlx rrx) = abs (lrx - llx) < abs (rrx - rlx)

noLinks :: Links
noLinks = M.empty

-- TODO : currently assumes lx < rx (if not then need to check)
add :: Link -> Links -> Links
add (Unlinked _)   cs = cs
add (Linked lx rx) cs = M.insert (lx, L) rx (M.insert (rx, R) lx cs)

del :: Link -> Links -> Links
del (Unlinked _)   cs = cs
del (Linked lx rx) cs = M.delete (lx, L) (M.delete (rx, R) cs)

query :: Link -> Dir -> Links -> Link
query (Unlinked r) L cs = queryAux (r, R) cs
query (Unlinked l) R cs = queryAux (l, L) cs
query (Linked _ r) L cs = queryAux (r, R) cs
query (Linked l _) R cs = queryAux (l, L) cs

queryAux :: (Int, Dir) -> Links -> Link
queryAux q@(x, _) cs =
    case M.lookup q cs of
        Nothing -> Unlinked x
        Just r  -> Linked x r

link :: Link -> Links -> Links
link c cs = tODO cs

tcs :: Links
tcs = M.fromList [((3,L),4),((4,R),3)]

t1 :: T.Test
t1 = T.TestList
    [
      U.teq "t100" ((Linked 3 4) >%> (Linked 4 6))    True
    , U.teq "t101" (add (Linked 3 4) noLinks)         tcs
    , U.teq "t102" (del (Linked 3 4) tcs)             noLinks
    , U.teq "t103" (query (Linked 3 4) L tcs)         (Linked 4 3) -- TODO : check
    , U.teq "t104" (query (Linked 3 4) R tcs)         (Linked 3 4) -- TODO : check
    , U.teq "t105" (query (Unlinked 3) R tcs)         (Linked 3 4) -- TODO : check
    , U.teq "t106" (query (Unlinked 3) L tcs)         (Unlinked 3) -- TODO : check
    ]

------------------------------------------------------------------------------

-- --------------------- --
-- Stereogram generation --
-- --------------------- --

-- The following constants are required for calculating
-- the separation. You usually should not need to change
-- them. If you do, be careful because unreasonable values
-- can lead to strange results.

dpi :: Double
dpi = 72.0         -- typical screen resolution (pixels per inch)

e :: Double
e = 2.5 * dpi      -- eye distance in pixels

d :: Double
d = 3.0            -- "distance" between projection plane and base
                   -- plane of the 3D image

separation :: Double -> Int
separation z = tODO 0

sirdsLine :: [Height] -> Links
sirdsLine hs = tODO noLinks

-- Assign random colors to the points of a line, but respect
-- the computed links: linked points should get the same color.
-- This function is given.
assign :: Int -> Links -> IO [RGB]
assign maxX cs =
  do
    let xs      = [0 .. maxX - 1]            -- all relevant x-coordinate
    let classes = map (findRightmost cs) xs  -- equivalence classes of colors
    -- compute random colors
    colorsR <- mapM randomRIO (replicate maxX (0,255))
    colorsG <- mapM randomRIO (replicate maxX (0,255))
    colorsB <- mapM randomRIO (replicate maxX (0,255))
    let colors  = zipWith3 RGB colorsR colorsG colorsB
    return (map (colors !!) classes)

-- Links can form chains in the Links data structure. For a given
-- x-coordinate, the function findRightmost finds the rightmost
-- point in a chain of links. If all the points in a chain of links
-- get the same color as the rightmost point in that chain, then
-- in particular all linked points end up with the same color.
-- This function is given.
findRightmost :: Links -> Int -> Int
findRightmost cs x =
  case query (Unlinked x) R cs of
    Unlinked x' -> x'
    Linked _ x' -> findRightmost cs x'

-- The function sirds computes a SIRDS from a heightmap.
-- It processes the input data line by line using sirdsLine,
-- and the assigns colors using the assign function.
sirds :: HeightMap -> IO Image
sirds = mapM (\ line -> assign (length (line)) (sirdsLine line))

-- -------------------- --
-- Decoding stereograms --
-- -------------------- --

decode :: Image -> Image
decode = map decodeLine

decodeLine :: [RGB] -> [RGB]
decodeLine ps = map (\ x -> M.findWithDefault red x (M.map gray (decodeLine' 0 M.empty ps)))
                    [0 .. length ps - 1]

decodeLine' :: Int -> Map Int Height -> [RGB] -> Map Int Height
decodeLine' _ acc []     = acc
decodeLine' x acc (p:ps) =
  let range = drop (separation 1 - 1)                -- separation 1 is the minimum
                (take (separation 0) (zip [1..] ps)) -- separation 0 is the maximum
      candidates = [ x | (x,q) <- range, p == q ]
      acc' = case candidates of
               []     -> acc
               (x':_) -> M.insert (x + x' `div` 2) (invSeparation x') acc
  in  decodeLine' (x + 1) acc' ps

-- The (approximate) inverse of the separation function.
invSeparation :: Int -> Double
invSeparation s = (2 * d * fromIntegral s - d * e) / (fromIntegral s - e)


-- --------------------------- --
-- Sample heightmap generators --
-- --------------------------- --

-- Turns a heightmap into an image, mapping the heights to gray values.
heightMap :: HeightMap -> Image
heightMap = map (map gray)

doubleChess :: HeightMap
doubleChess = toImage (maxX, maxY) doubleChess'

doubleChess' :: Gen Double
doubleChess' = zscale 0.8 (translate (25,25) (chess' 100)) \^/ chess' 50


------------------------------------------------------------------------------

-- ------------ --
-- Main program --
-- ------------ --

-- Default resolution to use; larger, especially wider, images are easier
-- to view, but of course, also more space- and time-intensive to generate.
maxX = 800
maxY = 400

-- An example main program. Feel free to change it to print other images
-- or perform other computations.
main =
  do
    writePPM "doubleChessPattern.ppm" (heightMap doubleChess) -- prints the pattern, unencoded
    i <- sirds doubleChess
    writePPM "doubleChess.ppm" i -- prints encoded chess pattern SIRDS
    writePPM "doubleChessDecoded.ppm" (decode i) -- prints decoded chess pattern SIRDS

------------------------------------------------------------------------------

-- The following functions are utilities for image computation.
-- They're not relevant for the assignment, only if you want to
-- define your own images as Haskell functions.

-- type of image generators
type Gen a = (Int, Int) -> a

-- turns an image generator into a list-of-list image;
-- kept polymorphic so that it works for HeightMap and
-- Image
toImage :: (Int,Int) -> (Gen a) -> [[a]]
toImage (maxX,maxY) f = [ [ f (x,y) | x <- [0..maxX-1] ] | y <- [0..maxY-1] ]

-- composes two image-generating functions, retaining the maximal values
(\^/) :: Ord a => Gen a -> Gen a -> Gen a
(f \^/ g) (x,y) = f (x,y) `max` g (x,y)

-- translate an image
translate :: (Int,Int) -> Gen a -> Gen a
translate (dx,dy) f (x,y) = f (x - dx, y - dy)

-- z-scale a heightmap generator
zscale :: Double -> Gen Double -> Gen Double
zscale s f (x,y) = s * f (x,y)

-- Generates Figure 2.
chess :: Image
chess = toImage (80,80) (gray . chess' 10)

chess' :: Int -> Gen Height
chess' n (x,y) = fromIntegral (((x `div` n) + (y `div` n) + 1) `mod` 2)

-- Generates Figure 3.
gradient :: Image
gradient = toImage (80,80)
           (\ (x,y) -> let step = round (fromIntegral (x + y) * 255 / 158)
                       in  RGB step step 255)

-- Generates Figure 4.
circular = toImage (80,80)
           (\ (x,y) -> let dist = round (sqrt (fromIntegral ((x - 40)^2 + (y - 40)^2))
                                           * 255 / 57)
                       in  RGB dist (255 - dist) 255)

------------------------------------------------------------------------------

ss :: IO T.Counts
ss = do
    _ <- T.runTestTT t0
    T.runTestTT t1

-- End of file.
