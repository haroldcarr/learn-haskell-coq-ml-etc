module Lib where

{-
example follows Henderson (2002)
- constructing images out of recursively-subdivided images
- each image may be modified by a spatial transformation

Basic Building Blocks
-}

data Tile

-- terminal constructors: haskell and church
haskell :: Tile
haskell  = undefined

church  :: Tile
church   = undefined

color
    :: Double  -- ^ red
    -> Double  -- ^ green
    -> Double  -- ^ blue
    -> Double  -- ^ alpha
    -> Tile
color = undefined

{-
each color should be in closed interval [0,1].
nothing in the typesystem requires this to be the case, so we will need to constrain it with a law:

∀ (r :: Double) (g :: Double) (b :: Double)
      (a :: Double).
  color r g b a =
    color (clamp 0 1 r)
          (clamp 0 1 g)
          (clamp 0 1 b)
          (clamp 0 1 a)

All terms in an algebra are built from
- terminal constructors, and
- inductive constructors
  - ones which "derive" new terms based on existing terms
-}

-- inductive
-- rotate 90 degress C(lock)W(ise)
cw :: Tile -> Tile
cw  = undefined

-- rotate 90 degress C(ounter)C(lock)W(ise)
ccw :: Tile -> Tile
ccw  = undefined

{-
-- laws
∀ (t :: Tile).
  cw (cw (cw (cw t))) = t

∀ (t :: Tile).
  ccw (cw t) = t

∀ (t :: Tile).
  cw (ccw t) = t

-- equational reasoning

  ccw t
= (via cw/cw/cw/cw)
  ccw (cw (cw (cw (cw t))))
= (via ccw/cw)
  cw (cw (cw t))

-}

-- flip H(orizontally)
flipH :: Tile -> Tile
flipH  = undefined

{-
∀ (t :: Tile).
  flipH (flipH t) = t

∀ (t :: Tile).
  flipH (cw (cw (flipH t)) = cw (cw t)
-}

{-
Exercise

 Prove flipH . cw^{2*n} . flipH = cw^{2*n}, where the ^ operation means repeated composition. For example, cw^4 = cw . cw . cw . cw.

Horizontally flipping a clockwise rotation is equivalent to rotating counterclockwise a horizontal flip.
This law that cw to ccw under flipH transformation:

∀ (t :: Tile).
   flipH (cw t) = ccw (flipH t)
-}

-- flipV(ertically)
flipV :: Tile -> Tile
flipV  = undefined

{-
can derive it from cw, ccw and flipH

-- its own inverse
∀ (t :: Tile).
  flipV (flipV t) = t

-- can derive it from cw, ccw and flipH
∀ (t :: Tile).
  flipV t = ccw (flipH (cw t))
∀ (t :: Tile).
  flipV (flipH t) = cw (cw t)

Exercise

 Derive the fact that flipV is its own inverse, using any of the other laws we’ve given for our algebra.

Solution
  flipV (flipV t)
= (via flipV)
  flipV (ccw (flipH (cw t)))
= (via flipV)
  ccw (flipH (cw (ccw (flipH (cw t)))))
= (via cw/ccw)
  ccw (flipH (flipH (cw t)))
= (via flipH/flipH)
  ccw (cw t)
= (via ccw/cw)
  t
Exercise

 Derive a proof that flipV . flipH = cw . cw

Solution
  flipV (flipH t)
= (via flipV)
  ccw (flipH (cw (flipH t)))
= (via ccw)
  cw (cw (cw (flipH (cw (flipH t)))))
= (via x-symmetry)
  cw (cw (flipH (ccw (cw (flipH t)))))
= (via ccw/cw)
  cw (cw (flipH (flipH t)))
= (via flipH/flipH)
  cw (cw t)


composing multiple tiles together
- every operation in algebra must take valid inputs to valid outputs
- tiles are always square
- Simply putting one square tile beside another would result in a rectangular image, INVALIDE
- to maintain closure, must subdivide square into two rectangular halves, then fill each half, stretching tiles to cover space
-}
beside :: Tile -> Tile -> Tile
beside  = undefined

above :: Tile -> Tile -> Tile
above  = undefined

quad :: Tile -> Tile -> Tile -> Tile -> Tile
quad  = undefined

swirl :: Tile -> Tile
swirl  = undefined

behind :: Tile -> Tile -> Tile
behind  = undefined

{-
∀ (t1 :: Tile) (t2 :: Tile).
  flipH (beside t1 t2) = beside (flipH t2) (flipH t1)

Exercise
 Prove flipH (flipH (beside t1 t2)) = beside t1 t2 in two separate ways.

∀ (t1 :: Tile) (t2 :: Tile).
  above t1 t2 = cw (beside (ccw t1) (ccw t2))

Intuitively, we can also rewrite an above of besides as a beside of aboves, so long as we swap the top-right and bottom-left tiles when we do so.

∀ (a :: Tile) (b :: Tile) (c :: Tile) (d :: Tile).
  above (beside a b) (beside c d) =
    beside (above a c) (above b d)

∀ (a :: Tile) (b :: Tile) (c :: Tile) (d :: Tile).
  above (beside a b) (beside c d) = quad a b c d

As an even more special case, we can rotate one tile as we move through a quad, creating a sort of swirl effect as in figure 18. This operation is given by:

∀ (t :: Tile).
  quad t (cw t) (ccw t) (cw (cw t)) = swirl t

color combinator property : unaffected by cw and flipH:

∀ (r :: Double) (g :: Double) (b :: Double)
      (a :: Double).
  cw (color r g b a) = color r g b a
∀ (r :: Double) (g :: Double) (b :: Double)
      (a :: Double).
  flipH (color r g b a) = color r g b a

-- color r g b 0 is a right-identity for behind
∀ (t :: Tile) (r :: Double) (g :: Double) (b :: Double).
  behind t (color r g b 0) = t

empty :: Tile
∀ (r :: Double) (g :: Double) (b :: Double).
  color r g b 0 = empty


equality of tiles to be “equal
-- cannot be definitional equality, because equation that t = cw (cw (cw (cw t))) is not equal syntactically
- two tiles are equal IFF they render to equal images : equal matrices of pixels

Semantics of tile algebra freely subdivide space, so images generated by it can have arbitrarily precise levels of detail.
-}

-- OBSERVATION of algebra
-- function “out” of tile algebra.

rasterize
    :: Int    -- ^ resulting width
    -> Int    -- ^ resulting height
    -> Tile
    -> [[Color]]  -- ^ pixels in row-major order
rasterize = undefined

data Color
--instance Eq Color

{-
two tiles are equal IFF they produce the same image under rasterize

∀ (t1 :: Tile) (t2 :: Tile).
  (∀ (w :: Int) (h :: Int).
    rasterize w h t1 == rasterize w h t2) => t1 = t2

Laws that constrain the observation of tile algebra.
e.g, flipV operation moves  bottom row of pixels to top, specifically, it should reverse order of the rows.
∀ (t :: Tile) (w :: Int) (h :: Int).
  rasterize w h (flipV t) = reverse (rasterize w h t)

flipH should flip the pixels within each row:
∀ (t :: Tile) (w :: Int) (h :: Int).
  rasterize w h (flipH t) =
    fmap reverse (rasterize w h t)

To put one tile above another, split height in half, then concatenate rows of top raster with bottom.
Attention paid to the height computations because integers are not always evenly divisible;
here we will make the arbitrary decision that the bottom raster should soak up the extra tile.
∀ (t1 :: Tile) (t2 :: Tile) (w :: Int) (h :: Int).
  rasterize w h (above t1 t2) =
    rasterize w (div h 2) t1 <>
      rasterize w (h - div h 2) t2

put tiles beside one another in a similar fashion, by gluing together each row.
first convert each raster to column-major, glue the columns together, and then convert back.

The transpose :: [[a]] -> [[a]] function can do this major-order shifting.
Here too, decide by fiat that the right-most tile absorbs the extra pixels if necessary.
There is an argument to be made here that perhaps we should “blend” the middle column, but as we will see in chapter 2.1.4, this approach doesn’t generalize nicely.
∀ (t1 :: Tile) (t2 :: Tile) (w :: Int) (h :: Int).
  rasterize w h (beside t1 t2) =
    transpose $
      transpose (rasterize (div w 2) h t1) <>
        transpose (rasterize (w - div w 2) h t2)

The clockwise cw operation requires us to rotate our rasterized matrix. I didn’t know of any built-in function to perform this operation, so I experimented until I found fmap reverse . transpose which works, though admittedly in

-}

