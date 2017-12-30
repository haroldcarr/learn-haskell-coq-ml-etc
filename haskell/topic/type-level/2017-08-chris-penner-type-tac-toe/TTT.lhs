> {-# language DataKinds      #-}
> {-# language DeriveFunctor  #-}
> {-# language GADTs          #-}
> {-# language KindSignatures #-}
> {-# language TypeFamilies   #-}
> {-# language ViewPatterns   #-}
>
> module TTT where
>
> import Data.Function ((&))

http://chrispenner.ca/posts/type-tac-toe.html

Type Tac Toe: Advanced Type Safety

by Chris Penner
Aug 25, 2017

Tic Tac Toe in types : statically guarantee nobody cheats.

singletons library which can generate a lot of the type-level primitives written below

> data PieceT = X | O | N
>   deriving (Eq, Show)
>
> data Trip a = Trip a a a
>   deriving (Eq, Functor, Show)
>
> newtype BoardRT a = BoardRT (Trip (Trip a))
>   deriving (Eq, Functor, Show)
>
> newBoardRT :: BoardRT PieceT
> newBoardRT  = BoardRT $ Trip (Trip N N N)
>                              (Trip N N N)
>                              (Trip N N N)

> -- | reference to individual squares in grid
> -- (x, y) coordinates (not using Int so coordinates are in bounds)
> data CoordT = A | B | C
>   deriving (Eq, Show)

> -- | Alter a value inside a triple
> -- Set values using `const x`
> overTrip :: CoordT -> (a -> a) -> Trip a -> Trip a
> overTrip A f (Trip a b c) = Trip (f a)   b    c
> overTrip B f (Trip a b c) = Trip    a (f b)   c
> overTrip C f (Trip a b c) = Trip    a    b (f c)

> play :: PieceT -> (CoordT, CoordT) -> BoardRT PieceT -> BoardRT PieceT
> play p (x, y) (BoardRT b) = BoardRT $ overTrip y (overTrip x (const p)) b

'play' does not validate.
- can play on same square over and over
- player X plays again and agin without giving O a turn
- could do error handling at runtime inside 'play'

------------------------------------------------------------------------------
Alternating Turns

*types* to represent X and O

can NOT do:

data X
data O
data N

because duplicate defs of X, O, N via 'PieceT'

Use: {-# LANGUAGE DataKinds #-}

Enables using constructors (defined with data or newtype) as types.

To reference the 'type' version of a data constructor, prefix it with an apostrophe.
- can leave off ' if not ambiguous to compiler
- good practice : be explicit for readability

Enabling DataKinds makes 'X, 'O, 'N (via 'PieceT').

DataKinds creates a new Kind for each family of constructors
- kind : type of type : usually shows arity, e.g.,  *, * -> *

λ> :k Int
Int :: *
λ> :k Maybe
Maybe :: * -> *
λ> :k Maybe Int
Maybe Int :: *
λ> :k 'X
'X :: PieceT

Last one says 'X says it is of Ken PieceT

alternate turns : store who's turn it is as part of the type of our board

give Board a parameter t(urn)

note: do not have to have the type in data-structure since compiler will check,
so use Phantom type (useful for specifying type constraints)

newtype Board t a = Board (Trip (Trip a))
  deriving (Eq, Functor, Show)

newBoard type shows that X goes first

newBoard :: Board 'X PieceT

error:
    * Expected a type, but ‘'X’ has kind ‘PieceT’

GHC expects types of kind *.

Via KindSignatures

> newtype BoardCT (t :: PieceT) a = BoardCT (Trip (Trip a))
>   deriving (Eq, Functor, Show)
>
> newBoardCT :: BoardCT 'X PieceT
> newBoardCT  = let BoardRT b = newBoardRT in BoardCT b

KindSignatures enables specifying what 'kind' a type should be.

> playXCT :: (CoordT, CoordT) -> BoardCT 'X PieceT -> BoardCT 'O PieceT
> playXCT (x, y) (BoardCT b) = BoardCT $ overTrip y (overTrip x (const X)) b
>
> playOCT :: (CoordT, CoordT) -> BoardCT 'O PieceT -> BoardCT 'X PieceT
> playOCT (x, y) (BoardCT b) = BoardCT $ overTrip y (overTrip x (const O)) b

duplication cleaned up below

can only playXCT on a board when it's X's turn

λ> playOCT (A, B) newBoard
error:
    • Couldn't match type ‘'X’ with ‘'O’
      Expected type: BoardCT 'O PieceT
        Actual type: BoardCT 'X PieceT

------------------------------------------------------------------------------
Preventing Replays

don't play on a space that's already been played

keep track of each place that has been played via type level list of coords and piece types

> -- | Keep a list of each Piece played and its location
> data BoardRep
>   = Empty
>   | Cons CoordT CoordT PieceT BoardRep

With DataKinds
- BoardRep is a kind
- Empty and Cons (fully applied) are types

Replace t in Board with type level BoardRep

> newtype Board (b :: BoardRep) a = Board (Trip (Trip a))
>   deriving (Eq, Functor, Show)
>
> -- | New boards are 'Empty now
> newBoard :: Board 'Empty PieceT
> newBoard  = let BoardRT b = newBoardRT in Board b

Now each play represented by change at the type level.

Need to get the "type" of the coordinates of each move.

Using {-# LANGUAGE GADTs #-}

Coord has constructor for each of Coordinate value.

Each constructur also sets the phantom type parameter of the Coord to
appropriate type-level version of the coordinate.

> -- | A proxy type which represents a coordinate
> data Coord (a :: CoordT) where
>   A' :: Coord 'A
>   B' :: Coord 'B
>   C' :: Coord 'C

Translate from Coord TYPE to CoordT VALUE.

> -- | Get coord's value from wrapper type
> coordVal :: Coord a -> CoordT
> coordVal A' = A
> coordVal B' = B
> coordVal C' = C

This is verbose.  Can also use Data.Proxy, but it is verbose too.

Use {-# language ViewPatterns #-} to make defs easier to read.

> playX1 :: (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'X b) PieceT
> playX1 (coordVal -> x, coordVal -> y) (Board b) =
>   Board $ overTrip y (overTrip x (const X)) b
>
> playO1 :: (Coord x, Coord y) -> Board b PieceT -> Board ('Cons x y 'O b) PieceT
> playO1 (coordVal -> x, coordVal -> y) (Board b) =
>   Board $ overTrip y (overTrip x (const O)) b

Now both type and value level coordinates available.

Now storing played pieces in type list.

Still need to check that it's an unplayed square.

Solution: type functions : aka {-# language TypeFamilies #-}

> -- | Has a square been played already?
> type family Played (x :: CoordT) (y :: CoordT) (b :: BoardRep) :: Bool where
>   --  Nothing is played on 'Empty board
>   Played _ _            'Empty  = 'False
>   --  Match, so square already played
>   Played x y ('Cons x y _    _) = 'True
>   --  No match, so recurse.
>   Played x y ('Cons _ _ _ rest) = Played x y rest

Use above as type constraints.

> playX2 :: (Played x y b ~ 'False)
>        => (Coord x, Coord y)
>        -> Board b PieceT
>        -> Board ('Cons x y 'X b) PieceT
> playX2 = playX1

> playO2 :: (Played x y b ~ 'False)
>        => (Coord x, Coord y)
>        -> Board b PieceT
>        -> Board ('Cons x y 'O b) PieceT
> playO2 = playO1

Asserts : to call play?2, board must not have played on those coordinates

~ : equality check on two types; creates constraint that requires them to be equal

------------------------------------------------------------------------------
Rechecking Alterating Turns

ensure X and O always alternate

use

> type family Turn (b :: BoardRep) :: PieceT where
>   Turn ('Cons _ _ 'X _) = 'O
>   Turn               _  = 'X

and use in constraints

> playX :: (Played x y b ~ 'False, Turn b ~ 'X)
>       => (Coord x, Coord y)
>       -> Board b PieceT
>       -> Board ('Cons x y 'X b) PieceT
> playX = playX2
>
> playO :: (Played x y b ~ 'False, Turn b ~ 'O)
>       => (Coord x, Coord y)
>       -> Board b PieceT
>       -> Board ('Cons x y 'O b) PieceT
> playO = playO2

------------------------------------------------------------------------------
play a game

> game1 :: Board ('Cons 'A 'A 'X ('Cons 'C 'C 'O ('Cons 'A 'B 'X 'Empty))) PieceT
> game1 = newBoard
>       & playX (A', B')
>       & playO (C', C')
>       & playX (A', A')


game2 = newBoard
      & playX (A', B')
      & playX (C', C')

    - Couldn't match type ‘'O’ with ‘'X’ arising from a use of ‘playX’
    - In the second argument of ‘(&)’, namely ‘playX (C', C')’
      In the expression: newBoard & playX (A', B') & playX (C', C')

game3 = newBoard
      & playX (A', B')
      & playO (A', B')

error:
    • Couldn't match type ‘'True’ with ‘'False’
        arising from a use of ‘playO’
    • In the second argument of ‘(&)’, namely ‘playO (A', B')’
      In the expression: newBoard & playX (A', B') & playO (A', B')

'game1' type includes the entire game board.
it will grow as moves are added

issues
- types unwieldy
- cannot write a function to use user input along lines of:

String -> Board b PieceT -> Board ? PieceT
- parse string into coords for a move
- hard to decide what would go into the '?'
- cannot give it a type because we do not know Coords until parsing
- sometimes possible in Idris' dependent types

post to teach some type machinery, not this dynamic user-input part

Exercise : combine playX and playO into a more general play.
Hint     : make another wrapper type like what was done for Coord.

Idris version: https://gitlab.com/snippets/1673837

Above Haskell solution not complete.
Game ends when three pieces in a line or all places filled.
So type of game should be `Finished`
with constructor with board and player that wins or draw.
E.g., play? :: `Either (Turn (next player)) (Finished)`
