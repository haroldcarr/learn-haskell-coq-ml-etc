{-
Created       : 2014 Oct 21 (Tue) 15:28:04 by Harold Carr.
Last Modified : 2014 Oct 21 (Tue) 21:36:43 by Harold Carr.

http://www.haskellforall.com/2012/01/haskell-for-mainstream-programmers_28.html
-}

module X_2012_01_28_lenses where

import           Control.Monad.State

{-
Lenses

Generalizes accessors, mutators, ...

C#

class Point {
    public double x { get; set; }
    public double y { get; set; } }

class Circle {
    public Point  center { get; set; }
    public double radius { get; set; } }

public void goRight(ref Circle c) { c.center.x += 10; }

naive Haskell:
-}
data Point0 = Point0 Double Double

getX0 :: Point0 -> Double
getX0 (Point0 a _) = a

setX0 :: Double -> Point0 -> Point0
setX0 x' (Point0 _ y) = Point0 x' y

getY0 :: Point0 -> Double
getY0 (Point0 _ b) = b

setY0 :: Double -> Point0 -> Point0
setY0 y' (Point0 x _) = Point0 x y'

data Circle0 = Circle0 Point0 Double

getCenter0 :: Circle0 -> Point0
getCenter0 (Circle0 p _) = p

setCenter0 :: Point0 -> Circle0 -> Circle0
setCenter0 p' (Circle0 _ r) = Circle0 p' r

getRadius0 :: Circle0 -> Double
getRadius0 (Circle0 _ r) = r

setRadius0 :: Double -> Circle0 -> Circle0
setRadius0 r' (Circle0 p _) = Circle0 p r'

{-
imagine:

getField8 (Record _ _ _ _ _ _ _ x _ _) = x

Haskell syntactic sugar to automatically derive accessor and mutator functions:
-}
data Point1  = Point1  { x1 :: Double, y1 :: Double }
    deriving (Show)
data Circle1 = Circle1 { center1 :: Point1, radius1 :: Double }
    deriving (Show)

-- now modify via names:

set42Radius1 :: Circle1 -> Circle1
set42Radius1 c = c { radius1 = 42.0 }

-- access via names:
-- >>> radius (Circle (Point 3.0 4.0) 5.0)
-- 5.0

-- Haskell equivalent of C# goRight above:

goRight1 :: Circle1 -> Circle1
goRight1 c = c { center1 = (\p -> p { x1 = x1 p + 10 }) (center1 c) }

{-
Uuuuuuuggggly!

cannot use record syntax to create point-free functions

setX10 :: Point -> Point
setX10 p = p { x = 10 } -- valid
setX10'  =   { x = 10 } -- invalid!

workaround:

getX :: Point -> Double
getX = x

setX :: Double -> Point -> Point
setX x' p = p { x = x' }

but Haskell equivalent of Java code:

Java   : p.setX(p.getX() + 10)

Haskell: setX (getX p + 10) p

Using Haskell's record syntax in code becomes unmaintainable quickly.

Lenses

a property consists of a getter and setter
packaged into a single data type
called a Lens (instead of a property)
-}

--                getter,  setter
type Lens a b = ( a -> b , b -> a -> a )

-- now more general get and set functions:

getL :: Lens a b -> a -> b
getL (g, _) = g
-- or  getL = fst

setL :: Lens a b -> b -> a -> a
setL (_, s) = s
-- or  setL = snd

-- Apply a function to the field
-- modL more fundamental than setL
modL :: Lens a b -> (b -> b) -> a -> a
modL l f a = setL l (f (getL l a)) a

x' :: Lens Point0 Double
x' = (getX0, setX0)
{-
>>> getL x' (Point 3.0 4.0)
3.0
>>> setL x' 10.0 (Point 3.0 4.0)
Point {x = 10.0, y = 4.0}

slight improvment on Java notation,  but behind C#

now use syntactic support for operators (i.e., infix functions):
-}

(^.) :: a -> Lens a b -> b
a ^. p     = getL p a
-- or (^.) = flip getL

(^=) :: Lens a b -> b -> a -> a
(p ^= b) a = setL p b a
 -- or (^=) = setL
{-
>>> (Point 3.0 4.0) ^. x'
3.0
>>> (x' ^= 10.0) (Point 3.0 4.0)
Point {x = 10.0, y = 4.0}

better - still resembles functional style, esp setter
want more imperative look, so use State
-}

-- SET: stateful version of (^=)
(^:=) :: Lens a b -> b -> State a ()
p ^:= b = do
    a <- get -- read the current state
    let a' = setL p b a
    put a'   -- set the current state
-- or p ^:= b = modify (setL p b)

-- GET
access :: Lens a b -> State a b
access p = do
    a <- get -- read the current state
    let b = getL p a
    return b -- return the value
-- or access p = gets (getL p)

-- the rest via the above two primitives:

-- modify field 'p' using function f
(%=) :: Lens a b -> (b -> b) -> State a ()
p %= f = do
    b <- access p
    p ^:= f b

(+=) :: Num b => Lens a b -> b -> State a ()
p += x = p %= (+ x)

(*=) :: Num b => Lens a b -> b -> State a ()
p *= x = p %= (* x)

y' :: Lens Point0 Double
y' = (getY0, setY0)

goUp :: State Point0 ()
goUp = y' += 7.0

-- >>> :type (execState goUp)
-- execState goUp :: Point -> Point
-- >>> execState goUp (Point 3.0 4.0)
-- Point {x = 3.0, y = 11.0}

{-
Edward Kmett : data-lens
- his Lens type is more efficient/elegant
- his operators use different symbols

Categories

Category design pattern : anything that implements composition and identity:
-}

class Category c where
    compose  :: c y z -> c x y -> c x z
    identity :: c x x

-- functions form a Category
instance Category (->) where
    compose f g x = f (g x)
    identity x = x

-- lenses form a Category
-- TODO
-- instance Category Lens where
--     compose  = undefined
--     identity = undefined
{-
-- Category Laws:
Left Identity: id . f = f
Right Identity: f . id = f
Associativity: (f . g) . h = f . (g . h)

Since Lenses are categories we can compose them:
-}

ex1 x = getL id x == id x

ex2 lens1 lens2 x = getL (lens1 . lens2) x
                    ==
                    (getL lens1 . getL lens2) x
-- i.e. getL defines a Functor

{-
modL l id = id
modL l (f . g) = modL l f . modL l g
-- (modL l) defines a Functor, too
In fact, you can use the above two definitions to derive the Category instance for Lens.

This leads us to Haskell's verbatim translation of our original goRight function:
goRight = (x' . center') += 10

>>> :type x'
x' :: Lens Point Double
>>> :type center'
center' :: Lens Circle Point
>>> :type (x' . center')
x' . center' :: Lens Circle Double
>>> let c = Circle (Point 3.0 4.0) 5.0
>>> c ^. (x' . center)
3.0
>>> c ^. center' ^. x'
3.0
>>> execState goRight c
Circle {center = Point {x = 13.0, y = 4.0}, radius = 5.0}
The Category class enforces seamless composition because of the Category laws. For example, when we compose two lenses, we can't "uncompose" the result because it is absolutely indistinguishable from the equivalent hand-written lens. This is what "seamless" means: the "seam" between the two lenses completely vanishes when we compose them. The Category design pattern guarantees leak-proof abstraction.

Haskell lenses provide several advantages over object-oriented languages:
Haskell's lenses behave like "first-class l-values": You can pass lenses as arguments to functions, modify them or compose them and despite all of this you can still assign to the resulting expression. You don't need to read a long language specification to know whether or not a certain expression qualifies as something assignable. If it's a Lens, it's assignable, period.
Haskell lenses are implemented using ordinary, pure functions. Haskell language designers never built Lens into the language, which means that if you disagree with its implementation, you can implement your own version! In fact, Hackage lists several lens packages, although fclabels and data-lens are in my opinion the two best implementations at the moment.
Lenses are easier to combine. You don't have to write a new getter and setter to combine two lenses. Just compose them and you're done. Use all that time you save to do whatever it is Haskell programmers do with all the free time they supposedly have.
Lenses still require boilerplate lens definitions, but many solutions exist to this problem. Most lens packages provide Template Haskell macros to automatically derive lenses for records. That means that the final idiomatic Haskell equivalent to the original C# code would be:
data Point = Point { _x :: Double, _y :: Double }
data Circle = Circle { _center :: Point, _radius :: Double }
$( makeLenses [''Point, ''Circle] ) -- <- Template Haskell

goRight = (x . center) += 10
Template Haskell is poorly thought out in my opinion, but there are some ideas to replace it with a much more powerful template system that can be statically checked. That makes a very interesting topic in its own right, which I'll visit in a later post.
-}
