module P2 where

-- 16:00

-- solution via lenses

-- lens basic usage
{-
view :: Lens source target -> (source -> target)
set  :: Lens source target -> target -> source -> source
(.)  :: Lens st -> Lens t u -> Lens s u
id   :: Lens a a
_1   :: Lens (a,b) a
_2   :: Lens (a,b) b
(_1 . _2) :: Lens ((c,d),e) d
view _1 (("hello" , Nothing), 3)
--> ("hello" , Nothing)
view (_1 . _1) (("hello", Nothing), 3)
 --> "hello"
set _2 1000 (("hello", Nothing), 3)
--> (("hello", Nothing), 1000)
set (_1 . _2) (Just "lens") (("hello", Nothing), 3)
--> (("hello", Just "lens"), 3)
-}

-- 20:17

-- prism

{-
Prism is like first-class pattern match.
Enables access to one branch of ADT

-- partical getter
preview :: Prism a b -> (a -> Maybe b)
-- constructor
review  :: Prism a b (b -> a)

-- 21:00
Prism a b says "a might be a b"

(.)      :: Presm s t -> Prism t u -> Prism s u
id       :: Prism a a
_Left    :: Prism (Either a b) a
_Right   :: Prism (Either a b) b
_Just    :: Prism (Maybe a) a
_Nothing :: Prism (Maybe a) ()

preview _Left (Left (Just 4))
--> (Just (Just 4))

preview (_Left . _Just) (Left (Just 4))
--> Just 4

preview _Right (Left (Just 4))
--> Nothing

-- 24:12

review (_Right . _Just) "hello"
--> Right (Just "hello")

review (_Just . _Left) 42
--> Just (Left 42)
-}

