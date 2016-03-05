-- https://github.com/ekmett/free/blob/master/examples/ValidationForm.hs

module ValidationForm where


import           Control.Applicative.Free
import           Control.Monad.State
import           Control.Monad.Writer
import           System.IO
import           Text.Printf
import           Text.Read                (readEither)

{-# ANN module "HLint: ignore Eta reduce" #-}

-- | A single field of a form.
data Field a = Field { fName     :: String                    -- ^ Name.
                     , fValidate :: String -> Either String a -- ^ Pure validation function.
                     , fHelp     :: String                    -- ^ Help message.
                     }

-- | Build a form with a single field.
--       name      validation fun                 help msg
field :: String -> (String -> Either String a) -> String -> Ap Field a
field n f h = liftAp $ Field n f h

-- | Singleton form accepting any input.
--        name      help
string :: String -> String -> Ap Field String
string n h = field n Right h

-- | Singleton form accepting anything but mentioned values.
--           taken       name      help
available :: [String] -> String -> String -> Ap Field String
available xs n h = field n check h
  where
    check x | x `elem` xs = Left "the value is not available"
            | otherwise   = Right x

-- | Singleton integer field form.
int :: String -> Ap Field Int
int n = field n readEither "an integer value"

-- | Count fields in a form.
count :: Ap Field a -> Int
count = getSum . runAp_ (\_ -> Sum 1)

-- | Interactive input of a form.
-- Shows progress on each field.
-- Repeats field input until it passes validation.
-- Show help message on empty input.
iIO :: Ap Field a -> IO a
iIO m = evalStateT (runAp inputField m) (1 :: Integer)
  where
    inputField f@(Field n g h) = do
        i <- get
        -- get field input with prompt
        x <- liftIO $ do
            putStr $ printf "[%d/%d] %s: " i (count m) n
            hFlush stdout
            getLine
        case words x of
            -- display help message for empty input
            [] -> do
                liftIO . putStrLn $ "help: " ++ h
                inputField f
            -- validate otherwise
            _ -> case g x of
                Right y -> do
                    modify (+ 1)
                    return y
                Left  e -> do
                    liftIO . putStrLn $ "error: " ++ e
                    inputField f

fromRight :: Either l r -> r
fromRight (Right r) = r
fromRight _         = error "fromRight"

iAP :: Applicative ap => Ap Field a -> ap a
iAP form0 = runAp inputField form0
  where
    inputField f@(Field n g h) = pure (fromRight (g n))

-- | User datatype.
data User = User { userName     :: String
                 , userFullName :: String
                 , userAge      :: Int }
          deriving (Show)

type Three a b c = (a,b,c)
type ThreeStrings = Three String String String

thre :: a -> b -> c -> Three a b c
thre a b c = (a,b,c)

-- | (Ap Field) for User.
form :: [String] -> Ap Field User
form us = User <$> available us  "Username"  "any vacant username"
               <*> string        "Full name" "your full name (e.g. John Smith)"
               <*> int           "Age"

-- | (Ap Field) for User.
form' :: [String] -> Ap Field ThreeStrings
form' us = thre<$> available us  "Username"  "any vacant username"
               <*> string        "Full name" "your full name (e.g. John Smith)"
               <*> string        "Age"       "your current age"

main :: IO ()
main = do
    user <- iIO (form ["bob", "alice"])
    putStrLn $ "Success: created: " ++ show user

main' :: IO ()
main' = do
    user <- iAP (form' ["bob", "alice"])
    putStrLn $ "Success: created: " ++ show user

off :: IO ThreeStrings
off = iAP (form' ["bob", "alice"])

mf :: Maybe ThreeStrings
mf = iAP (form' ["bob", "alice"])

lf :: [ThreeStrings]
lf = iAP (form' ["bob", "alice"])

