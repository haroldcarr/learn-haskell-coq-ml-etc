-- https://github.com/ekmett/free/blob/master/examples/ValidationForm.hs

-- for 'doc'
{-# LANGUAGE FlexibleContexts #-}

module FA_EK_ValidationForm where

import           Control.Applicative.Free (Ap, liftAp, runAp, runAp_)
import           Control.Monad.State      (evalStateT, execStateT, get, modify,
                                           put, runStateT)
import           Control.Monad.Writer     (Sum (Sum), getSum, liftIO)
import           Data.Either.Unwrap       (fromRight)
import           System.IO                (hFlush, stdout)
import           Text.Printf              (printf)
import           Text.Read                (readEither)

{-# ANN module "HLint: ignore Eta reduce" #-}

-- | A single field of a form.
data Field a = Field { fName     :: String                    -- ^ Name.
                     , fValidate :: String -> Either String a -- ^ Pure validation function.
                     , fHelp     :: String                    -- ^ Help message.
                     , fExample  :: String                    -- ^ Example in put value.
                     }

-- | Build a form with a single field.
--       name      validation fun                 help      example
field :: String -> (String -> Either String a) -> String -> String -> Ap Field a
field n f h e = liftAp $ Field n f h e

-- | Singleton form accepting any input.
--        name      help      example
string :: String -> String -> String -> Ap Field String
string n h e = field n Right h e

-- | Singleton form accepting anything but mentioned values.
--           taken       name      help      example
available :: [String] -> String -> String -> String -> Ap Field String
available xs n h e = field n check h e
  where
    check x | x `elem` xs = Left "the value is not available"
            | otherwise   = Right x

-- | Singleton integer field form.
int :: String -> String -> Ap Field Int
int n e = field n readEither "an integer value" e

-- | Count fields in a form.
count :: Ap Field a -> Int
count = getSum . runAp_ (\_ -> Sum 1)

-- | Interactive input of a form.
-- Shows progress on each field.
-- Repeats field input until it passes validation.
-- Show help message on empty input.
-- runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
iIO :: Ap Field a -> IO a
iIO m = evalStateT (runAp inputField m) (1 :: Integer)
  where
    inputField f@(Field n g h _) = do
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

-- runAp :: Applicative g => (forall x. f x -> g x) -> Ap f a -> g a
iAP :: Applicative ap => Ap Field a -> ap a
iAP form0 = runAp inputField form0
  where
    inputField (Field n g _ _) = pure (fromRight (g n))

iAP' :: Applicative ap => Ap Field a -> ap a
iAP' form0 = runAp inputField form0
  where
    inputField (Field _ g _ e) = pure (fromRight (g e))

doc :: Monad m => Ap Field a -> m (a, String)
doc m = runStateT (runAp inputField m) ""
  where
    inputField (Field n g _ e) = do
        s <- get
        put (s ++ n)
        return (fromRight (g e))

-- | User datatype.
data User = User { userName     :: String
                 , userFullName :: String
                 , userAge      :: Int }
          deriving (Show)

type Three a b c = (a,b,c)
type ThreeStrings = Three String String String

thr :: a -> b -> c -> Three a b c
thr a b c = (a,b,c)

-- | (Ap Field) for User.
form :: [String] -> Ap Field User
form us = User <$> available us  "Username"  "any vacant username"  "(e.g. johnsmith)"
               <*> string        "Full name" "your full name"       "(e.g. John Smith)"
               <*> int           "Age"                              "31"

-- | (Ap Field) for User.
form' :: [String] -> Ap Field ThreeStrings
form' us = thr <$> available us  "Username"  "any vacant username"  "(e.g. johnsmith)"
               <*> string        "Full name" "your full name"       "(e.g. John Smith)"
               <*> string        "Age"       "your current age"     "34"

main :: IO ()
main = do
    user <- iIO (form ["bob", "alice"])
    putStrLn $ "Success: created: " ++ show user

off :: IO ThreeStrings
off = iAP (form' ["bob", "alice"])

mf :: Maybe ThreeStrings
mf = iAP (form' ["bob", "alice"])

lf :: [ThreeStrings]
lf = iAP (form' ["bob", "alice"])

dc :: Monad m => m (User, String)
dc = doc (form ["bob", "alice"])
