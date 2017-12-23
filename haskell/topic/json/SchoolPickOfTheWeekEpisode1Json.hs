{-
Created       : 2013 Oct 01 (Tue) 19:10:35 by carr.
Last Modified : 2017 Dec 23 (Sat) 11:21:33 by Harold Carr.

https://www.fpcomplete.com/school/pick-of-the-week/episode-1-json
-}

{-# LANGUAGE OverloadedStrings #-}

module SchoolPickOfTheWeekEpisode1Json where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Monoid

-------------------------------------------------------------------------------

data Recipe = Recipe
  { recipeName  :: String
  , ingredients :: [Ingredient]
  , steps       :: [Step]
  } deriving Show

instance FromJSON Recipe where
    parseJSON (Object r) = Recipe <$>
                           r .: "name" <*>
                           r .: "ingredients" <*>
                           r .: "steps"
    parseJSON _ = mzero

instance ToJSON Recipe where
    toJSON (Recipe n i s) = object ["name" .= n, "ingredients" .= i, "steps" .= s]

-------------------------------------------------------------------------------

data Step = Step
  { stepName     :: String
  , order        :: Int
  , stepDuration :: Maybe Duration
  } deriving (Eq, Show)

instance Ord Step where
    compare s1 s2 = compare (order s1) (order s2)

instance FromJSON Step where
    parseJSON (Object s) = Step <$>
                           s .: "step" <*>
                           s .: "order" <*>
                           s .:? "duration"
    parseJSON _ = mzero

instance ToJSON Step where
    toJSON (Step s o d) = object ["step" .= s, "order" .= o, "duration" .= d]

-------------------------------------------------------------------------------

type Measure = String

data Ingredient = Ingredient
  { ingredientName :: String
  , quantity       :: Int
  , measure        :: Maybe Measure
  } deriving Show

instance FromJSON Ingredient where
    parseJSON (Object i) = Ingredient <$>
                           i .: "name" <*>
                           i .: "quantity" <*>
                           i .:? "measure"
    parseJSON _ = mzero

instance ToJSON Ingredient where
    toJSON (Ingredient n q m) = object ["name" .= n, "quantity" .= q, "measure" .= m]

-------------------------------------------------------------------------------

data Duration = Duration
  { duration        :: Int
  , durationMeasure :: Measure
  } deriving (Eq, Show)

instance FromJSON Duration where
    parseJSON (Object d) = Duration <$>
                           d .: "duration" <*>
                           d .: "measure"
    parseJSON _ = mzero

instance ToJSON Duration where
    toJSON (Duration d m) = object ["duration" .= d, "measure" .= m]

-------------------------------------------------------------------------------

main :: IO ()
main = do
    let toParse = C8.unlines $ map C8.pack [
                                    "{ ",
                                    "    \"name\": \"Ciambellone Cake\", ",
                                    "    \"ingredients\": [ ",
                                    "        { ",
                                    "            \"name\": \"Flour\", ",
                                    "            \"quantity\": 250, ",
                                    "            \"measure\": \"gr\" ",
                                    "        }, ",
                                    "        { ",
                                    "            \"name\": \"Sugar\", ",
                                    "            \"quantity\": 250, ",
                                    "            \"measure\": \"gr\" ",
                                    "        }, ",
                                    "        { ",
                                    "            \"name\": \"Sunflower Oil\", ",
                                    "            \"quantity\": 130, ",
                                    "            \"measure\": \"ml\" ",
                                    "        }, ",
                                    "        { ",
                                    "            \"name\": \"Water\", ",
                                    "            \"quantity\": 130, ",
                                    "            \"measure\": \"ml\" ",
                                    "        }, ",
                                    "        { ",
                                    "            \"name\": \"Egg\", ",
                                    "            \"quantity\": 3 ",
                                    "        }, ",
                                    "        { ",
                                    "            \"name\": \"Yeast\", ",
                                    "            \"quantity\": 1 ",
                                    "        } ",
                                    "    ], ",
                                    "    \"steps\": [ ",
                                    "        { ",
                                    "            \"step\": \"Mix everything\", ",
                                    "            \"order\": 1 ",
                                    "        }, ",
                                    "        { ",
                                    "            \"step\": \"Cook in oven at 200 degrees\", ",
                                    "            \"order\": 2, ",
                                    "            \"duration\": { ",
                                    "                \"duration\": 30, ",
                                    "                \"measure\": \"minutes\" ",
                                    "            } ",
                                    "        } ",
                                    "    ] ",
                                    "} "
                                ]
      in case (eitherDecode' toParse :: Either String Recipe) of
        Right r -> print r
        Left e  -> C8.putStrLn $ C8.pack e <> " in " <> toParse

-- End of file.
