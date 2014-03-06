{-
Created       : 2013 Oct 01 (Tue) 13:26:19 by carr.
Last Modified : 2014 Mar 05 (Wed) 13:20:56 by Harold Carr.
-}

module FP03ObjSetsTweetSet where

import           Data.List (isInfixOf)

-- represent tweets.
type User = String
type Text = String
type Retweets = Int

data Tweet = Tweet User Text Retweets

instance Show Tweet where
    show (Tweet u t r) =
        "User: " ++ u ++ "\n" ++
        "Text: " ++ t ++ " [" ++ show r ++ "]"

-- A set of Tweet implemented as a binary search tree.
data TweetSet =
      Empty
    | NonEmpty Tweet TweetSet TweetSet

filter' :: (Tweet -> Bool) -> TweetSet -> TweetSet
filter' p ts = filterAcc ts Empty
  where
    filterAcc Empty              _ = Empty
    filterAcc (NonEmpty e l r) acc =
        let result = filterAcc l acc `union` filterAcc r acc
        in if p e then result `incl` e
           else        result

union :: TweetSet -> TweetSet -> TweetSet
union Empty x = x
union this that =
    if isEmpty that then this
    else incl this (hd that) `union` tl that
  where
    isEmpty Empty      = True
    isEmpty NonEmpty{} = False

    hd Empty = error "hd Empty"
    hd (NonEmpty e l _) = if isEmpty l then e else hd l

    tl Empty = error "tl Empty"
    tl (NonEmpty e l r) = if isEmpty l then r else NonEmpty e (tl l) r

mostRetweeted :: TweetSet -> Tweet
mostRetweeted Empty = error "NoSuchElementException"
mostRetweeted ts@(NonEmpty e _ _) = mostRetweetedAcc ts e
  where
    mostRetweetedAcc Empty currentMax = currentMax
    mostRetweetedAcc (NonEmpty e' l' r') currentMax =
        max' e' $ max' (mostRetweetedAcc l' currentMax) $ mostRetweetedAcc r' currentMax
      where
        max' x@(Tweet _ _ xRetweets) y@(Tweet _ _ yRetweets) = if xRetweets > yRetweets then x else y

descendingByRetweet :: TweetSet -> [Tweet]
descendingByRetweet Empty = []
descendingByRetweet ts =
    let most = mostRetweeted ts
    in most : descendingByRetweet (remove ts most)

-- sorted by tweet text
-- note: if more than one user has same text then only the first one is kept
incl :: TweetSet -> Tweet -> TweetSet
incl Empty t = NonEmpty t Empty Empty
incl this@(NonEmpty el@(Tweet _ elText _) l r) x@(Tweet _ xText _)
    | xText < elText = NonEmpty el (incl l x)   r
    | xText > elText = NonEmpty el l            (incl r x)
    | otherwise        = this

remove :: TweetSet -> Tweet -> TweetSet
remove Empty _ = Empty
remove    (NonEmpty el@(Tweet _ elText _) l r) x@(Tweet _ xText _)
    | xText < elText = NonEmpty el (remove l x) r
    | xText > elText = NonEmpty el l            (remove r x)
    | otherwise        = l `union` r

contains :: TweetSet -> Tweet -> Bool
contains Empty _ = False
contains (NonEmpty (Tweet _ elText _) l r) x@(Tweet _ xText _)
    | xText < elText = contains l x
    | xText > elText = contains r x
    | otherwise        = True

size :: TweetSet -> Int
size Empty = 0
size (NonEmpty _ l r) = 1 + size l + size r

foreach :: TweetSet -> (Tweet -> IO ()) -> IO ()
foreach Empty            _ = return ()
foreach (NonEmpty e l r) f = do
    f e
    foreach l f
    foreach r f

{-# ANN collectByKeywords "HLint: ignore Eta reduce" #-}
collectByKeywords :: TweetSet -> [String] -> TweetSet
collectByKeywords    tweetSet    keywords = foldr step Empty keywords
  where step kw acc = union acc $ filter' (\(Tweet _ text _) -> isInfixOf kw text) tweetSet

-- End of file.
