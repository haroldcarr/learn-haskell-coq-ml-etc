{-
Created       : 2013 Oct 01 (Tue) 13:26:19 by carr.
Last Modified : 2013 Oct 02 (Wed) 15:30:28 by carr.
-}

module X03ObjSetsTweetSet where

import Data.List (isInfixOf)

-- represent tweets.
type User = String
type Text = String
type Retweets = Int

data Tweet = Tweet User Text Retweets

instance Show Tweet where
    show (Tweet u t r) =
        "User: " ++ u ++ "\n" ++
        "Text: " ++ t ++ " [" ++ (show r) ++ "]"

-- A set of Tweet implemented as a binary search tree.
data TweetSet =
      Empty
    | NonEmpty Tweet TweetSet TweetSet

filter' :: (Tweet -> Bool) -> TweetSet -> TweetSet
filter' p ts = filterAcc p ts Empty
  where
    filterAcc p Empty            acc = Empty
    filterAcc p (NonEmpty e l r) acc =
        let result = (filterAcc p l acc) `union` (filterAcc p r acc)
        in if p e then result `incl` e
           else        result

union :: TweetSet -> TweetSet -> TweetSet
union Empty x = x
union this that =
    if isEmpty that then this
    else incl this (head that) `union` (tail that)
  where
    isEmpty Empty = True
    isEmpty (NonEmpty _ _ _) = False

    head Empty = error "head Empty"
    head (NonEmpty e l r) = if isEmpty l then e else head l

    tail Empty = error "tail Empty"
    tail (NonEmpty e l r) = if isEmpty l then r else NonEmpty e (tail l) r

mostRetweeted :: TweetSet -> Tweet
mostRetweeted Empty = error "NoSuchElementException"
mostRetweeted ts@(NonEmpty e l r) = mostRetweetedAcc ts e
  where
    mostRetweetedAcc Empty currentMax = currentMax
    mostRetweetedAcc (NonEmpty e l r) currentMax =
        max e $ max (mostRetweetedAcc l currentMax) $ mostRetweetedAcc r currentMax
      where
        max x@(Tweet _ _ xRetweets) y@(Tweet _ _ yRetweets) = if (xRetweets > yRetweets) then x else y

descendingByRetweet :: TweetSet -> [Tweet]
descendingByRetweet Empty = []
descendingByRetweet ts =
    let most = mostRetweeted ts
    in most : (descendingByRetweet $ remove ts most)

-- sorted by tweet text
-- note: if more than one user has same text then only the first one is kept
incl :: TweetSet -> Tweet -> TweetSet
incl Empty t = NonEmpty t Empty Empty
incl this@(NonEmpty elem@(Tweet _ elemText _) l r) x@(Tweet _ xText _) =
    if      xText < elemText then NonEmpty elem (incl l x)   r
    else if xText > elemText then NonEmpty elem l            (incl r x)
    else                          this

remove :: TweetSet -> Tweet -> TweetSet
remove Empty _ = Empty
remove    (NonEmpty elem@(Tweet _ elemText _) l r) x@(Tweet _ xText _) =
    if      xText < elemText then NonEmpty elem (remove l x) r
    else if xText > elemText then NonEmpty elem l            (remove r x)
    else                          union l r

contains :: TweetSet -> Tweet -> Bool
contains Empty _ = False
contains (NonEmpty (Tweet _ elemText _) l r) x@(Tweet _ xText _) =
    if      xText < elemText then contains l x
    else if xText > elemText then contains r x
    else                          True

size :: TweetSet -> Int
size Empty = 0
size (NonEmpty _ l r) = 1 + (size l) + (size r)

foreach :: TweetSet -> (Tweet -> IO ()) -> IO ()
foreach Empty            f = return ()
foreach (NonEmpty e l r) f = do
    f e
    foreach l f
    foreach r f

collectByKeywords :: TweetSet -> [String] -> TweetSet
collectByKeywords    tweetSet    keywords = foldr step Empty keywords
  where step kw acc = union acc $ filter' (\(Tweet _ text _) -> isInfixOf kw text) tweetSet

-- End of file.
