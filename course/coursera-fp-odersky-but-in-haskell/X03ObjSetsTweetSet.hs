{-
Created       : 2013 Oct 01 (Tue) 13:26:19 by carr.
Last Modified : 2013 Oct 01 (Tue) 18:57:45 by carr.
-}

module X03ObjSetsTweetSet where

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
   if isEmptyTS that then this
   else incl this (headTS that) `union` (tailTS that)

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

isEmptyTS :: TweetSet -> Bool
isEmptyTS Empty = True
isEmptyTS (NonEmpty _ _ _) = False

headTS :: TweetSet -> Tweet
headTS Empty = error "headTS Empty"
headTS (NonEmpty e l r) = if isEmptyTS l then e else headTS l

tailTS :: TweetSet -> TweetSet
tailTS Empty = error "tailTS Empty"
tailTS (NonEmpty e l r) = if isEmptyTS l then r else NonEmpty e (tailTS l) r

incl :: TweetSet -> Tweet -> TweetSet
incl Empty t = NonEmpty t Empty Empty
incl this@(NonEmpty elem@(Tweet _ elemText _) l r) x@(Tweet _ xText _) =
    if         xText < elemText then NonEmpty elem (incl l x) r
    else if elemText <    xText then NonEmpty elem l          (incl r x)
    else                             this

remove :: TweetSet -> Tweet -> TweetSet
remove Empty _ = Empty
remove    (NonEmpty elem@(Tweet _ elemText _) l r) x@(Tweet _ xText _) =
    if         xText < elemText then NonEmpty elem (remove l x) r
    else if elemText <    xText then NonEmpty elem l            (remove r x)
    else                              union l r

contains :: TweetSet -> Tweet -> Bool
contains Empty _ = False
contains (NonEmpty (Tweet _ elemText _) l r) x@(Tweet _ xText _) =
    if         xText < elemText then contains l x
    else if elemText <    xText then contains r x
    else                             True

size :: TweetSet -> Int
size Empty = 0
size (NonEmpty _ l r) = 1 + (size l) + (size r)

{-
//  ==============================================================================
object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  val all: TweetSet = TweetReader.allTweets

  def collect(l:List[String], acc:TweetSet): TweetSet =
      if (l.isEmpty) acc
      else collect(l.tail, acc.union(all.filter((x:Tweet) => x.text.contains(l.head))))

  lazy val googleTweets: TweetSet = collect(google, new Empty)
  lazy val appleTweets: TweetSet = collect(apple, new Empty)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: [Tweet] = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
-}
-- End of file.
