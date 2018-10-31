{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}

module IJ where

{-
    icelandj / Pattern Synonyms for Dates and an IRC Bot

Sections

    Introduction to Pattern Synonyms
        Example with dates
        More complicated dates
        Accessing values
    IRC
        Setting the stage
        “Ping — Pong” Pattern
        Glad you could (μ : M² → M) us
        Responding to messages
        Commands
        Responding to PMs
    Some ideas
        Infix constructors
        Variadic patterns

------------------------------------------------------------------------------
Introduction to Pattern Synonyms

PatternSynonyms : abstract away from data type impl

Combined with ViewPatterns : enable moving some guard/case logic into patterns

Example with dates

example from Abstract Value Constructors: Symbolic Constants for Standard ML (PDF) *TODO*
-}

data Date = Date
  { month :: Int
  , day   :: Int
  } deriving Show

-- before 7.8, to match on months, a better representation is

data DateM = JanuaryM Int | FebruaryM Int | DecemberM Int

-- PatternSynonyms : NewPattern = OldPattern

-- Months
pattern January  day = Date { month =  1, day = day }
pattern February day = Date { month =  2, day = day }
pattern March    day = Date { month =  3, day = day }
pattern December day = Date { month = 12, day = day }

-- Holidays
pattern Christmas    = Date { month = 12, day = 25  }

-- then match NewPattern
describe :: Date -> String
describe (January  1) = "First day of year"
describe (February n) = show n ++ "th of February"
describe Christmas    = "Presents!"
describe _            = "meh"

{-
Pattern matching on Christmas same as matching on (Date 12 25).
Without PatternSynonyms, only possible to match on data constructors.
With PatternSynonyms can match independently of underlying representation.

ghci> describe Date { month = 12, day = 25 }
"Presents!"
ghci> describe Christmas
"Presents!"
ghci> describe Date { month = 2,  day = 5  }
"5th of February"
ghci> describe (February 10)
"10th of February"
ghci> March 5
Date { month = 3, day = 5 }

can define Christmas in terms of the pattern December
-}
pattern ChristmasToo = December 25
{-
Christmas, ChristmasToo, December 25, Date 12 25 are interchangable

examples so far are bidirectional patterns because
- can be used as both patterns and expressions
- not true of all patterns

------------------------------------------------------------------------------
More complicated dates

example: match days of December leading up to and following Christmas

use ViewPatterns and use uni-directional patterns

Uni-directional patterns are defined using pattern and an arrow <- rather than =
-}

-- bidirectional pattern (DecemberHC) used in following patterns
pattern DecemberHC day = Date { month = 12, day = day }

-- uni-directional patterns
-- (cannot be used as expressions :  what would the values of BeforeChristmas and AfterChristmas be?)
-- view patterns to compare given day to 25
pattern BeforeChristmas1 <- DecemberHC (compare 25 -> GT)
pattern ChristmasHC1     <- DecemberHC (compare 25 -> EQ)
pattern AfterChristmas1  <- DecemberHC (compare 25 -> LT)

react :: Date -> String
react BeforeChristmas1 = "Waiting :("
react ChristmasHC1     = "Presents!"
react AfterChristmas1  = "Have to wait a whole year :("
react _                = "It's not even December..."

-- equivalent to:

react' :: Date -> String
react' (Date 12 (compare 25 -> GT)) = "Waiting :("
react' (Date 12 (compare 25 -> EQ)) = "Presents!"
react' (Date 12 (compare 25 -> LT)) = "Have to wait a whole year :("
react' _                            = "It's not even December..."
{-
------------------------------------------------------------------------------
using view patterns but no pattern synonyms

could have used different predicates, e.g.,
- December ((< 25) -> True)
- December 25
- December ((> 25) -> True)
- requires running a new predicate for each clause
- "compare" design only applies predicate 'compare 25' once
- part of the GHC user guide) producing something like this:

react date = case date of
  Date 12 day -> case compare 25 day of
    GT -> "Waiting :("
    EQ -> "Presents!"
    LT -> "Have to wait a whole year :("
  _           -> "It's not even December..."

------------------------------------------------------------------------------
Accessing values

can retrieve record using as-pattern
-}
days'tilChristmas :: Date -> Int
days'tilChristmas d@BeforeChristmas1 = 25 - day d
days'tilChristmas   Christmas       = 0
days'tilChristmas d@AfterChristmas1  = 365 + 25 - day d

-- also:

isItNow :: Int -> (Ordering, Int)
isItNow day = (compare 25 day, day)

pattern BeforeChristmas2 day <- December (isItNow -> (GT, day))
pattern Christmas2           <- December (isItNow -> (EQ, _))
pattern AfterChristmas2  day <- December (isItNow -> (LT, day))

days'tilChristmas2 :: Date -> Int
days'tilChristmas2 (BeforeChristmas2 n) = 25 - n
days'tilChristmas2 Christmas            = 0
days'tilChristmas2 (AfterChristmas2 n)  = 365 + 25 - n
days'tilChristmas2 _                    = undefined
{-
Exercise: Create a pattern where Unix time can be used to match Date value.
          something like Epoch 1419470000 should match Christmas

Exercise: Represent date with a single Unix timestamp
          enable matching on it with March 5 and
          ThirdOf which matches the third day of any month

------------------------------------------------------------------------------
IRC

To build the bot we need some basic commands:

import Control.Monad
import Network
import System.IO
-- show
-- Choose a nick
nick :: Handle -> String -> IO ()
nick h name = hPutStrLn h ("NICK " ++ name)

-- Specify username
user :: Handle -> String -> IO ()
user h name = hPutStrLn h ("USER " ++ name ++ " 0 * :" ++ name)

-- Join a channel
joinChan :: Handle -> String -> IO ()
joinChan h chan = hPutStrLn h ("JOIN " ++ chan)

Now we can connect to the server and run our action forever

main = do
  h <- connectTo "irc.freenode.org" (PortNumber 6667)
  hSetBuffering   h NoBuffering
  hSetNewlineMode h (NewlineMode CRLF CRLF)

  nick h "PatternBot"
  user h "PatternBot"

  joinChan h "##patternsynonyms"

  forever (action h)

For action = hGetLine >=> putStrLn the bot should identify itself, join ##patternsynonyms and output everything it receives.
“Ping — Pong” Pattern

If the server says PING you must say PONG!

An example PING command may look like

PING :orwell.freenode.net

meaning that you need to respond with

PONG :orwell.freenode.net

to let it know we're still there. Here we can use our patterns!

pattern Ping serv <- (words -> ["PING", serv])

this pattern only matches two-word PING commands and gives us the server we need to include in our PONG. Now action turns into:

pong :: Handle -> String -> IO ()
pong h serv = hPutStrLn h ("PONG " ++ serv)

action :: Handle -> IO ()
action h = do
  line <- hGetLine h
  case line of
    {-hi-}PING serv -> pong h serv{-/hi-}
    _         -> return ()

and we can treat line :: String as if it were a data type of IRC messages.
Glad you could (μ : M² → M) us

If new people come to our channel we want them to feel welcome so the bot should greet people as they join. The JOIN message looks something like this

:<nick>!<user>@<host> JOIN <channel>

We want to know who joined what channel so let's parse that in an ad-hoc way (there are packages on Hackage that do this properly). The nick goes from the initial colon to the exclamation mark:

-- /show
import Data.List
-- show
getNick :: String -> Maybe String
getNick (':':prefix) = do
  index <- findIndex (== '!') prefix
  return (take index prefix)
getNick _            = Nothing

Now we create the pattern for joins (and for our bot) and integrate them into the logic

pattern PBot = "PatternBot"
pattern JOIN nick chan
   <- (words -> [getNick -> Just nick, "JOIN", chan])

msg :: Handle -> String -> IO ()
msg h chan msg = hPutStrLn h ("PRIVMSG " ++ chan ++ " :" ++ msg)

action :: Handle -> IO ()
action h = do
  line <- hGetLine h
  case line of
    PING serv      -> pong h serv
    -- Greet channel when we join
    {-hi-}JOIN PBot chan -> msg h chan "Halló, heimur!"{-/hi-}
    -- Greet nicks that join
    {-hi-}JOIN nick chan -> msg h chan (nick ++ ": Welcome to " ++ chan){-/hi-}
    _              -> return ()

The JOIN pattern definition is not very pretty but we're not concerned with that.
Responding to messages

Messages are either sent to a channel or to a single user (private message) and are either:

:<nick>!<user>@<host> PRIVMSG <channel> :<msg>
:<nick>!<user>@<host> PRIVMSG <nick>    :<msg>

We would like to pick out the sender, the message and the target channel or nick:

getPriv :: String -> Maybe (String, String, String)
getPriv msg = case words msg of
  sender : "PRIVMSG" : target : (':':_) : _ -> do
    nick <- getNick sender
    return (nick, target, clean msg)
  _ -> Nothing
  where
  clean = tail . dropWhile (/=':') . dropWhile (/= ' ') . tail

Now we create two patterns that determine whether something is a nick or a channel:

pattern Nick n <- ((\a -> (head a /= '#', a)) -> (True, n))
pattern Chan c <- ((\a -> (head a == '#', a)) -> (True, c))

Yuck. Anyway, we can use these to define the desired patterns

-- Private message to our bot
pattern PM from m <- (getPriv -> Just (from, Nick PBot,  m))

-- Message to channel
pattern MSG from to m <- (getPriv -> Just (from, Chan to, m))

This is so nice is almost absolves me of the horrible code above :) but the good thing is that we can replace the underlying representation with a data type provided by some IRC parsing library without having to change the actual action code! Now let's put MSG to use: if anyone mentions “cats” we respond:

-- /show
import Data.Char
-- show
-- Matches any cat
pattern Cat <- (isInfixOf "cat" . map toLower -> True)

-- …
case line of
  MSG _ chan Cat -> msg h chan "Meow!"

Commands

Now we may want to allow users to run commands starting with > :

-- /show
{-# LANGUAGE ScopedTypeVariables #-}
import System.Random
-- show
pattern Command cmd = '>':' ':cmd

pattern Roll <- Command (map toLower -> "roll")

-- …
case line of
  MSG from chan Roll -> do
    roll :: Int <- randomRIO (1, 6)
    msg h chan (from ++ ": You rolled " ++ show roll)

It's now easy to add additional commands.
Responding to PMs

Bots are snarky

case line of
  PM from m -> msg h from ("You said \"" ++ m ++ "\" to me?!")

and now we can have a complete session:

*** PatternBot (~xxxx) has joined channel ##patternsynonyms
<PatternBot> Halló, heimur!
*** SomeNick (~yyyy) has joined channel ##patternsynonyms
<PatternBot> SomeNick: Welcome to ##patternsynonyms
*** SomeNick (~yyyy) has left channel ##patternsynonyms
<Iceland_jack> PatternBot: hey
<Iceland_jack> I should learn category theory
<PatternBot> Meow!
<Iceland_jack> > roll
<PatternBot> Iceland_jack: You rolled 3

and the core logic looks something like this

action :: Handle -> IO ()
action h = do
  line <- hGetLine h
  case line of
    PING serv           -> pong h serv
    JOIN PBot chan      -> msg h chan "Halló, heimur!"
    JOIN nick chan      -> msg h chan (nick ++ ": Welcome to " ++ chan)
    PM   from m         -> msg h from ("You said \"" ++ m ++ "\" to me?!")
    MSG  from chan Cat  -> msg h chan "Meow!"
    MSG  from chan Roll -> do
      roll :: Int <- randomRIO (1, 6)
      msg h chan (from ++ ": You rolled " ++ show roll)
    _                   -> return ()

This example is absolutely overusing pattern synonyms it but it ends up being quite pleasant.
Some ideas
Infix constructors

There are some things that would be nice to have: currently you can't pattern match on infix non-binary operators

-- This works
data Foo = (:⇒) Sender Recipient Message

pattern (:→) a b c = (:⇒) a b c
pattern To   a b c = (:⇒) a b c

-- This works
msg₁ :: Foo
msg₁ = ("Alice" :→ "Bob") "ossifrage"

msg₂ :: Foo
msg₂ = ("Bob" :→ "Alice") "pasta"

-- But {-hi-}this doesn't{-/hi-}
foo msg = case msg of
  ("Alice" :⇒ "Bob"  ) msg → …
  ("Bob"   :→ "Alice") msg → …
  (a      `To` b     ) msg → …

Replace Alice and Bob with nicks and channels and it makes sense for the IRC bot if you're into that sort of thing.

------------------------------------------------------------------------------
Variadic patterns

More controversial, since patterns are always fully applied they could as well be variadic:

pattern PING             <- (words -> ["PING"])
pattern PING serv        <- (words -> ["PING", serv])
pattern PING serv₁ serv₂ <- (words -> ["PING", serv₁, serv₂])

It would also allow us to define the BeforeChristmas and AfterChristmas both ways

pattern BeforeChristmas     <- December (isItNow -> (GT, _))
pattern BeforeChristmas day <- December (isItNow -> (GT, day))

pattern AfterChristmas      <- December (isItNow -> (LT, day))
pattern AfterChristmas  day <- December (isItNow -> (LT, day))

or any other pattern that may be used for pattern matching only or accessing some value as well:

parity :: Int -> (Bool, Int)
parity n = (even n, n `div` 2)

pattern Even   <- (parity -> (True, _))
pattern Even n <- (parity -> (True, n))

pattern Odd    <- (parity -> (False, _))
pattern Odd n  <- (parity -> (False, n))
-}