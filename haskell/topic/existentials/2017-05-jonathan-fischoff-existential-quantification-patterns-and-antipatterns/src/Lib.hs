{-# OPTIONS_GHC -fno-warn-type-defaults                  #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Lib where

------------------------------------------------------------------------------
import           Data.Char
import qualified Data.Foldable as F
import           Data.Kind
import           Prelude       hiding (sum)
import           Test.Hspec
------------------------------------------------------------------------------

{-
https://medium.com/@jonathangfischoff/existential-quantification-patterns-and-antipatterns-3b7b683b7d71

Existential Quantification Patterns and Antipatterns
Jonathan Fischoff
May 27, 2017

'ExistentialQuantification'
- can be used to hide a type variable
- use case : manipulate different types of values in a uniform way

Note: Existential quantification uses forall syntax just like RankNTypes.
But they are not similar extensions.

------------------------------------------------------------------------------
The Existential Type Class Antipattern

to make a list of things to be shown:

    map show [1, 'h', True] -- won't compile

Use ExistentialQuantification to make a list of showable things:
-}
data Showable = forall a. Show a => Showable a -- hides 'a'

-- this cannot be derived
instance Show Showable where
  show (Showable x) = show x

t01 :: Spec
t01  = do
  it "Showable" $ map show [Showable 1, Showable 'h', Showable True]
    `shouldBe`
    [show 1, show 'h', show True] -- could just have done this directly

{-
The above antipattern delays the use of type class methods with an additional wrapper type.
Could just make the function calls ahead of time.

Creating a uniform type for type class method application is not a good use of type classes.

------------------------------------------------------------------------------
Pattern 1: Intermediate Value Pattern

Hide a type that is used internally by a function(s), e.g.,

-- from foldl package
-}
data Fold a b
  -- |              step          initial   extract
  = forall x. Fold  (x -> a -> x) x         (x -> b)

sum :: Num a => Fold a a
sum  = Fold (+) 0 id

-- | Apply a strict left 'Fold' to a 'Foldable' container
fold :: F.Foldable f => Fold a b -> f a -> b
fold (Fold step begin done) as = F.foldr cons done as begin
  where
    cons a k x = k $! step x a

t02 :: Spec
t02  = it "Fold" $ fold sum [1,2,3] `shouldBe` 6

{-
- Fold holds initial value 'x' (existential)
- first time "step" called, it is given a value of type 'x' and returns a new value of type 'x'
- at the end the value of type 'x' is converted to type 'b' by "extract"

After creating a Fold, there is no way to tell what the type of 'x' is.

No need to expose type of x.
- Fold x a b (instead of Fold a b) makes library unnecessarily complicated to use

Another example : free applicative
https://hackage.haskell.org/package/free-4.12.4/docs/Control-Applicative-Free.html#g:1


------------------------------------------------------------------------------
Pattern 2: Unhide Hidden Types


When a type is hidden with an existential, generally it cannot be retrieved.

Pattern to make retrievable:
-}

-- do not export
data Hideable0 a
  = a ~ Int  => IntHideable0 a
  | a ~ Char => CharHideable0 a

-- Equivalent GADT
data Hideable a where
  IntHideable  :: Int  -> Hideable Int
  CharHideable :: Char -> Hideable Char

-- hide 'a' with an existential:
-- this would be exported
data AnyHideable = forall a. AnyHideable (Hideable a)
{-
pattern matching does not reveal the type

foo (AnyHideable x) = x
    • Couldn't match expected type ‘p’ with actual type ‘Hideable a’
        because type variable ‘a’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a pattern with constructor:
          AnyHideable :: forall a. Hideable a -> AnyHideable,

The "library/module" that exports 'AnyHideable' would also export
functions to operate on 'AnyHideable'

Users store, pass, etc., 'AnyHideable' around.
Then pass back to library for operations.
-}

incAndShow :: AnyHideable -> String
incAndShow  = \case
  AnyHideable (IntHideable  i) -> show          (i + 1)
  AnyHideable (CharHideable c) -> show (chr (ord c + 1))

t03 :: Spec
t03  = do
  it "unhide int" $ incAndShow (AnyHideable (IntHideable   0 )) `shouldBe`  "1"
  it "unhide chr" $ incAndShow (AnyHideable (CharHideable 'a')) `shouldBe` "'b'"
{-
bottom line : can regain type info

------------------------------------------------------------------------------
Pattern 3: Type Aligned Data Structures

Create lists, sequences, trees, etc., where new data and old data’s types match up.
- lists : https://hackage.haskell.org/package/thrist-0.3.0.2/docs/Data-Thrist.html
- https://hackage.haskell.org/package/type-aligned-0.9.6/docs/Data-TASequence.html
  - heterogeneous sequences where types enforce element order
  - see Reflection Without Remorse : http://okmij.org/ftp/Haskell/zseq.pdf

use-cases
- precisely typed programs
- algorithmic improvements


type aligned list for well typed paths between TCP states.

Uses DataKinds
- State is kind
- Closed, .. are types for the TCP states
- each transition in state diagram is constructor in GADT (the Path type).
-}

data State
  = Closed    | Listen  | SynSent  | SynReceived | Established
  | CloseWait | LastAck | FinWait1 | FinWait2    | Closing     | TimeWait

data Path :: State -> State -> Type where
  ServerStart          :: Path Closed      Closed
  PassiveOpen          :: Path Closed      Listen
  ServerClose          :: Path Listen      Closed
  ServerSynRecieved    :: Path Listen      SynReceived
  ClientSynRecieved    :: Path SynReceived SynReceived
  SynResponded         :: Path SynReceived Established
  ToFinWait1           :: Path Established FinWait1
  ActiveOpen           :: Path Closed      SynSent
  PassiveToActive      :: Path Listen      SynSent
  ClientClose          :: Path SynSent     Closed
  FinishHandshake      :: Path SynSent     Established
  PassiveClose         :: Path Established CloseWait
  ToLastAck            :: Path CloseWait   LastAck
  EarlyClose           :: Path SynReceived FinWait1
  ActiveClose          :: Path Established FinWait1
  SimulatanousClose    :: Path FinWait1    Closing
  SimulatanousCloseAck :: Path Closing     TimeWait
  FastClose            :: Path FinWait1    TimeWait
  CloseAcknowledge     :: Path FinWait1    FinWait2
  FinishClose          :: Path FinWait2    TimeWait
  Waited               :: Path TimeWait    Closed
  -- Existential quantification constructor hide intermediate type 'b'
  (:::)                :: Path a b -> Path b c -> Path a c

deriving instance Show (Path a b)

instance Eq (Path a b) where
  ServerStart          == ServerStart          = True
  PassiveOpen          == PassiveOpen          = True
  ServerClose          == ServerClose          = True
  ServerSynRecieved    == ServerSynRecieved    = True
  ClientSynRecieved    == ClientSynRecieved    = True
  SynResponded         == SynResponded         = True
  ToFinWait1           == ToFinWait1           = True
  ActiveOpen           == ActiveOpen           = True
  PassiveToActive      == PassiveToActive      = True
  ClientClose          == ClientClose          = True
  FinishHandshake      == FinishHandshake      = True
  PassiveClose         == PassiveClose         = True
  ToLastAck            == ToLastAck            = True
  EarlyClose           == EarlyClose           = True
  ActiveClose          == ActiveClose          = True
  SimulatanousClose    == SimulatanousClose    = True
  SimulatanousCloseAck == SimulatanousCloseAck = True
  FastClose            == FastClose            = True
  CloseAcknowledge     == CloseAcknowledge     = True
  FinishClose          == FinishClose          = True
  Waited               == Waited               = True
  -----
  ServerStart          == _           = False
  PassiveOpen          == _           = False
  ServerClose          == _           = False
  ServerSynRecieved    == _           = False
  ClientSynRecieved    == _           = False
  SynResponded         == _           = False
  ToFinWait1           == _           = False
  ActiveOpen           == _           = False
  PassiveToActive      == _           = False
  ClientClose          == _           = False
  FinishHandshake      == _           = False
  PassiveClose         == _           = False
  ToLastAck            == _           = False
  EarlyClose           == _           = False
  ActiveClose          == _           = False
  SimulatanousClose    == _           = False
  SimulatanousCloseAck == _           = False
  FastClose            == _           = False
  CloseAcknowledge     == _           = False
  FinishClose          == _           = False
  Waited               == _           = False
  -----
  ServerStart ::: ActiveOpen  == ServerStart ::: ActiveOpen  = True
  ActiveOpen  ::: ClientClose == ActiveOpen  ::: ClientClose = True
  (:::) ((:::) ServerStart ActiveOpen) ClientClose ==
    (:::) ((:::) ServerStart ActiveOpen) ClientClose         = True
  l                           == r                           =
    error ("Eq ::: not completely implemented " ++ show l ++ " " ++ show r)

-- Valid path that goes through Closed, SynSent, Closed states, in that order.
-- Intermediate 'SynSent' state must match up, but is not visible in the type.
connectAndClose :: Path Closed Closed
connectAndClose  = ServerStart ::: ActiveOpen ::: ClientClose

serverStart :: ([String], Path Closed Closed)
serverStart  = (["serverStart"], ServerStart)

activeOpen :: ([String], Path Closed Closed) -> ([String], Path Closed SynSent)
activeOpen (i,_) = ("activeOpen":i, ActiveOpen)

clientClose :: ([String], Path Closed SynSent) -> ([String], Path SynSent Closed)
clientClose (i,_) = ("clientClose":i, ClientClose)

connectAndCloseX :: ([String], Path Closed Closed)
connectAndCloseX  =
  let s@(_ ,ssp) = serverStart
      a@(_ ,aop) = activeOpen s
      (  cc,ccp) = clientClose a
   in (cc, ssp ::: aop ::: ccp)

t04 :: Spec
t04  = do
  it "TAS" $ connectAndCloseX `shouldBe`
    ( ["clientClose","activeOpen","serverStart"]
    , ServerStart ::: ActiveOpen ::: ClientClose)
