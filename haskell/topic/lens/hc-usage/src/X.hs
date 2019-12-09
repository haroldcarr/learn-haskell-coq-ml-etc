{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module X where

------------------------------------------------------------------------------
import           Control.Lens
-- import qualified Control.Lens.Internal.FieldTH  as FTH
-- import           Control.Lens.TH
-- import           Control.Monad.Trans.RWS.Strict
-- import qualified Data.Set                       as Set
-- import qualified Language.Haskell.TH.Datatype   as THD
-- import qualified Language.Haskell.TH.Lib        as THL
-- import qualified Language.Haskell.TH.Syntax     as THS
import qualified Prelude
import           Protolude                      hiding (get, gets, round, to)
------------------------------------------------------------------------------

{-# ANN module ("HLint: ignore Reduce duplication" :: Prelude.String) #-}
{-# ANN module ("HLint: ignore Redundant return" :: Prelude.String) #-}

newtype Author    = Author   { _getAuthor   :: Text             } deriving (Eq, Ord, Show)
newtype Epoch     = Epoch    { _epochEpoch    :: Int              } deriving (Eq, Num, Ord, Show)
newtype HashValue = HashValue{ _getHashValue:: ByteString       } deriving (Eq, Ord, Show)
newtype Round     = Round    { _getRound    :: Int              } deriving (Eq, Num, Ord, Show)

makeClassyFor "RWAuthor" "lAuthor" [("_getAuthor", "getAuthor")] ''Author
makeClassyFor "RWEpoch" "lEpoch" [("_epochEpoch", "getEpoch")] ''Epoch
makeClassyFor "RWHashValue" "lHashValue" [("_getHashValue", "getHashValue")] ''HashValue
makeClassyFor "RWRound" "lRound" [("_getRound", "getRound")] ''Round
makeFields ''Author
makeFields ''Epoch
makeFields ''HashValue
makeFields ''Round

data BlockInfo = BlockInfo
  { _biAuthor :: !Author
  , _biEpoch  :: !Epoch
  , _biRound  :: !Round
  , _biId     :: !HashValue
  } deriving (Eq, Show)
makeClassyFor "RWBlockInfo" "lBlockInfo"
  [ ("_biAuthor" , "biAuthor")
  , ("_biEpoch"  , "biEpoch")
  , ("_biRound"  , "biRound")
  , ("_biId"     , "biId") ]
  ''BlockInfo
makeFields ''BlockInfo

data VoteData = VoteData
  { _vdProposed :: !BlockInfo
  , _vdParent   :: !BlockInfo
  } deriving (Eq, Show)
makeClassyFor "RWVoteData" "lVoteData"
  [ ("_vdProposed", "vdProposed")
  , ("_vdParent"  , "vdParent") ]
  ''VoteData
makeFields ''VoteData

foo :: Author -> Epoch -> HashValue -> Round -> BlockInfo -> VoteData -> Text
foo author epoch hashValue round blockInfo voteData =
  show (author, epoch, hashValue, round, blockInfo, voteData)
  <>
  show ( author^.getAuthor, epoch^.getEpoch, hashValue^.getHashValue, round^.getRound
       , blockInfo^.biAuthor
       , voteData^.vdProposed)

bar :: (RWBlockInfo x, RWVoteData x) => x -> Text
bar  x = show (x^.vdProposed) <> show (baz x)

baz :: RWBlockInfo x => x -> Text
baz  x = show (x^.biAuthor.getAuthor)

qux :: HasEpoch s Int => s -> Int
qux x = x^.epoch
{-
obmMakeClassyFor
  :: Prelude.String -> Prelude.String -> [(Prelude.String, Prelude.String)] -> THS.Name
  -> THL.DecsQ
obmMakeClassyFor clsName funName fields = FTH.makeFieldOptics $
  classyRulesFor (const (Just (clsName, funName))) fields

classyRulesFor
  :: (Prelude.String -> Maybe (Prelude.String, Prelude.String)) {- ^ Type Name -> Maybe (Class Name, Method Name) -} ->
  [(Prelude.String, Prelude.String)] {- ^ [(Field Name, Method Name)] -} ->
  LensRules
classyRulesFor classFun fields = classyRules
  & lensClass .~ (over (mapped . both) THS.mkName . classFun . THS.nameBase)
  & lensField .~ lookingupNamer fields

-- makeFieldOptics :: LensRules -> THS.Name -> THL.DecsQ
-- makeFieldOptics rules = (`evalStateT` Set.empty) . makeFieldOpticsForDatatype rules <=< THD.reifyDatatype
-}
{-
:set -XTemplateHaskell
THS.Q x = THD.reifyDatatype ''VoteData
THD.datatypeName x
-}
