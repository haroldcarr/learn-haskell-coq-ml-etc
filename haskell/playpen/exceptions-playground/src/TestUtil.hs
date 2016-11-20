module TestUtil where

import           Control.Exception
import           Data.Text         as T (isInfixOf, pack)
import           Text.Regex.Posix  ((=~))

-- | Confirm that the second argument is an exception containing a
-- | line number and column range and a git hash in square brackets,
-- | in addition to containing the specified string
assertionFailureContaining1 :: String -> Either SomeException a -> Bool
assertionFailureContaining1 str res =
  msg =~ ":[[:digit:]]+:[[:digit:]]+-[[:digit:]]+.*\\[[[:alnum:]]{8}\\]" &&
  T.isInfixOf (T.pack str) (T.pack msg)
 where msg = case res of
               Left ex  -> show ex
               _        -> "ThisStringShouldNotBeIncludedInAnyAssertionMessage"

-- | Confirm that the second argument is an exception containing a
-- line number and column range and a git hash in square brackets,
-- in addition to containing the specified string.
-- Returns (<True if exception>, Maybe <True if contains line-col-hash>, Maybe <True if contains string>)
assertionFailureContaining2 :: String -> Either SomeException a -> (Bool, Maybe Bool, Maybe Bool)
assertionFailureContaining2 str res =
  case res of
    Right _  -> (False, Nothing, Nothing)
    Left ex  -> let msg = show ex
                in if msg =~ ":[[:digit:]]+:[[:digit:]]+-[[:digit:]]+.*\\[[[:alnum:]]{8}\\]"
                     then (True, Just True, Just $ T.isInfixOf (T.pack str) (T.pack msg))
                     else (True, Just False, Nothing)
