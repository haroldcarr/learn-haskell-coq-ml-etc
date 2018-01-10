{-# LANGUAGE TemplateHaskell #-}

module Assert
  (
    withMessage
  )
where

import           Control.Exception.Assert (assertMessage)
import           Development.GitRev       (gitHash)

mkMsg :: String -> String
mkMsg m = m ++ " [" ++ take 8 $(gitHash) ++ "]"

withMessage :: (a -> a) -> String -> a -> a
withMessage arse msg = assertMessage (mkMsg msg) "ASSERTION-FAILURE" arse
{-# INLINE withMessage #-}
