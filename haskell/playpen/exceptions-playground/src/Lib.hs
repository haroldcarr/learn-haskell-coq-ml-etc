{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( failWithMsg
    , failWithoutMsg
    ) where

import           Assert                   (withMessage)
import           Control.Exception.Assert (assert, assertMessage)

failWithMsg :: (Int, Maybe Int)
failWithMsg = withMessage (assert False) "failWithMsg" (3, Nothing)

failWithoutMsg :: (Int, Maybe Int)
failWithoutMsg = assertMessage "failWithoutMsg" "Z" (assert False) (4, Just 5)
