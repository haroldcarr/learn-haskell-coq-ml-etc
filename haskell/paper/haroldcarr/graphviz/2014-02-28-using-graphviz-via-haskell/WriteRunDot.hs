{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Feb 28 (Fri) 19:17:31 by Harold Carr.
-}

module WriteRunDot where

import           Control.Monad          (forM_)
import           Data.GraphViz
import           Data.GraphViz.Printing
import qualified Data.Text              as T
import           Data.Text.Lazy         as L
import           Shelly
default (T.Text)

writeDot :: PrintDot a => (T.Text, a) -> Sh ()
writeDot ng = writeDotToDir "/tmp" ng

writeDotToDir :: PrintDot a => T.Text -> (T.Text, a) -> Sh ()
writeDotToDir d (n,g) =
    writefile (fromText (mkFileName d n "dot"))
              (T.pack (unpack (renderDot $ toDot g)))

runDot :: T.Text -> Sh ()
runDot n = runDotFromTo "/tmp" "/tmp" n "png"

runDotFromTo :: T.Text -> T.Text -> T.Text -> T.Text -> Sh ()
runDotFromTo f t n e = do
    let from = mkFileName f n "dot"
    let to   = mkFileName t n e
    run_ "dot" [T.append "-T" e, from, "-o", to]

--doDots :: [(T.Text, G.DotGraph L.Text)] -> Sh ()
doDots :: PrintDot a => [(T.Text, a)] -> Sh ()
doDots cases = forM_ cases (\x -> do writeDot x; (runDot . fst) x)

mkFileName :: T.Text -> T.Text -> T.Text -> T.Text
mkFileName d n e = T.concat [d,"/",n,".",e]

-- End of file.
