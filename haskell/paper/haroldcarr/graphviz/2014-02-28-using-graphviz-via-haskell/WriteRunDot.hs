{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

{-
Created       : 2014 Feb 26 (Wed) 18:54:30 by Harold Carr.
Last Modified : 2014 Mar 01 (Sat) 21:45:47 by Harold Carr.
-}

module WriteRunDot where

import           Control.Monad (forM_)
import           Data.GraphViz
import qualified Data.Text     as T

doDots :: PrintDotRepr dg n => [(T.Text, dg n)] -> IO ()
doDots cases = forM_ cases createImage

createImage :: PrintDotRepr dg n => (T.Text, dg n) -> IO FilePath
createImage (n, g) = createImageInDir "/tmp" n Png g

createImageInDir :: PrintDotRepr dg n => T.Text -> T.Text -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = w $ mkFilePath $ T.toLower (T.pack (show o))
    where mkFilePath e = T.unpack $ T.concat [d,"/",n,".",e]
          w fp         = runGraphvizCommand Dot g o fp

-- End of file.
