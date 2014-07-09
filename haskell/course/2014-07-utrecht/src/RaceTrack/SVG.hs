{-
Created       : by Jan Rochel
Last Modified : 2014 Jul 08 (Tue) 01:41:34 by Harold Carr.
-}

{-# LANGUAGE OverloadedStrings #-}
module RaceTrack.SVG (module RaceTrack.SVG, Svg) where

import           Data.String                    (fromString)
import           RaceTrack.Types
import           Text.Blaze.Svg.Renderer.String (renderSvg)
import           Text.Blaze.Svg11               as S
import           Text.Blaze.Svg11.Attributes    as A

renderLine :: String -> Line -> Svg
renderLine colour ((x1,y1),(x2,y2)) =
	S.path ! stroke (fromString colour) ! strokeWidth "0.4" ! fill "none" ! (d $ mkPath $ m x1 y1 >> l x2 y2)

type Dimensions = (Int,Int,Int,Int) -- xmin ymin xmax ymax

linesToSvg :: FilePath -> Dimensions -> [Svg] -> IO ()
linesToSvg file (xmin,ymin,xmax,ymax) lines = writeFile file $ renderSvg $ docTypeSvg
	! version "1.1"
	! width "1000"
	! height "800"
	! preserveaspectratio "xMinYMin meet"
	! viewbox (fromString $ show xmin ++ " " ++ show ymin ++ " " ++ show xmax ++ " " ++ show ymax) $ do
		sequence_ lines

-- End of file.
