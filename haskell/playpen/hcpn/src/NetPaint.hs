{-# LANGUAGE FlexibleContexts #-}
module NetPaint where

import           Graphics.UI.WX
import           Graphics.UI.WXCore (getTextExtent)

import           Control.Concurrent
import           System.IO.Unsafe   (unsafePerformIO)

import           NetAux
import           NetData
import           WindowContext

-- paint the net, selection, and arcs under construction
paintNet vContext dc view
  = do context@Context{vNet=vNet} <- varGet vContext
       let (tVs,pVs,as) = selected context
       net <- varGet vNet
       set dc [brushKind := BrushSolid]
       mapM_ (drawPlace dc context pVs) $ places net
       mapM_ (drawTrans dc context tVs) $ trans  net
       mapM_ (drawArc (mPos context) dc as)            $ arcs   net
       case current context of
        Just (PTP tV ps) -> partialArc tV ps (mPos context)
        Just (PPT pV ps) -> partialArc pV ps (mPos context)
        Nothing          -> return ()
  where
    partialArc nV ps' mPos =
      do fromN <- readMVar nV
         let ps = map snd ps'
         polyline dc ((nPos $ object fromN):(ps++[mPos])) []

relevant Nothing     mPos nPos nType = False
relevant (Just from) mPos nPos nType =
  currentType from /= nType && closeByPt nodeUnit mPos nPos
  where
    currentType (PTP _ _) = T
    currentType (PPT _ _) = P

drawNode dc context nVs nV nType nDraw = do
  (pos,_)  <- getNPos nV
  (name,_) <- getName nV
  let props = if null name
              then [penColor := yellow, penWidth := 2]
              else if relevant (current context) (mPos context) pos nType
              then [penColor := red, penWidth := 2]
              else if nV `elem` nVs
              then [penColor := blue, penWidth := 2]
              else []
  nDraw pos props

drawPlace dc context nVs nV = do
  (pos,_)  <- getNPos nV
  (name,_) <- getName nV
  drawText dc (name) (pointMove nodeAttrOffset1 pos)
    [textColor := placeNameColour, fontSize := 8, fontWeight := WeightBold]
  (Just pType,_) <- getPlaceType nV
  (Just pInit,_) <- getPlaceInit nV
  drawText dc (pInit++(if pType/="()" then "::"++pType else ""))
    (pointMove nodeAttrOffset2 pos)
    [textColor := placeInitColour,fontSize := 8]
  dcWith dc [brushColor := placeColour] $
    drawNode dc context nVs nV P $
      \pos->circle dc pos placeRadius

drawTrans dc context nVs nV = do
  (pos,_)  <- getNPos nV
  (name,_) <- getName nV
  drawText dc (name) (pointMove nodeAttrOffset1 pos)
    [textColor := transNameColour, fontSize := 8, fontWeight := WeightBold]
  Trans{tGuard=tGuard,tVert=tVert} <- fmap nType $ getNodeObject nV
  when (tGuard/="True") $
    drawText dc (tGuard) (pointMove nodeAttrOffset2 pos)
      [textColor := transGuardColour,fontSize := 8]
  dcWith dc [brushColor := transColour] $
    drawNode dc context nVs nV T $
      if tVert
      then \pos->drawRect dc (rectMove (rect pos transVSize) transVOffset)
      else \pos->drawRect dc (rectMove (rect pos transSize) transOffset)

drawArc mPos dc as a@(NetObject nId arc) = do
  (t,l,from',ps',to') <- getPoss arc
  Trans{tVert=tVert} <- fmap nType $ getNodeObject $ getArcTrans a
  let ps              = map snd ps'
      (from,to,arrow) = if null ps
                        then scaleEdge tVert t from' to'
                        else let (x,_,_) = scaleEdge tVert t from' (head ps)
                                 (_,y,a) = scaleEdge tVert t (last ps) to'
                             in (x,y,a)
      props           = if (a `elem` as)
                        then [penColor := blue, textColor := blue]
                        else []
      -- place label on the fragment closest to the transition
      -- try to reduce text overlaps
      (labelFrom,labelTo) = case t of
                              P -> (last (from:ps), to)
                              T -> (from, head (ps++[to]))
      labelPos            = midArc labelFrom labelTo
      (labelAlign,top)    = let v = vecBetween labelFrom labelTo
                            in case (vecX v,vecY v) of
                                  (x,y) | abs y < nodeUnit -> (AlignCentre,y<=0)
                                  (x,y) | x <= 0           -> (AlignRight,y<=0)
                                  (x,y)                    -> (AlignLeft,y<=0)
  dcWith dc props $ do
    polyline dc ((from:ps)++[to]) []
    mapM_ (\(f,p)->when f $
      drawRect dc (arcPointRect p) [penColor:= blue]) ps'
    polygon dc arrow [brushColor := arrowColour]
    when (l/="()") $
      drawTextAligned labelAlign top
        dc l labelPos [textColor := arcLabelColour,fontSize := 8]

drawTextAligned AlignLeft   top  dc t pos props = do
  s <- dcWith dc props $ getTextExtent dc t
  drawText dc t (pointMove (flip (vecFromSize s)) pos) props
  where flip (Vector x y) = vec 2 (if top then 0 else negate y) -- 2 from the line
drawTextAligned AlignRight  top dc t pos props = do
  s <- dcWith dc props $ getTextExtent dc t
  drawText dc t (pointMove (flip (vecFromSize s)) pos) props
  where flip (Vector x y) = vec (negate x) (if top then 0 else negate y)
drawTextAligned AlignCentre top dc t pos props = do
  s <- dcWith dc props $ getTextExtent dc t
  drawText dc t (pointMove (flip (vecFromSize s)) pos) props
  where flip (Vector x y) = vec (negate $ x `div` 2) (if top then 0 else negate y)

getPoss (PT pV tV l ps) = do
  (from,_) <- getNPos pV
  (to,_)   <- getNPos tV
  return (P,l,from,ps,to)
getPoss (TP tV pV l ps) = do
  (from,_) <- getNPos tV
  (to,_)   <- getNPos pV
  return (T,l,from,ps,to)

-- helpers

scaleEdge tVert fromType from to = (f,t,[arrowLeft,t,arrowRight])
  where
    x@(f,t,[arrowLeft,_,arrowRight],_,_) = scaleEdge' tVert fromType from to

scaleEdge' tVert fromType from to =
  (f,t,[arrowLeft,t,arrowRight],(tx,ty),(vecX tv,vecY tv))
  where
    (f,t) = case fromType of
                P -> (pointMove pv from,pointMove (vecNegate tv) to)
                T -> (pointMove tv from,pointMove (vecNegate pv) to)

    diff      = vecBetween from to
    (vx,vy)   = (vecX diff,vecY diff)
    (svx,svy) = (nonzero $ signum vx,nonzero $ signum vy)

    (tHHeight',tHWidth') = if tVert then (tHWidth,tHHeight) else (tHHeight,tHWidth)

    dx,dy  :: Rational
    (dx,dy) = (fromIntegral vx,fromIntegral vy)
    (tx,ty) = (proj tHWidth' tHHeight' dx dy,proj tHHeight' tHWidth' dy dx)
    proj ux uy x 0 = myround $ fromIntegral ux*signum x
    proj ux uy x y = myround $ x*fromIntegral uy/y
    absmin a b | abs b > abs a = a
    absmin a b | otherwise     = b
    nonzero 0 = 1
    nonzero z = z
    tv = vec (absmin (tHWidth'*svx)  (tx*svy))
             (absmin (tHHeight'*svy) (ty*svx))

    pv = scaleVec nodeUnit       diff

    arrow      = scaleVec (-arrowWidth) diff
    arrowOrth  = vecOrtogonal arrow
    arrowLength= scaleVec arrowRatio arrow
    arrowLeft  = pointMove (vecAdd arrowLength arrowOrth) t
    arrowRight = pointMove (vecSub arrowLength arrowOrth) t

