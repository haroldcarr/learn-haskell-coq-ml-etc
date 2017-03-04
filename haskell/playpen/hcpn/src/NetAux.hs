module NetAux where

import Graphics.UI.WX
import Control.Concurrent

import NetData
import WindowContext

version = "HCPN NetEdit v0.1"

-- auxiliary functions -- {{{

scaleVec s v = vec (myround $ (fromIntegral $ vecX v) * f)
                   (myround $ (fromIntegral $ vecY v) * f)
               where
                f = s/vecLength v

myround x = truncate (0.5*signum x + x)

findObject pt ((Left (pt',nV)):ns) 
  | closeByPt nodeUnit pt pt' 
  = Just nV
findObject pt ((Right ((f,ps',t),aV)):ns) 
  | not (closeByPt nodeUnit f pt) && not (closeByPt nodeUnit t pt) 
    && or cbls && not (any (closeByPt arcPointUnit pt) ps)
  = Just aV
  where
    ps  = map snd ps'
    pts = f:(ps++[t])
    cbls  = zipWith (closeByLine pt) (init pts) (tail pts)
findObject pt (_:ns) = findObject pt ns
findObject pt []     = Nothing

findAux pt ((Right ((f,ps',t),aV)):ns) 
  | not (closeByPt nodeUnit f pt) && not (closeByPt nodeUnit t pt) 
    && any (closeByPt arcPointUnit pt) ps
  = Just aV
  where
    ps  = map snd ps'
findAux pt (_:ns) = findAux pt ns
findAux pt []     = Nothing

closeByPt unit pt pt' = unit > (abs $ vecLength $ vecBetween pt pt') 

closeByLine pt f t = (abs x) < nodeUnit && 0 <= y && y <= l
  where (x,y) = betweenPtLine pt f t
        l = vecLength $ vecBetween f t

betweenPtLine pt f t = (a*ll,b*ll)
  where
    (fx,fy) = (fromIntegral $ pointX f,fromIntegral $ pointY f)
    (tx,ty) = (fromIntegral $ pointX t,fromIntegral $ pointY t)
    (px,py) = (fromIntegral $ pointX pt,fromIntegral $ pointY pt)
    (lx,ly) = (tx-fx,ty-fy)
    ll      = sqrt (lx*lx+ly*ly)
    (fpx,fpy) = (fx-px,fy-py)
    (a,b)   = if lx==0 && ly/=0
              then (-fpx/ly,-fpy/ly)
              else ((lx*fpy-ly*fpx)/(lx*lx+ly*ly)
                   ,(-a*ly-fpx)/lx)

midArc from to = pointMove (scaleVec (vecLength v/2) v) from
  where v = vecBetween from to

getArcPos a@(NetObject{object=(PT p t l ps)}) = do
  (p,_) <- getNPos p
  (t,_) <- getNPos t
  return $ Right ((p,ps,t),a)
getArcPos a@(NetObject{object=(TP t p l ps)}) = do
  (p,_) <- getNPos p
  (t,_) <- getNPos t
  return $ Right ((t,ps,p),a)
getLabel a@(NetObject{object=(TP t p l _)}) = return l
getLabel a@(NetObject{object=(PT p t l _)}) = return l
setLabel a@(NetObject{object=(TP t p _ ps)}) l = return a{object=TP t p l ps}
setLabel a@(NetObject{object=(PT p t _ ps)}) l = return a{object=PT p t l ps}

getArcPlace (NetObject{object=(TP _ p _ _)}) = p
getArcPlace (NetObject{object=(PT p _ _ _)}) = p
getArcTrans (NetObject{object=(TP t _ _ _)}) = t
getArcTrans (NetObject{object=(PT _ t _ _)}) = t
getArcLabel (NetObject{object=(TP _ _ l _)}) = l
getArcLabel (NetObject{object=(PT _ _ l _)}) = l
modArcPoints f (no@NetObject{object=(TP t p l ps)}) 
  = no{object=(TP t p l $ f ps)}
modArcPoints f (no@NetObject{object=(PT p t l ps)}) 
  = no{object=(PT p t l $ f ps)}

getPos nV  = readMVar nV >>= \no->return $ Left (nPos $ object no,nV)
getNPos nV  = readMVar nV >>= \no->return (nPos $ object no,nV)
getName nV = readMVar nV >>= \no->return (nName $ object no,nV)
getId  nV  = readMVar nV >>= \no->return (nId no,nV)
getPlaceType nV    = readMVar nV >>= \no->return $ (placeType $ nType $ object no,nV)
setPlaceType nV t  = updateObject nV $ \o->o{nType=(nType o){pType=t}}
getPlaceInit nV    = readMVar nV >>= \no->return $ (placeInit $ nType $ object no,nV)
setPlaceInit nV i  = updateObject nV $ \o->o{nType=(nType o){pInit=i}}
setPlaceMark nV m  = updateObject nV $ \o->o{nType=(nType o){pInit=m}} -- add separate field later...
getTransGuard nV   = readMVar nV >>= \no->return $ (transGuard $ nType $ object no,nV)
setTransGuard nV g = updateObject nV $ \o->o{nType=(nType o){tGuard=g}}
getNodeObject nV  = readMVar nV >>= \no->return $ object no
updateObject nV f = modifyMVar_ nV $ \no->return no{object=f (object no)}

class TransAttr a where transGuard :: a -> Maybe String
instance TransAttr Trans where transGuard = Just . tGuard
instance TransAttr Place where transGuard = const Nothing

class PlaceAttr a where placeType :: a -> Maybe String
                        placeInit :: a -> Maybe String
instance PlaceAttr Place where placeType = Just . pType 
                               placeInit = Just . pInit 
instance PlaceAttr Trans where placeType = const Nothing
                               placeInit = const Nothing
                               
-- }}}

-- defaults-- {{{

-- default editor properties

historyLength :: Int
historyLength = 10

-- default node attributes
placeColour      = green
placeNameColour  = rgb 0 150 0
placeTypeColour  = rgb 0 150 0
placeInitColour  = rgb 0 150 0
transColour      = grey
transNameColour  = darkgrey
transGuardColour = darkgrey
arcLabelColour   = darkgrey

arrowWidth,arrowRatio :: Num a => a
arrowWidth  = 3
arrowRatio  = 5
arrowColour = black

nodeUnit, arcPointUnit :: Num a => a
nodeUnit     = 10
arcPointUnit = 5

tHHeight, tHWidth :: Num a => Int
tHHeight =   2
tHWidth  =  10


placeRadius :: Int
placeRadius = nodeUnit

transSize   = sz (2*tHWidth) (2*tHHeight) 
transOffset = vec (-tHWidth) (-tHHeight) 
transVSize   = sz (2*tHHeight) (2*tHWidth) 
transVOffset = vec (-tHHeight) (-tHWidth) 

nodeAttrOffset1 = vec (08::Int) (12::Int)
nodeAttrOffset2 = vec (08::Int) (24::Int)

arcPointRect p = rect (pointMove (vec (negate 2) (negate 2)) p) (sz 4 4)
-- }}}
