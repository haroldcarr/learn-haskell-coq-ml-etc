{-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS -fglasgow-exts #-}
{-
  HCPN NetEdit v0.1

  09/06/2004

  a simple graphical editor for
  Haskell-coloured Petri nets

  Claus Reinke
-}
module NetEdit(module NetEdit
              ,module NetData
              ,module WindowContext
              ,module NetAux
              ,module NetPaint
              ,module NetImportExport) where

import           Graphics.UI.WX
import           Graphics.UI.WXCore

import           Control.Concurrent
import qualified Control.Exception  as CE
import           Control.Monad      (mplus, unless)
import           Data.Char          (ord)
import           Data.List          (intersperse, isPrefixOf, nub, partition,
                                     (\\))
import           Data.Unique
import           System.Directory
import           System.Environment
import           System.Info
import           System.IO
import           System.IO.Unsafe   (unsafePerformIO)

import           DefaultLocation
import           NetAux
import           NetData
import           NetImportExport
import           NetPaint
import           WindowContext

-- net editing window -- {{{

editFrame file = do
  -- the main editing window
  f <- frame [text := version
             ,area := rect (pt 100 100) (sz 600 400)
             ,visible := False
             ]
  p <- panel f []

  -- a help window
  hft <- frameTool [text := "Help",visible:=False,closeable:=False] f
  h <- textCtrl hft [text:=helpText,enabled:=False]
  set hft [layout    :=dynamic $ fill $ widget h
          ,clientSize:=sz 250 80]

  -- a window for declarations
  dft <- frameTool [text := "Declarations",visible:=False] f
  t <- textCtrl dft [wrap := WrapLine]

  -- lists of net elements
  vNet <- varCreate $
    Net{trans=[],places=[],arcs=[],decls=""}

  -- editing context
  vContext <- varCreate $
    Context{wStatus=version
           ,vNet=vNet,current=Nothing
           ,mPos=pointZero,mDownPos=pointZero,mDragPos=pointZero
           ,selected=([],[],[])
           ,history=[]
           ,p=p
           ,f=f
           ,t=t
           ,h=h
           ,currentFile="unnamed.hcpn"
           }

  set f [layout     := fill $ widget p
        ,clientSize := sz 600 400
        ,visible    := True
        ]
  windowRaise f

  set p [on paint    := paintNet vContext
        ,on keyboard := keyHandler vContext
--         ,on click    := selectionHandler vContext p
        ,on motion   := recordMousePos vContext
        ,on drag     := dragHandler vContext
        ,on mouse    :~ addHandler1 (selectionHandler vContext)
        ]
  maybe (return ()) (\f->importNet (Just f) vContext) file
-- }}}

-- event handlers -- {{{

addHandler1 handler previous x = do {handler x; previous x}

-- need to merge down and up branches
-- need to handle arcpoints in down branch
selectionHandler vContext (MouseLeftDown pt mods) = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  varSet vContext context{mDownPos=pt}
  unless (shiftDown mods) $ do
    let (ts,ps,as) = selected context
    net <- varGet vNet
    transNodes <- mapM getPos $ trans net
    placeNodes <- mapM getPos $ places net
    arcsAndPos <- mapM getArcPos $ arcs net
    let (ts',ps',as') = keepOrNew (ts,ps,as) (findObject pt transNodes
                                             ,findObject pt placeNodes
                                             ,findObject pt arcsAndPos)
    varUpdate vContext $ \c->c{selected=(ts',ps',as'),current=Nothing}
    repaint p
  where
    keepOrNew s@(ts,ps,as) (Just t,p,a)             | t `elem` ts = s
    keepOrNew s@(ts,ps,as) (Just t,p,a)             | otherwise   = ([t],[],[])
    keepOrNew s@(ts,ps,as) (Nothing,Just p,a)       | p `elem` ps = s
    keepOrNew s@(ts,ps,as) (Nothing,Just p,a)       | otherwise   = ([],[p],[])
    keepOrNew s@(ts,ps,as) (Nothing,Nothing,Just a) | a `elem` as = s
    keepOrNew s@(ts,ps,as) (Nothing,Nothing,Just a) | otherwise   = ([],[],[a])
    keepOrNew s@(ts,ps,as) (Nothing,Nothing,Nothing)              = s

selectionHandler vContext (MouseLeftUp pt mods) = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  if mDownPos context==pt
    then do
      let (ts,ps,as) = selected context
          action s      = if shiftDown mods then toggle s else new s
          arcsAction pt = if shiftDown mods then toggleArcP pt else newArcP pt
      net <- varGet vNet
      transNodes <- mapM getPos $ trans net
      placeNodes <- mapM getPos $ places net
      arcsAndPos <- mapM getArcPos $ arcs net
      let (ts',ps',as',aux) = action (ts,ps,as) (findObject pt transNodes
                                                ,findObject pt placeNodes
                                                ,findObject pt arcsAndPos
                                                ,findAux    pt arcsAndPos)
      flip (maybe (unless (shiftDown mods) $
                   varSet vNet net{arcs=map clearArcPs $ arcs net}))
           aux $ \a->do
        varSet vNet net{arcs=map (arcsAction pt a) $ arcs net}
      varUpdate vContext $ \c->c{selected=(ts',ps',as'),current=Nothing}
      repaint p
    else propagateEvent -- possibly dragging some nodes?
  where
    clearArcPs a = modArcPoints (map $ \(_,p)->(False,p)) a

    toggleArcP pt a' a | a'==a
      = modArcPoints (map $ \(f,p)->if closeByPt arcPointUnit pt p
                              then (not f,p)
                              else (f,p))
                     a
    toggleArcP pt a' a | otherwise
      = a

    newArcP pt a' a | a'==a
      = modArcPoints (map $ \(f,p)->(closeByPt arcPointUnit pt p,p)) a
    newArcP pt a' a | otherwise
      = modArcPoints (map $ \(f,p)->(False,p)) a

    toggleSelect []      o = [o]
    toggleSelect (o':os) o | o==o'     = os
    toggleSelect (o':os) o | otherwise = o':(toggleSelect os o)

    new s@(ts,ps,as) (Just t,p,a,_)                = ([t],[],[],Nothing)
    new s@(ts,ps,as) (Nothing,Just p,a,_)          = ([],[p],[],Nothing)
    new s@(ts,ps,as) (Nothing,Nothing,Just a,_)    = ([],[],[a],Nothing)
    new s@(ts,ps,as) (Nothing,Nothing,Nothing,aux) = ([],[],[],aux)

    toggle s@(ts,ps,as) (Just t,p,a,_)
      = (toggleSelect ts t,ps,as,Nothing)
    toggle s@(ts,ps,as) (Nothing,Just p,a,_)
      = (ts,toggleSelect ps p,as,Nothing)
    toggle s@(ts,ps,as) (Nothing,Nothing,Just a,_)
      = (ts,ps,toggleSelect as a,Nothing)
    toggle s@(ts,ps,as) (Nothing,Nothing,Nothing,aux)
      = (ts,ps,as,aux)

selectionHandler vContext me = do
  propagateEvent

recordMousePos vContext pos = do
  context@Context{p=p} <- varGet vContext
  varSet vContext context{mPos=pos,mDragPos=pos}
  repaint p

dragHandler vContext pt = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  net <- varGet vNet
  let s@(sts,sps,sas) = selected context
      mPos = mDragPos context
      diff = vecBetween mPos pt
  mapM (changePos diff) sts
  mapM (changePos diff) sps
  varUpdate vNet $ \n->n{arcs=map (changeArcPos s diff) $ arcs n}
  varSet vContext context{mDragPos=pt,mPos=pt}
  repaint p
  where
    affected (sps,sts) a =
      (getArcPlace a `elem` sps) && (getArcTrans a `elem` sts)
    changePos d nV = modifyMVar_ nV $
      \no->return no{object=(object no){nPos=pointMove d (nPos $ object no)}}
    changeArcPos (sts,sps,sas) d a@NetObject{object=(PT p t l ps)}
      = a{object=PT p t l $ map (guardedPointMove d arcFlag) ps}
      where arcFlag =  (a `elem` sas) || affected (sps,sts) a
    changeArcPos (sts,sps,sas) d a@NetObject{object=(TP t p l ps)}
      = a{object=TP t p l $ map (guardedPointMove d arcFlag) ps}
      where arcFlag =  (a `elem` sas) || affected (sps,sts) a
    guardedPointMove d arcFlag (pointFlag,p)
      | arcFlag || pointFlag
      = (pointFlag,pointMove d p)
    guardedPointMove d arcFlag (pointFlag,p)
      | otherwise
      = (pointFlag,p)

keyHandler vContext k = do
  Context{p=p} <- varGet vContext
  dispatch (keyKey k) (keyPos k) (keyModifiers k) vContext
  repaint p
  where
    dispatch (KeyChar c) pos mods vContext | c=='t'||c=='p' = do
      addNode c vContext pos
    dispatch (KeyChar 'a') pos mods vContext = do
      addArc vContext pos
    dispatch (KeyChar 'l') pos mods vContext = do
      editLabel vContext pos
    dispatch (KeyChar 'i') pos mods vContext = do
      editInitMark vContext pos
    dispatch (KeyChar 'n') pos mods vContext = do
      editName vContext pos
    dispatch (KeyChar 'd') pos mods vContext = do
      editDecls vContext pos
    dispatch (KeyChar 'u') pos mods vContext = do
      undo vContext pos
    dispatch (KeyChar c) pos mods vContext
      | ord c==01 && controlDown mods = do -- ctrl-a
      selectAll vContext
    dispatch (KeyChar c) pos mods vContext
      | ord c==05 && controlDown mods = do -- ctrl-e
      exportNet2Haskell vContext
    dispatch (KeyChar c) pos mods vContext
      | ord c==19 && controlDown mods = do -- ctrl-s
      exportNet vContext
    dispatch (KeyChar c) pos mods vContext
      | ord c==12 && controlDown mods = do -- ctrl-l
      Context{vNet=vNet,t=t} <- varGet vContext
      importNet Nothing vContext
    dispatch (KeyChar 'r') pos mods vContext = do
      rotateSelection vContext pos
    dispatch (KeyBack) pos mods vContext = do
      removeSelection vContext pos
    dispatch (KeyEscape) pos mods vContext = do
      varUpdate vContext $ \c->c{current=Nothing}
      return ()
    dispatch (KeyChar 's') pos mods vContext = do
      context@Context{vNet=vNet,p=p,currentFile=cf} <- varGet vContext
      startGHCI cf -- vContext pos
      return ()
    dispatch (KeyChar 'h') pos mods vContext = do
      showHelp vContext pos
    dispatch (KeyChar 'q') pos mods vContext = do
      Context{f=f} <- varGet vContext
      close f
    dispatch  key pos mods vContext = do
      let nord (KeyChar c) = Just $ ord c
          nord _           = Nothing
      print ("unknown key",(key,nord key),pos,mods)
      return ()

-- first fragments of an undo operation
undo vContext pos = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  let (ts,ps,as) = selected context
  net <- varGet vNet
  case history context of
    ((AddNodeTrans t):rest)
      -> do varSet vNet net{trans=filter (/=t) $ trans net}
            varSet vContext context{history =rest}
    ((AddNodePlace p):rest)
      -> do varSet vNet net{places=filter (/=p) $ places net}
            varSet vContext context{history =rest}
    ((RemoveSelection (ts',ps') as'):rest)
      -> do varSet vContext context{selected=(ts++ts',ps++ps',as)
                                   ,history =rest
                                   }
            varSet vNet net{trans =trans net++ts'
                           ,places=places net++ps'
                           ,arcs  =arcs net++as'
                           }
    _ -> errorDialog p "error" "no undo history available"

updateHistory vContext f = do
  varUpdate vContext $ \c->c{history=take historyLength $ f $ history c}
  return ()

selectAll vContext = do
  context@Context{vNet=vNet} <- varGet vContext
  net <- varGet vNet
  varUpdate vContext $
    \c->c{selected=(trans net,places net,arcs net),current=Nothing}
  return ()

-- }}}

-- net structure manipulation -- {{{

-- add a new transition
addNode 't' vContext pos = do
  nId <- newUnique
  context@Context{vNet=vNet} <- varGet vContext
  nodeVar <- newMVar $ mkTrans nId "" "True" pos
  varUpdate vNet $ \n->n{trans =nodeVar:(trans  n)}
  updateHistory vContext ((AddNodeTrans nodeVar):)
  return ()
-- add a new place
addNode 'p' vContext pos = do
  nId <- newUnique
  context@Context{vNet=vNet} <- varGet vContext
  nodeVar <- newMVar $ mkPlace nId "" "()" "" pos
  varUpdate vNet $ \n->n{places =nodeVar:(places  n)}
  updateHistory vContext ((AddNodePlace nodeVar):)
  return ()

-- add an arc between a place and a transition
addArc vContext pt = do
  context@Context{vNet=vNet} <- varGet vContext
  net <- varGet vNet
  case current context of
   (Nothing)
     -> do transNodes <- mapM getPos $ trans net
           placeNodes <- mapM getPos $ places net
           case (findObject pt transNodes,findObject pt placeNodes) of
            (Just t,_) -> varSet vContext context{current=Just $ PTP t []}
            (Nothing,Just p) -> varSet vContext context{current=Just $ PPT p []}
            (Nothing,Nothing) -> addArcSegment vNet pt
   current@(Just (PTP fromT ps))
     -> finishArc vContext vNet current (TP fromT) pt (places net)
   current@(Just (PPT fromP ps))
     -> finishArc vContext vNet current (PT fromP) pt (trans net)
  varUpdate vContext $ \c->c{selected=([],[],[])}
  return ()
  where
    addArcSegment vNet pt = do
      net <- varGet vNet
      arcsAndPos <- mapM getArcPos $ arcs net
      case findObject pt arcsAndPos of
        Nothing -> return ()
        Just a  -> do as' <- mapM (addArcSegmentArc a pt) $ arcs net
                      varSet vNet $ net{arcs=as'}
    addArcSegmentArc a' pt a | a==a'     = getArcPos a >>= addArcSegmentPts pt
    addArcSegmentArc a' pt a | otherwise = return a
    addArcSegmentPts pt (Right ((f,ps,t),a)) = return $ modArcPoints (const ps') a
      where
        loop ((fa,a):((fb,b):t)) | closeByLine pt a b = ((fa,a):((True,pt):((fb,b):t)))
        loop (a:(b:t))           | otherwise          = (a:(loop (b:t)))
        loop [a]                                      = [a]
        pts = (False,f):(ps++[(False,t)])
        ps' = init $ tail $ loop pts
    finishArc vContext vNet cur partialArc pt nodes = do
      posNodes <- mapM getPos $ nodes
      case findObject pt posNodes of
        Just toN -> do aId <- newUnique
                       let arc = mkArc aId $ partialArc toN "()" $ pts cur
                       varUpdate vNet     $ \n->n{arcs=arc:(arcs n)}
                       varUpdate vContext $ \c->c{current=Nothing}
                       return ()
        Nothing  -> do varUpdate vContext $ \c->c{current=add (False,pt) cur}
                       return ()
    pts (Just (PTP fromT ps)) = ps
    pts (Just (PPT fromP ps)) = ps
    add pt (Just (PTP fromT ps)) = Just $ PTP fromT $ ps++[pt]
    add pt (Just (PPT fromP ps)) = Just $ PPT fromP $ ps++[pt]

editLabel vContext pos = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  net <- varGet vNet
  arcsAndPos <- mapM getArcPos $ arcs net
  case (findObject pos arcsAndPos) of
    Just a  -> do a' <- updateLabel p pos a
                  varSet vNet net{arcs=updateArcs a a' $ arcs net}
    _       -> return ()
  transNodes <- mapM getPos $ trans net
  case (findObject pos transNodes) of
    Just tV -> updateGuard p tV
    _       -> return ()
  placeNodes <- mapM getPos $ places net
  case (findObject pos placeNodes) of
    Just pV -> updateType p pV
    _       -> return ()
  where
    updateArcs oa na []     = []
    updateArcs oa na (a:as) | a==oa = na:(updateArcs oa na as)
    updateArcs oa na (a:as) = a :(updateArcs oa na as)
    updateLabel p pos a = do
      old   <- getLabel a
      label <- textDialog p "Arc label?" "Arc" old
      {- something like this, but how to synch entry and update??
      synch <- newEmptyMVar
      t <- textEntry p [text:=old,position:=pos,processEnter:=True]
      set t [on command:= (get t text >>= putMVar synch) ]
      label <- takeMVar synch
      -}
      setLabel a (if (label/="") then label else old)
    updateGuard p tV = do
      (Just old,_)   <- getTransGuard tV
      label <- textDialog p "Transition guard?" "Transition" old
      when (label/="") $ setTransGuard tV label
    updateType p pV = do
      (Just old,_)   <- getPlaceType pV
      label <- textDialog p "Place type?" "Place" old
      when (label/="") $ setPlaceType pV label

editInitMark vContext pos = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  net <- varGet vNet
  placeNodes <- mapM getPos $ places net
  case (findObject pos placeNodes) of
    Just pV -> updateInit p pV
    _       -> return ()
  where
    updateInit p pV = do
      (Just old,_)   <- getPlaceInit pV
      label <- textDialog p "Initial marking?" "Place" old
      setPlaceInit pV label

editName vContext pos = do
  context@Context{vNet=vNet,p=p} <- varGet vContext
  net <- varGet vNet
  transNodes <- mapM getPos $ trans net
  placeNodes <- mapM getPos $ places net
  case (findObject pos transNodes,findObject pos placeNodes) of
    (Just nV,_) -> updateName p nV
    (_,Just nV) -> updateName p nV
    _           -> return ()
  where
    updateName p nV = do
      (old,_) <- getName nV
      name    <- textDialog p "Node name?" "Node" old
      updateObject nV $ \o->o{nName=name}

editDecls vContext pos = do
  context@Context{vNet=vNet,t=t} <- varGet vContext
  ft <- get t parent
  set ft [visible :~ not]
  return ()

helpText = unlines ["-- creating net elements"
                   ,"t: create transition"
                   ,"p: create place"
                   ,"a: (on place/transition) start/finish arc"
                   ,"   (elsewhere)           segment arc"
                   ,"d: toggle declarations window"
                   ,""
                   ,"-- editing attributes"
                   ,"n: (on place/transition) edit node name"
                   ,"l: (on arc)        edit arc label"
                   ,"   (on place)      edit place type"
                   ,"   (on transition) edit transition guard"
                   ,"i: (on place)      edit initial marking"
                   ,""
                   ,"-- general manipulation"
                   ,"mouse click left: select net element"
                   ,"ctrl-a: select all net elements"
                   ,"mouse shift click left: toggle element selection"
                   ,"mouse left drag: move selected elements"
                   ,"escape: clear partial arc"
                   ,"backspace: remove selected nodes/arcs"
                   ,"u: undo (currently very limited)"
                   ,""
                   ,"-- file operations"
                   ,"ctrl-s: save net"
                   ,"ctrl-l: load net"
                   ,"ctrl-e: export Haskell code for net simulation"
                   ,""
                   ,"s: try entering simulation mode"
                   ,"   (need to export simulation code first!"
                   ,"    also need to set environment variable HCPNdir to installation directory)"
                   ,""
                   ,"-- in simulation mode (running exported code)"
                   ,"s: start/suspend simulation run"
                   ,"S: single-step simulation"
                   ,"i: re-initialise simulation"
                   ,"+/-: increase/decrease delay between steps"
                   ,""
                   ,"--"
                   ,"q: quit HCPN NetEdit"
                   ,"h: toggle help"
                   ]

showHelp vContext pos = do
  context@Context{vNet=vNet,h=h} <- varGet vContext
  hft <- get h parent
  set hft [visible :~ not]
  return ()

-- should rotate net fragments in 90 degree steps
-- currently limited to rotating transitions
rotateSelection vContext pos = do
  context@Context{vNet=vNet} <- varGet vContext
  let (ts,ps,as) = selected context
      rotateTrans tV = updateObject tV $
                          \o@Node{nType=t}->o{nType=t{tVert=not $ tVert t}}
  mapM_ rotateTrans ts

removeSelection vContext pos = do
  context@Context{vNet=vNet} <- varGet vContext
  let (ts,ps,as) = selected context
      noArc (TP t p _ _) = t `elem` ts || p `elem` ps
      noArc (PT p t _ _) = t `elem` ts || p `elem` ps
  net <- varGet vNet
  let (noArcs,as') = partition (noArc . object) $ arcs net
  varSet vContext context{current=Nothing
                         ,selected=([],[],[])
                         }
  updateHistory vContext ((RemoveSelection (ts,ps) noArcs):)
  varSet vNet net{trans=trans net\\ts
                 ,places=places net\\ps
                 ,arcs=removeSelectedSegments $ as'\\as
                 }
  where
    removeSelectedSegments as =
      map (modArcPoints $ filter (\(f,p)->not f)) as
-- }}}

-- try to start simulation, collect error messages -- {{{
ghci = "ghci -package wx"

getHCPNpaths = do
  installPath <- (getEnv "HCPNdir")
  let sep = if os `elem` ["mingw32"] then "\\" else "/"
      binPath = installPath++sep++"bin"
      srcPath = installPath++sep++"src"
  binExists <- doesDirectoryExist binPath
  srcExists <- doesDirectoryExist srcPath
  unless (binExists && srcExists) $
    error $ "can't find HCPN dirs! current value is HCPNdir="++installPath
  return (installPath,binPath,srcPath,sep)

-- mostly adapted from wxHaskell's Process.hs sample
startGHCI netName = do
  let emaNten = reverse netName
      netHsName = case take 5 emaNten of
                    "npch." -> (reverse $ drop 5 emaNten)++".hs"
                    _ -> error $ "unknown netName in startGHCI:"++netName
  netExists <- doesFileExist netHsName
  unless netExists $ error $ "can't find Haskell code for net: "++netHsName
  (installPath,binPath,srcPath,sep) <- getHCPNpaths
  let ghciCall = ghci++" -i"++srcPath++" -odir "++binPath++" -hidir "++binPath++" "++netHsName

  f <- frame [text := ghciCall]
  p <- panel f []
  input  <- comboBox p [processEnter := True, text := "cmd"]
  status <- textCtrlRich p [bgcolor := black, textColor := green, font := fontFixed{ _fontSize = 10}]
  errorLog <- textCtrl p [text:="",visible:=False]
  output <- textCtrlRich p [bgcolor := black, textColor := white, font := fontFixed{ _fontSize = 10}]
  simulate <- button p     [text := "simulate", enabled := False]
  retry  <- button p       [text := "reload", enabled := False]
  stop   <- button p       [text := "quit", enabled := False]

  focusOn input
  textCtrlSetEditable status False
  textCtrlSetEditable output False
  set errorLog [enabled:= False]
  set f [layout := container p $
                   margin 10 $ column 5 [fill (widget output)
                                        ,fill (widget status)
                                        ,row 5 [hfill (widget input)
                                               ,widget simulate
                                               ,widget retry
                                               ,widget stop
                                               ]
                                        ]
        ,clientSize := sz 800 400
        ]

  set status [textColor := green, text := unlines ["starting ghci"]]
  let message txt = appendText output txt
  incoming <- newMVar ""
  incomingErrors <- newMVar ""
  (send,process,pid) <- processExecAsync f ghciCall 256
                          (onEndProcess f message)
                          (onReceive netHsName False incoming incomingErrors errorLog status message)
                          (onReceive netHsName True  incoming incomingErrors errorLog status message)
  when (pid /= 0) $ do
    let sendLn txt = send $ txt++"\n"
    set input    [on command := sendCommand input sendLn message]
    set stop     [enabled := True, on command  := send ":q\n"]
    set simulate [enabled := True, on command  := send "main\n"]
    set retry    [enabled := True, on command  := send ":r\n"]
    return ()

onEndProcess f message exitcode
  = do message ("\n-- process ended with exitcode " ++ show exitcode ++ " --\n")

onReceive netHsName errors incoming incomingErrors errorLog status message txt streamStatus
  = do message txt
       -- need to deal with buffering..
       modifyMVar_ (if errors then incomingErrors else incoming) $ \i->loop $ i++txt
  where
    dropPrompt ('*':l) = dropWhile (`elem` "> ") $  dropWhile (/='>') l
    dropPrompt       l = l
    loop todo = do
      let isLineSep = (`elem` "\r\n")
          (line,seprest) = span (not . isLineSep) todo
          (sep,rest)     = span isLineSep seprest
      if null line || null sep then return $ dropWhile isLineSep todo else do
        case dropPrompt line of
          l | ("Compiling Unnamed" `isPrefixOf` l) -> do
            set status [textColor := green]
            appendText status $
              "trying to load Haskell code for net simulation"++sep
            set errorLog [text:="",enabled:=True]
          l | ("Ok, modules loaded" `isPrefixOf` l) -> do
            set status [textColor := green]
            appendText status $
              "Haskell code for net simulation loaded; enter main"++sep
            set errorLog [enabled:=False]
          l | ("Failed, modules loaded" `isPrefixOf` l) -> do
            set status [textColor := red]
            appendText status $
              "Failed to load Haskell code for net simulation; check error messages"++sep
            scanErrors netHsName errorLog status
            set errorLog [enabled:=False]
          _ -> return ()
        logging <- get errorLog enabled
        when (logging && errors) $ appendText errorLog $ line++sep
        loop $ dropWhile isLineSep rest

sendCommand input send message
  = do txt <- get input text
       count <- comboBoxGetCount input
       appendText input txt
       set input [selection := count]
       send txt
       message $ txt++"\n"

scanErrors netHsName errorLog status = do
  source <- fmap (zip [1..] . lines) $ readFile netHsName
  let (prefix,declRest) = span (not . isPrefixOf "-- declarations" . snd) source
      (decls,markRest)  = span (not . isPrefixOf "-- markings" . snd) declRest
      (mark,transActRest)  = span (not . isPrefixOf "-- transition actions" . snd) markRest
      (transAct,transRest) = span (not . isPrefixOf "-- transitions" . snd) transActRest
      (trans,initRest)     = span (not . isPrefixOf "-- initial marking" . snd) transRest
      (initial,postfix)    = span (not . isPrefixOf "-- end of net code" . snd) transRest
      errorMap = [(prefix,    "[error in generated prefix code (shouldn't happen..)]\n     "
                              ,locateLine)
                 ,(decls,     "[error in declarations]\n   "
                              ,locateLine)
                 ,(mark,      "[error in place name or type]\n     "
                              ,locateLine)
                 ,(transAct,  "[error in transition name, guard or arc label, "
                              ,locateTrans "transition not found??]" "unknown part??]" transAct)
                 ,(trans,     "[error in transition list (possible?)]\n     "
                              ,locateLine)
                 ,(initial,   "[error in initial marking]\n     "
                              ,locateLine)
                 ,(postfix,   "[error in generated postfix code (shouldn't happen..)]\n     "
                              ,locateLine)
                 ]
      locateLine n = snd $ source!!(n-1)
      locateTrans name part [] n
        = name++":"++part++"]\n     "++locateLine n
      locateTrans name part ((n',l):rest) n
        | n'>n
        = name++":"++part++"]\n     "++locateLine n
      locateTrans name part ((n',l):rest) n
        | dropWhile (/=' ') l == " :: Mark -> [Mark]"
        = locateTrans (takeWhile (/=' ') l) part rest n
      locateTrans name part ((n',l):rest) n
        | "do" == dropWhile (==' ') l
        = locateTrans name "input arc" rest n
      locateTrans name part ((n',l):rest) n
        | "if " `isPrefixOf` dropWhile (==' ') l
        = locateTrans name "guard (or output arc..?)" rest n -- ghc's line number isn't precise here..
      locateTrans name part ((n',l):rest) n
        | "then " `isPrefixOf` dropWhile (==' ') l
        = locateTrans name "output arc" rest n
      locateTrans name part ((n',l):rest) n
        | n'==n
        = name++":"++part++"]\n     "++l
      locateTrans name part ((n',l):rest) n
        = locateTrans name part rest n
      classify n []                   = "numbered line not found in file???"
      classify n ((ls,msg,more):rest) | n `elem` map fst ls = msg++more n
      classify n ((ls,msg,more):rest) | otherwise           = classify n rest
  log <- fmap lines $ get errorLog text
  let loop []     = return ()
      loop (l:ls) = do
        case span (/=':') l of
          (file,':':rest) -> case span (/=':') rest of
            (n',':':message) ->
              case reads n' of
               [(n,"")] -> do
                  appendText status $ file++";"++n'++";"++message++"\n"
                  appendText status $ "     "++classify n errorMap++"\n"
               _ -> hPutStrLn stderr $ "can't interpret error msg: "++l
            _ -> return ()
          _ -> return ()
        loop ls
  loop log


-- }}}

-- Show instances..-- {{{

-- the next two instances are a bit dubious,
-- if in doubt, just show generic tag, no content
{-
instance Show a=>Show (Var a) where
  showsPrec d mv = showString "(Var "
                 . showsPrec d (unsafePerformIO $ varGet mv)
                 . showString ")"


instance Show a=>Show (MVar a) where
  showsPrec d mv = showString "(MVar "
                 . showsPrec d (unsafePerformIO $ readMVar mv)
                 . showString ")"
-}
      -- }}}

