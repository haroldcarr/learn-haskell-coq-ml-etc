module NetImportExport where

import           NetAux
import           NetData
import           WindowContext

import           Control.Concurrent
import qualified Control.Exception  as CE
import           Data.Char          (toLower)
import           Data.List          (intersperse, isPrefixOf, nub, partition,
                                     (\\))
import           Data.Unique
import           Graphics.UI.WX
import           System.Directory   (getCurrentDirectory, setCurrentDirectory)
import           System.IO

-- import/export of nets

fileFormatVersion = "file format version: 28072004"

-- fileDialogs return absolute paths..
-- and prefer upper case drive letters on windows..
localisePath fp' = do
  d' <- getCurrentDirectory
  let downIt p' = case p' of
                   (drive:(':':path)) -> (toLower drive):':':path
                   _                  -> p'
      d   = downIt d'
      fp  = downIt fp'
      lfp = if d `isPrefixOf` fp then fp \\ d else fp
  if null d
    then return lfp
    else return $ dropWhile (`elem` "/\\") lfp

-- finally, a Read instance for Point..
restorePoint no@NetObject{object=o@Node{nPos=Point x y}} = no
  -- no{object=o{nPos=pt x y}}
replacePoint no@NetObject{object=o@Node{nPos=pt}} =  no
  -- no{object=o{nPos=(pointX pt,pointY pt)}}

restorePoints no@NetObject{object=o@(PT p t l pts)} =
  no{object=PT p t l [(False,pt)|pt<-pts]}
restorePoints no@NetObject{object=o@(TP t p l pts)} =
  no{object=TP t p l [(False,pt)|pt<-pts]}
replacePoints no@NetObject{object=o@(PT p t l pts)} =
  no{object=PT p t l [pt |(_,pt)<-pts]}
replacePoints no@NetObject{object=o@(TP t p l pts)} =
  no{object=TP t p l [pt |(_,pt)<-pts]}

-- change to directory part of preSelected filepath
goThere fp = do
  let (sep,rpath) = span (`elem` "/\\")
                  $ dropWhile (not . (`elem` "/\\"))
                  $ reverse fp
      path        = reverse rpath
  if null sep
    then return fp
    else do
            cwd <- getCurrentDirectory
            setCurrentDirectory path
            localisePath $ cwd++sep++fp

-- import a net
importNet preSelect vContext
  = do context@Context{wStatus=version,vNet=vNet,p=p,f=fr,t=t,currentFile=cf} <- varGet vContext
       net@Net{trans=trans,places=places,arcs=arcs,decls=decls} <- varGet vNet
       file <- maybe
                (fileOpenDialog p True True "Select net for import"
                 [("HCPN",["*.hcpn"])] "" cf)
                (fmap Just . goThere)
                preSelect
       flip (maybe (return ())) file $ \fileName-> do
         fileName <- localisePath fileName
         f <- openFile fileName ReadMode
         varUpdate vContext $ \c->c{currentFile=fileName}
         set fr [text := version++" ["++fileName++"]"]
         l <- hGetLine f
         ff <- hGetLine f
         if (ff/=fileFormatVersion)
          then do
           errorDialog p "importNet" $
             unlines ["version mismatch:"
                     ,"reading: "++show version
                     ,"expecting "++fileFormatVersion
                     ,show l
                     ,show ff
                     ]
          else do
               "-- places" <- hGetLine f
               (ps,"-- transitions") <- hGetLine f
                     >>= loop importNode ([]::[NetObject Int (Node String Point Place)]) f
               (ts,"-- arcs") <- hGetLine f
                     >>= loop importNode ([]::[NetObject Int (Node String Point Trans)]) f
               pmap <- mapM refreshNode ps
               tmap <- mapM refreshNode ts
               (as,"-- decls") <- hGetLine f
                     >>= loop importArc  ([]::[NetObject Int (Arc Int Int (Bool,Point))]) f
               amap <- mapM (refreshArc (pmap,tmap)) as
               ds <- hGetContents f
               seq (length ds) $ return ()
               let newTrans  = map snd tmap
                   newPlaces = map snd pmap
                   newArcs   = map snd amap
               varUpdate vNet $ \n->n{trans ={- trans ++ -} newTrans
                                     ,places={- places++ -} newPlaces
                                     ,arcs  ={- arcs  ++ -} newArcs
                                     ,decls ={- decls ++ -} ds}
               varUpdate vContext $ \c->c{selected=(newTrans,newPlaces,[])}
               set t [text:=ds]
         hClose f
       where
        loop _ nos f l@('-':('-':_)) = return (reverse nos,l)
        loop importer nos f l        = do no <- importer vContext l
                                          -- print no
                                          l <- hGetLine f
                                          loop importer (no:nos) f l

importArc vContext l =
  (return $! restorePoints $ read l)

importNode vContext l = do
  (return $! restorePoint $ read l)

refreshArc (pmap,tmap) no@NetObject{object=(PT pnr tnr l ps)} =
  refreshId no{object=PT pV tV l ps}
  where (Just pV,Just tV) = (lookup pnr pmap,lookup tnr tmap)
refreshArc (pmap,tmap) no@NetObject{object=(TP tnr pnr l ps)} =
  refreshId no{object=TP tV pV l ps}
  where (Just pV,Just tV) = (lookup pnr pmap,lookup tnr tmap)

refreshNode no = do
  (nr,node) <- refreshId no
  nV  <- newMVar node
  return (nr,nV)

refreshId no@NetObject{nId=n} = do
  nId <- newUnique
  return (n,no{nId=nId})

-- export the net
-- (change to exporting selection?)
exportNet vContext
  = do context@Context{vNet=vNet,p=p,t=t,currentFile=cf} <- varGet vContext
       ds <- get t text
       varUpdate vNet $ \n->n{decls=ds}
       net <- varGet vNet
       file <- fileSaveDialog p True True "Select name for HCPN export"
                [("HCPN",["*.hcpn"])] "" cf
       flip (maybe (return ())) file $ \fileName-> do
         fileName <- localisePath fileName
         f <- openFile fileName WriteMode
         hPutStrLn f $ "generated by: "++version
         hPutStrLn f $ fileFormatVersion
         hPutStrLn f "-- places"
         ps <- mapM readMVar $ places net
         mapM_ (exportNetObject f context . replacePoint) ps
         hPutStrLn f "-- transitions"
         ts <- mapM readMVar $ trans net
         mapM_ (exportNetObject f context . replacePoint) ts
         hPutStrLn f "-- arcs"
         let extractIds arc nV nV' =
              do n  <- readMVar nV
                 n' <- readMVar nV'
                 return $ arc (nId n) (nId n')
             untangle no =
              case object no of
               PT pV tV l ps -> extractIds (\p t->no{object=PT p t l ps}) pV tV
               TP tV pV l ps -> extractIds (\t p->no{object=TP t p l ps}) tV pV
         as <- mapM untangle $ arcs   net
         mapM_ (exportNetObject f context . replacePoints) as
         hPutStrLn f "-- decls"
         hPutStr f ds
         hClose f

exportNetObject f context node =
  hPutStrLn f $ show node

-- export Haskell code for simulation
{- assumptions:
  - input arc labels are patterns --> permit expressions if == defined?
    needs recursive do..
    stick to guards for now, should issue warning
  - all nodes are named.. --> should issue warning
-}
checkForNames net = do
  namesAndTrans  <- mapM getName $ trans net
  namesAndPlaces <- mapM getName $ places net
  return $ (unnamed namesAndTrans, unnamed namesAndPlaces)
  where unnamed nos = [no |("",no) <- nos]

exportNet2Haskell vContext
  = do context@Context{vNet=vNet,p=p,t=t,currentFile=cf} <- varGet vContext
       ds <- get t text
       varUpdate vNet $ \n->n{decls=ds}
       net <- varGet vNet
       (unnamedTs,unnamedPs) <- checkForNames net
       when (not $ null unnamedTs) $ do
         errorDialog p "exportNet2Haskell" $
            unlines ["there are unnamed transitions (coloured yellow) -"
                    ,"generated code will be invalid!"]
       when (not $ null unnamedPs) $ do
         errorDialog p "exportNet2Haskell" $
            unlines ["there are unnamed places (coloured yellow) -"
                    ,"generated code will be invalid!"]
       let hsfile = case span (/='.') $ reverse cf of
                      ("npch",'.':elif) -> reverse elif++".hs"
                      _                 -> cf++".hs"
           escape f = concatMap (\c->if c=='\\' then "\\\\" else [c]) f
       file <- fileSaveDialog p True True
                  "Select name for Haskell code export"
                  [("Haskell",["*.hs"])] "" hsfile
       flip (maybe (return ())) file $ \fileName-> do
         fileName <- localisePath fileName
         f <- openFile fileName WriteMode
         hPutStrLn f $ "-- generated by "++version
         hPutStrLn f "module Unnamed where"
         hPutStrLn f "import SimpleHCPN"
         hPutStrLn f "import GuiHCPN"
         hPutStrLn f "import List (intersperse)"
         hPutStrLn f ""
         hPutStrLn f "-- declarations"
         hPutStrLn f $ decls net
         hPutStrLn f "-- markings"
         ps <- mapM readMVar $ places net
         let entry (NetObject{object=o}) =
              nName o++" :: ["++pType (nType o)++"]"
         hPutStrLn f "data Mark = Mark {"
         outSeq f "    "
                  "  , "
                  "  } deriving Show"
                $ map (hPutStrLn f . entry) ps
         hPutStrLn f "-- transition actions"
         let as = arcs net
             tCode tV = do
              t@(NetObject{object=o}) <- readMVar tV
              hPutStrLn f $ nName o++" :: Mark -> [Mark]"
              hPutStrLn f $ nName o++" m = "
              hPutStrLn f $ "  do"
              let inArc tV (PT pV tV' l ps) = tV==tV'
                  inArc tV (TP tV' pV l ps) = False
                  inArcs = filter (inArc tV . object) as
                  outArc tV (TP tV' pV l ps) = tV==tV'
                  outArc tV (PT pV tV' l ps) = False
                  outArcs = filter (outArc tV . object) as
                  preSet  = map getArcPlace inArcs
                  postSet = map getArcPlace outArcs
                  opSet = nub $ preSet++postSet
                  samePlace a1 a2 = getArcPlace a1 == getArcPlace a2
                  reGroupBy p []    = []
                  reGroupBy p (h:t) = let (same,other) = partition (p h) t in
                                      (h:same):(reGroupBy p other)
              mapM_ (preSelectionCode f) opSet
              mapM_ (selectionCode f) $ reGroupBy samePlace inArcs
              hPutStrLn f $ "    if "++tGuard (nType o)
              hPutStrLn f $ "     then return m{ "
              outSeq f "              "
                       "            , "
                       "            }"
                    $ map (modificationCode f)
                    $ reGroupBy samePlace $ inArcs++outArcs
              hPutStrLn f $ "     else fail \"guard failed\""
         mapM_ tCode $ trans net
         hPutStrLn f "-- transitions"
         outSeq f "net = Net{trans=[ "
                  "                , "
                  "                ]} "
                $ map (listTransition f) $ trans net
         hPutStrLn f "-- initial marking"
         outSeq f "mark = Mark{ "
                  "           , "
                  "           } "
                $ map (listMarking f) $ places net
         hPutStrLn f "-- end of net code"
         hPutStrLn f ""
         hPutStrLn f $ "main = simMain \""++escape cf++"\" showMarking net mark"
         hPutStrLn f ""
         outSeq f "showMarking pmap = let "
                  "                       "
                  "                   in \\setPlaceMark m-> do"
                $ map (lookupPlaceCode f) $ places net
         outSeq f "                         "
                  "                         "
                  ""
                $ map (showPlaceCode f) $ places net
         hClose f

preSelectionCode f p = do
  (pn,_) <- getName p
  hPutStrLn f $ "    let "++pn++"_marking = "++pn++" m"

-- thread place marking through inArc selection
selectionCode f as@(a:_) = do
  let p = getArcPlace a
  (pn,_) <- getName p
  let selectCode a = let l = getArcLabel a in
        hPutStrLn f $ "    ("++l++", "++pn++"_marking"
                        ++") <- select $ "++pn++"_marking"
  mapM_ selectCode as

-- collect outArc and inArc modifications per place
modificationCode f as@(a:_) = do
  (pn,_) <- getName p
  hPutStr f $ pn++" = "
  mapM_ (hPutStr f . modArc) as
  hPutStrLn f $ pn++"_marking"
  where
    p = getArcPlace a
    inArc (NetObject{object=PT _ _ _ _}) = True
    inArc (NetObject{object=TP _ _ _ _}) = False
    modArc a | inArc a   = ""
    modArc a | otherwise = let l = getArcLabel a in "("++l++") : "

listTransition f t = do
  (n,_) <- getName t
  hPutStrLn f $ "Trans{name=\""++n++"\",info=Nothing,action="++n++"}"

listMarking f p = do
  (n,_) <- getName p
  (Just i,_) <- getPlaceInit p
  hPutStrLn f $ n++" = ["++i++"]"

lookupPlaceCode f p = do
  (n,_) <- getName p
  hPutStrLn f $ "(Just nV_"++n++") = lookup "++show n++" pmap"

showPlaceCode f p = do
  (n,_) <- getName p
  hPutStrLn f $ "setPlaceMark nV_"++n++" (concat $ intersperse \",\" $ map show $ "++n++" m)"

outSeq f prefix separator postfix s = do
  hPutStr f prefix
  sequence_ $ intersperse (hPutStr f separator) s
  hPutStrLn f postfix

