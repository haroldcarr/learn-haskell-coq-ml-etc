module GuiHCPN(simMain) where

import Control.Concurrent
import Data.Maybe (fromMaybe)
import Graphics.UI.WX
import Graphics.UI.WXCore (windowRaise)
import NetEdit hiding (version)
import qualified SimpleHCPN as S

version = "NetSim v0.1"

data SimState t n m w ti = SimState{tick  :: ti
                                   ,outM  :: m -> IO ()
                                   ,outT  :: (String,t) -> IO ()
                                   ,outS  :: String -> IO ()
                                   ,flush :: IO ()
                                   ,simOut:: w
                                   ,net    :: n
                                   ,marking:: m
                                   ,initial:: m
                                   }

-- simulation setup --{{{
simMain name showMarking' netNoInfo mark = start $ simFrame init
  where 
    init vContext = do
      importNet (Just name) vContext
      Context{vNet=vNet,f=f,p=p} <- varGet vContext
      net' <- varGet vNet
      tmap <- mapM getName $ trans net'
      pmap <- mapM getName $ places net'
      let net = netNoInfo{S.trans=
                    map (\t->t{S.info=fromMaybe (error "unknown transition") 
                                              $ lookup (S.name t) tmap})
                        (S.trans netNoInfo)}
          showMarking = showMarking' pmap

      -- a window for textual simulation output
      sft <- frameTool [text := "Simulation output",visible:=True,closeable:=False] f
      s <- textCtrl sft [enabled:=True]
      set sft [layout    :=dynamic $ fill $ widget s
              ,clientSize:=sz 600 400]

      -- a timer to drive the simulation
      tick <- timer p [enabled := False
                      ,interval := 500
                      ]

      simState <- newMVar SimState{tick  = tick
                                  ,outM  =outMarking showMarking s True
                                  ,outT  =outTrans vContext s True
                                  ,outS  =appendText s . (++"\n")
                                  ,flush =repaint p
                                  ,simOut=s
                                  ,net    =net
                                  ,marking=mark
                                  ,initial=mark
                                  }
      set tick [ on command := runGEvent simState ]

      set p [on keyboard :~ sim vContext tick simState 
            ]

      return ()

-- simulation control and visualisation
sim vContext tick simState previous k 
  | keyKey k == KeyChar 'i' = do
  modifyMVar_ simState $ \s@SimState{simOut=simOut
                                    ,outM=outM
                                    ,flush=flush
                                    ,initial=initial}->do
    set simOut [text := "", visible := True]
    outM initial
    flush
    return s{marking=initial}
sim vContext tick simState previous k 
  | keyKey k == KeyChar 's' = do
  Context{p=p} <- varGet vContext
  set tick [enabled :~ not]
  return ()
sim vContext tick simState previous k 
  | keyKey k == KeyChar 'S' = do
  Context{p=p} <- varGet vContext
  runGEvent simState
  return ()
sim vContext tick simState previous k 
  | keyKey k == KeyChar '+' = do
  Context{p=p} <- varGet vContext
  set tick [interval :~ \i->min (i+100) 1000]
sim vContext tick simState previous k 
  | keyKey k == KeyChar '-' = do
  Context{p=p} <- varGet vContext
  set tick [interval :~ \i->max (i-100) 1]
sim vContext tick simState previous k 
  = previous k

outMarking showMarking s False m = do
  showMarking setPlaceMark m 
outMarking showMarking s True m = do
  showMarking setPlaceMark m 
  appendText s $ show m++"\n"

outTrans vContext s False (name,t) = do
  appendText s $ name++"\n"
outTrans vContext s True (name,t) = do
  varUpdate vContext $ \c->c{selected= 
    (\(ts,ps,as)->([t],[],[])) $ selected c }
  appendText s $ name++"\n"

-- }}}

-- net simulation window -- {{{
simFrame init = do
  f <- frame [text := version, visible := False]
  p <- panel f []
  hft <- frameTool [text := "Help",visible:=False,closeable:=False] f
  h <- textCtrl hft [text:=helpText,enabled:=False]
  set hft [layout    :=dynamic $ fill $ widget h
          ,clientSize:=sz 250 80]
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

  set f [layout := fill $ widget p
        ,clientSize := sz 600 400
        ,visible := True
        ]
  windowRaise hft
  windowRaise f

  set p [on paint    := paintNet vContext
        ,on keyboard := keyHandler vContext 
--         ,on click    := selectionHandler vContext p
        ,on motion   := recordMousePos vContext 
        ,on drag     := dragHandler vContext 
        ,on mouse    :~ addHandler1 (selectionHandler vContext)
        ]

  init vContext

  return ()
-- }}}

-- simulation "loop"-- {{{
-- inverted simulation loop, for event handler
runGEvent simState = do
  modifyMVar_ simState $ \old->do
    new <- stepG old
    maybe (return old) return new

-- single simulation step, parameterised by visualisation
stepG s@SimState{tick=tick,outM=outM,outT=outT,outS=outS,flush=flush
                ,net=net,marking=marking} = do
  if null enabledTs
    then do outS "no more enabled transitions!" 
            set tick [enabled := False]
            return Nothing
    else do trans <- S.choose enabledTs
            outT (fst trans)
            let newMarking = snd trans
            outM newMarking
            flush
            return $ Just s{marking=newMarking}
  where
    enabledTs = S.enabled net marking
-- }}}
