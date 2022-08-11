{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module RDRs where

import Control.Monad
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.IORef
import Text.Printf

import DLM

------------------------------------------------------------------------------

newtype RDR a = RDR { runRDR :: ReaderT Env IO a } 

instance Monad RDR where
  return = RDR . return
  (RDR c) >>= f = RDR (do v <- c; runRDR (f v))

instance MonadIO RDR where
  liftIO c = RDR (liftIO c)

instance MonadReader Env RDR where
  ask = RDR ask 
  local f c = RDR (local f (runRDR c))

--

type Chain = ([Label],Int) 
newtype Protected a = Protected (a,Label,[Chain]) 

currentL :: Protected a -> Label
currentL (Protected (_,lv,chains)) = 
  meetL lv (meetLs (map (uncurry (!!)) chains))

type R a = IORef (Protected a) 

data Proc m a b = Proc {
      pcL  :: Label,
      auth :: Principal,
      argL :: Label,
      resL :: Label,
      code :: Protected a -> m (Protected b) -- extend to recursive functions
}

type Env = ([Principal],PermissionContext,Principal,Label)

--

class (Monad m, MonadIO m, MonadReader Env m) => M m where
  runM         :: Env -> m (Protected a) -> IO a
  returnM      :: Protected a -> m (Protected a) 
  localUserM   :: Principal -> m (Protected a) -> m (Protected a)
  tagM         :: Label -> a -> m (Protected a) 
  binopM       :: (a -> b -> c) -> 
                  Protected a -> Protected b -> 
                  m (Protected c)
  ifM          :: Protected Bool -> 
                    m (Protected a) -> m (Protected a) -> m (Protected a) 
  callM        :: Protected (Proc m a b) -> Protected a -> m (Protected b) 
  newM         :: Label -> Protected a -> m (Protected (R a))
  readM        :: Protected (R a) -> m (Protected a)
  writeM       :: Protected (R a) -> Protected a -> m (Protected ())
  declassifyM  :: Protected a -> Label -> m (Protected a)
  delegateM    :: Protected a -> [Label] -> m (Protected a) 
  redelegateM  :: Protected a -> Label -> m (Protected a) 
  revokeM      :: Protected a -> [Label] -> m (Protected a) 

instance M RDR where
  runM env c = 
      runReaderT (runRDR 
        (do v <- c
            Protected (v',_,_) <- declassifyM v leastRestrictiveL
            return v'))
      env
  returnM e = -- need to make sure that every function ends with returnM
    do (allPs, perms, olduser,pc) <- ask
       let Protected (v,vlab,vd) = e
       return (Protected (v, joinL vlab pc, vd))
  tagM vlab v = return (Protected (v,vlab,[]))
  binopM op e1 e2 = 
    do let Protected (v,vlab,vd) = e1
           Protected (w,wlab,wd) = e2
       return (Protected (v `op` w, joinL vlab wlab, vd `intersect` wd)) 
  newM xlab e = 
    do (allPs, perms, user, pc) <- ask
       let Protected (v,vlab, vd) = e
           clab = currentL e
       if noMoreRestrictiveThanL allPs perms user (joinL pc clab) xlab 
         then do x <- liftIO $ newIORef (Protected (v,xlab,vd))
                 return (Protected (x,pc,[])) 
         else error "newM"
  readM r = 
    do let Protected (x,xlab,xd) = r
       Protected (v,vlab,vd) <- liftIO $ readIORef x
       return (Protected (v, joinL vlab xlab, vd `intersect` xd))
  writeM r e = 
    do (allPs, perms, user, pc) <- ask
       let Protected (x,xlab,xd) = r
           Protected (v,vlab,vd) = e
           cxlab = currentL r
           cvlab = currentL e
       Protected (e',vlab',vd') <- liftIO $ readIORef x
       let cvlab' = currentL (Protected (e',vlab',vd'))
       if noMoreRestrictiveThanL allPs perms user (joinL pc cxlab) cvlab' &&
          noMoreRestrictiveThanL allPs perms user (joinL pc cvlab) cvlab'
         then do liftIO $ writeIORef x (Protected (v, vlab', vd'))
                 return (Protected ((),pc,[]))
         else error "writeM"
  localUserM user c = 
    do (allPs, perms, olduser,pc) <- ask
       if actsFor perms olduser user
         then local (\_ -> (allPs, perms, user,pc)) c 
         else error "localUserM"
  ifM e c1 c2 = -- not done
    do (allPs, perms, user,pc) <- ask
       let Protected (v,vlab,vd) = e
       local 
         (\_ -> (allPs, perms, user, joinL pc vlab))
         (if v then c1 else c2)
  callM proc e = -- not done
    do (allPs, perms, user,pc) <- ask
       let Protected (Proc {pcL, auth, argL, resL, code}, plab, pd) = proc 
           Protected (v,vlab,vd) = e
       if noMoreRestrictiveThanL allPs perms user (joinL pc plab) pcL && 
          actsFor perms user auth && 
          noMoreRestrictiveThanL allPs perms user (joinL pc vlab) argL
          then local (\_ -> (allPs, perms, auth, pcL)) $ 
                 do Protected (w,wlab,wd) <- code (Protected (v, argL, vd))
                    if noMoreRestrictiveThanL allPs perms user (joinL pcL wlab) resL
                       then return (Protected (w, resL, wd))
                       else error "callM-Out"
          else error "callM-In"
  declassifyM e newL@(L newR newW) = -- declassify/endorse -- not quite done
    do (allps,perms,user,pc) <- ask
       let Protected (v, L vr vw, vd) = e
       let labR = JoinRP newR (RP user TopP)
           labW = MeetWP newW (WP user BottomP)
       if noMoreRestrictiveThanRP allps perms user vr labR &&
          noMoreRestrictiveThanWP allps perms user labW vw
         then return (Protected (v, newL, vd))
         else error "declassifyM"
  delegateM e ls = 
    do (allps,perms,user,pc) <- ask
       let Protected (v, vlab@(L vr vw), vd) = e
           f (L lr lw) = 
             let labR = JoinRP lr (RP user TopP)
                 labW = MeetWP lw (WP user BottomP)
             in noMoreRestrictiveThanRP allps perms user vr labR &&
                noMoreRestrictiveThanWP allps perms user labW vw
       if all f ls
         then return (Protected (v, vlab, vd `union` [(ls,0)]))
         else error "delegateM"
  redelegateM e l = 
    do (allps,perms,user,pc) <- ask
       let Protected (v, vlab, vd) = e
       return (Protected (v, vlab, vd))
  revokeM e ls = 
    do (allps,perms,user,pc) <- ask
       let Protected (v, vlab, vd) = e
       return (Protected (v, vlab, vd))

------------------------------------------------------------------------------
-- Ex

patient = Name "Patient"
phys1 = Name "Phys1"
phys2 = Name "Phys2"
phys3 = Name "Phys3"
insurance = Name "Insurance"

privL p = L (RP p TopP) (WP p BottomP) 

e = runM ([patient,phys1,phys2,phys3],[],TopP,leastRestrictiveL) c
  where c :: RDR (Protected Int) 
        c = do p1 <- localUserM patient $ 
                       do hist <- tagM (privL patient) 1
                          delegateM hist [privL phys1, privL phys2]
               localUserM phys1 $ 
                 do z <- tagM leastRestrictiveL 0
                    obs1 <- newM (privL phys1) z
                    writeM obs1 p1
                    v <- readM obs1
                    delegateM v [privL phys3]

------------------------------------------------------------------------------

{--

bob, taxUser, preparer :: Principal
bob = Name "Bob"
taxUser = Name "Tax user"
taxPreparer = Name "Tax Preparer"

taxPermissions :: PermissionContext
taxPermissions = [(bob,taxUser)]
    
taxC = runM ([bob,taxUser,preparer],taxPermissions,TopP,leastRestrictiveL) $
         do (preparerProc,_,_) <- preparerC
            bobC preparerProc

privateRL :: Principal -> Label
privateRL p = L (RP p p) leastRestrictiveWP

bobL, preparerL, taxUserL :: Label
bobL = privateRL bob
preparerL = privateRL taxPreparer
taxUserL = privateRL taxUser

bobC :: Proc Int Int -> RDR ()
bobC preparerProc = 
  localUserM taxUser $ 
    do spreadsheet <- newM taxUserL (publicV 1) 
       s <- readM spreadsheet
       let taxdata = binopV (+) s (publicV 10) 
       finaltaxform <- callM preparerProc taxdata
       form <- declassifyM finaltaxform publicL
       outM publicL form 

preparerC :: RDR (LabeledValue (Proc Int Int))
preparerC = 
  localUserM preparer $ 
    do database <- newM preparerL (publicV 100)
       return (publicV (Proc { 
                          argL = joinL taxUserL preparerL,
                          resL = taxUserL,
                          auth = preparer,
                          code = \ lv@(v,vsl,vrtl) -> 
                                   do d <- readM database
                                      let v' = binopV (+) lv d
                                      declassifyM v' taxUserL
                        }))
--}




------------------------------------------------------------------------------
