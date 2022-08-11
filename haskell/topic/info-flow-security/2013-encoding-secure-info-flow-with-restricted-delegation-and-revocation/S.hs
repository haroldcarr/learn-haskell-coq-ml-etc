{-# LANGUAGE RankNTypes, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module S where

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

newtype Protected a = Protected (a,Label) 

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

instance M RDR where
  runM env c = 
      runReaderT (runRDR 
        (do v <- c
            Protected (v',_) <- declassifyM v leastRestrictiveL
            return v'))
      env
  returnM e = -- need to make sure that every function ends with returnM
    do (allPs, perms, olduser,pc) <- ask
       let Protected (v,vlab) = e
       return (Protected (v, joinL vlab pc))
  localUserM user c = 
    do (allPs, perms, olduser,pc) <- ask
       if actsFor perms olduser user
         then local (\_ -> (allPs, perms, user,pc)) c 
         else error "localUserM"
  tagM vlab v = return (Protected (v,vlab))
  binopM op e1 e2 = 
    do let Protected (v,vlab) = e1
           Protected (w,wlab) = e2
       return (Protected (v `op` w, joinL vlab wlab)) 
  ifM e c1 c2 = 
    do (allPs, perms, user,pc) <- ask
       let Protected (v,vlab) = e
       local 
         (\_ -> (allPs, perms, user, joinL pc vlab))
         (if v then c1 else c2)
  callM proc e = 
    do (allPs, perms, user,pc) <- ask
       let Protected (Proc {pcL, auth, argL, resL, code}, plab) = proc 
           Protected (v,vlab) = e
       if noMoreRestrictiveThanL allPs perms user (joinL pc plab) pcL && 
          actsFor perms user auth && 
          noMoreRestrictiveThanL allPs perms user (joinL pc vlab) argL
          then local (\_ -> (allPs, perms, auth, pcL)) $ 
                 do Protected (w,wlab) <- code (Protected (v, argL))
                    if noMoreRestrictiveThanL allPs perms user (joinL pcL wlab) resL
                       then return (Protected (w, resL))
                       else error "callM-Out"
          else error "callM-In"
  newM xlab e = 
    do (allPs, perms, user, pc) <- ask
       let Protected (v,vlab) = e
       if noMoreRestrictiveThanL allPs perms user pc xlab &&
          noMoreRestrictiveThanL allPs perms user (joinL pc vlab) xlab 
         then do x <- liftIO $ newIORef (Protected (v,xlab))
                 return (Protected (x,pc)) 
         else error "newM"
  readM r = 
    do let Protected (x,xlab) = r
       Protected (v,vlab) <- liftIO $ readIORef x
       return (Protected (v, joinL vlab xlab))
  writeM r e = 
    do (allPs, perms, user, pc) <- ask
       let Protected (x,xlab) = r
           Protected (v,vlab) = e
       Protected (_,vlab') <- liftIO $ readIORef x
       if noMoreRestrictiveThanL allPs perms user (joinL pc xlab) vlab &&
          noMoreRestrictiveThanL allPs perms user (joinL pc vlab) vlab'
         then do liftIO $ writeIORef x (Protected (v, vlab'))
                 return (Protected ((),pc))
         else error "writeM"
  declassifyM e newL@(L newR newW) = -- declassify/endorse
    do (allps,perms,user,pc) <- ask
       let Protected (v,L vr vw) = e
       let labR = JoinRP newR (RP user TopP)
           labW = MeetWP newW (WP user BottomP)
       if noMoreRestrictiveThanRP allps perms user vr labR &&
          noMoreRestrictiveThanWP allps perms user labW vw
         then return (Protected (v, newL))
         else error "declassifyM"

------------------------------------------------------------------------------
-- Tax preparer

bob, taxUser, preparer :: Principal
bob      = Name "Bob"
taxUser  = Name "Tax user"
preparer = Name "Preparer"

privateRL :: Principal -> Label
privateRL p = L (RP p p) leastRestrictiveWP

taxPermissions :: PermissionContext
taxPermissions = [(bob,taxUser)]
    
bobL, preparerL, taxUserL :: Label
bobL      = privateRL bob
preparerL = privateRL preparer
taxUserL  = privateRL taxUser

bobC :: Protected (Proc RDR Int Int) -> RDR (Protected Int) 
bobC preparerProc = 
  localUserM taxUser $ 
    do v1 <- tagM leastRestrictiveL 1
       spreadsheet <- newM taxUserL v1
       s <- readM spreadsheet
       v10 <- tagM leastRestrictiveL 10
       taxdata <- binopM (+) s v10
       returnM taxdata

preparerC :: RDR (Protected (Proc RDR Int Int))
preparerC = 
  localUserM preparer $ 
    do v100 <- tagM leastRestrictiveL 100
       database <- newM preparerL v100
       tagM leastRestrictiveL $ 
         Proc { 
           pcL  = preparerL,
           argL = joinL taxUserL preparerL,
           resL = taxUserL,
           auth = preparer,
           code = \e -> do d <- readM database
                           v' <- binopM (+) e d
                           declassifyM v' taxUserL
       }

taxC = runM ([bob,taxUser,preparer],taxPermissions,TopP,leastRestrictiveL) $
         do preparerProc <- preparerC
            taxdata <- bobC preparerProc
            finaltaxform <- callM preparerProc taxdata
            form <- declassifyM finaltaxform leastRestrictiveL
            returnM form


