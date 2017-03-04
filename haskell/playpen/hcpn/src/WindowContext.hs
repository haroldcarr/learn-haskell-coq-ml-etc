module WindowContext where

-- window context information

data Context var net ts ps as a m sts sps sas h p f t =
  Context{wStatus :: String
         ,vNet    :: var -- (net ts ps as)
         ,current :: a
         ,mPos    :: m
         ,mDownPos:: m
         ,mDragPos:: m
         ,selected:: (sts,sps,sas) -- note: selection of arc segment points
                                   --       recorded in arcs:-(
         ,history :: [h]
         ,p       :: p
         ,f       :: f
         ,t       :: t
         ,h       :: t
         ,currentFile :: String
         }
  deriving Show

-- etc.pp... add others, also for motion
data History t p a = 
    AddNodeTrans t
  | AddNodePlace p
  | RemoveSelection ([t],[p]) [a]
  deriving Show
