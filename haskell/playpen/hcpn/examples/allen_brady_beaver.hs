-- generated by HCPN NetEdit v0.0
module Unnamed where
import SimpleHCPN
import GuiHCPN
import List (intersperse)

-- declarations
data TM = TM{left:: String, current:: Char, right:: String} deriving Show

empty_tape = TM "" ' ' ""

inspect []    = (' ',[])
inspect (h:t) = (h,t)

move_left (TM{left=l,current=c,right=r}) = TM{left=lt,current=lh,right=c:r}
  where (lh,lt) = inspect l

move_right (TM{left=l,current=c,right=r}) = TM{left=c:l,current=rh,right=rt}
  where (rh,rt) = inspect r

-- markings
data Mark = Mark {
    stop :: [TM]
  , s4 :: [TM]
  , s3 :: [TM]
  , s2 :: [TM]
  , s1 :: [TM]
  } deriving Show
-- transition actions
stopping :: Mark -> [Mark]
stopping m = 
  do
    let s3_marking = s3 m
    let stop_marking = stop m
    (t@TM{current=' '}, s3_marking) <- select $ s3_marking
    if True
     then return m{ 
              s3 = s3_marking
            , stop = (t{current='1'}) : stop_marking
            }
     else fail "guard failed"
t44_0 :: Mark -> [Mark]
t44_0 m = 
  do
    let s4_marking = s4 m
    (t@TM{current=' '}, s4_marking) <- select $ s4_marking
    if True
     then return m{ 
              s4 = (move_right $ t{current='1'}) : s4_marking
            }
     else fail "guard failed"
t41_1 :: Mark -> [Mark]
t41_1 m = 
  do
    let s4_marking = s4 m
    let s1_marking = s1 m
    (t@TM{current='1'}, s4_marking) <- select $ s4_marking
    if True
     then return m{ 
              s4 = s4_marking
            , s1 = (move_right $ t{current=' '}) : s1_marking
            }
     else fail "guard failed"
t34_1 :: Mark -> [Mark]
t34_1 m = 
  do
    let s3_marking = s3 m
    let s4_marking = s4 m
    (t@TM{current='1'}, s3_marking) <- select $ s3_marking
    if True
     then return m{ 
              s3 = s3_marking
            , s4 = (move_left $ t{current='1'}) : s4_marking
            }
     else fail "guard failed"
t23_1 :: Mark -> [Mark]
t23_1 m = 
  do
    let s2_marking = s2 m
    let s3_marking = s3 m
    (t@TM{current='1'}, s2_marking) <- select $ s2_marking
    if True
     then return m{ 
              s2 = s2_marking
            , s3 = (move_left $ t{current=' '}) : s3_marking
            }
     else fail "guard failed"
t21_0 :: Mark -> [Mark]
t21_0 m = 
  do
    let s2_marking = s2 m
    let s1_marking = s1 m
    (t@TM{current=' '}, s2_marking) <- select $ s2_marking
    if True
     then return m{ 
              s2 = s2_marking
            , s1 = (move_left $ t{current='1'}) : s1_marking
            }
     else fail "guard failed"
t12_1 :: Mark -> [Mark]
t12_1 m = 
  do
    let s1_marking = s1 m
    let s2_marking = s2 m
    (t@TM{current='1'}, s1_marking) <- select $ s1_marking
    if True
     then return m{ 
              s1 = s1_marking
            , s2 = (move_left $ t{current='1'}) : s2_marking
            }
     else fail "guard failed"
t12_0 :: Mark -> [Mark]
t12_0 m = 
  do
    let s1_marking = s1 m
    let s2_marking = s2 m
    (t@TM{current=' '}, s1_marking) <- select $ s1_marking
    if True
     then return m{ 
              s1 = s1_marking
            , s2 = (move_right $ t{current='1'}) : s2_marking
            }
     else fail "guard failed"
-- transitions
net = Net{trans=[ Trans{name="stopping",info=Nothing,action=stopping}
                , Trans{name="t44_0",info=Nothing,action=t44_0}
                , Trans{name="t41_1",info=Nothing,action=t41_1}
                , Trans{name="t34_1",info=Nothing,action=t34_1}
                , Trans{name="t23_1",info=Nothing,action=t23_1}
                , Trans{name="t21_0",info=Nothing,action=t21_0}
                , Trans{name="t12_1",info=Nothing,action=t12_1}
                , Trans{name="t12_0",info=Nothing,action=t12_0}
                ]} 
-- initial marking
mark = Mark{ stop = []
           , s4 = []
           , s3 = []
           , s2 = []
           , s1 = [empty_tape]
           } 
-- end of net code

main = simMain "allen_brady_beaver.hcpn" showMarking net mark

showMarking pmap = let (Just nV_stop) = lookup "stop" pmap
                       (Just nV_s4) = lookup "s4" pmap
                       (Just nV_s3) = lookup "s3" pmap
                       (Just nV_s2) = lookup "s2" pmap
                       (Just nV_s1) = lookup "s1" pmap
                   in \setPlaceMark m-> do
                         setPlaceMark nV_stop (concat $ intersperse "," $ map show $ stop m)
                         setPlaceMark nV_s4 (concat $ intersperse "," $ map show $ s4 m)
                         setPlaceMark nV_s3 (concat $ intersperse "," $ map show $ s3 m)
                         setPlaceMark nV_s2 (concat $ intersperse "," $ map show $ s2 m)
                         setPlaceMark nV_s1 (concat $ intersperse "," $ map show $ s1 m)
