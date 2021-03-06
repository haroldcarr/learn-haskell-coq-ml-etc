-- generated by HCPN NetEdit v0.0
module Unnamed where
import SimpleHCPN
import GuiHCPN
import List (intersperse)

-- declarations

-- markings
data Mark = Mark {
    forks :: [Int]
  , phil_ready :: [Int]
  , phil_has_both :: [Int]
  , phil_has_right :: [Int]
  } deriving Show
-- transition actions
phil_finished :: Mark -> [Mark]
phil_finished m = 
  do
    let phil_has_both_marking = phil_has_both m
    let forks_marking = forks m
    let phil_ready_marking = phil_ready m
    (p, phil_has_both_marking) <- select $ phil_has_both_marking
    if True
     then return m{ 
              phil_has_both = phil_has_both_marking
            , forks = ((p-1) `mod` 3) : ((p+1) `mod` 3) : forks_marking
            , phil_ready = (p) : phil_ready_marking
            }
     else fail "guard failed"
phil_takes_left :: Mark -> [Mark]
phil_takes_left m = 
  do
    let forks_marking = forks m
    let phil_has_right_marking = phil_has_right m
    let phil_has_both_marking = phil_has_both m
    (f, forks_marking) <- select $ forks_marking
    (p, phil_has_right_marking) <- select $ phil_has_right_marking
    if f == ((p+1) `mod` 3)
     then return m{ 
              forks = forks_marking
            , phil_has_right = phil_has_right_marking
            , phil_has_both = (p) : phil_has_both_marking
            }
     else fail "guard failed"
phil_takes_right :: Mark -> [Mark]
phil_takes_right m = 
  do
    let forks_marking = forks m
    let phil_ready_marking = phil_ready m
    let phil_has_right_marking = phil_has_right m
    (f, forks_marking) <- select $ forks_marking
    (p, phil_ready_marking) <- select $ phil_ready_marking
    if f == ((p-1) `mod` 3)
     then return m{ 
              forks = forks_marking
            , phil_ready = phil_ready_marking
            , phil_has_right = (p) : phil_has_right_marking
            }
     else fail "guard failed"
-- transitions
net = Net{trans=[ Trans{name="phil_finished",info=Nothing,action=phil_finished}
                , Trans{name="phil_takes_left",info=Nothing,action=phil_takes_left}
                , Trans{name="phil_takes_right",info=Nothing,action=phil_takes_right}
                ]} 
-- initial marking
mark = Mark{ forks = [0,1,2]
           , phil_ready = [0,1,2]
           , phil_has_both = []
           , phil_has_right = []
           } 
-- end of net code

main = simMain "philosophers-folded.hcpn" showMarking net mark

showMarking pmap = let (Just nV_forks) = lookup "forks" pmap
                       (Just nV_phil_ready) = lookup "phil_ready" pmap
                       (Just nV_phil_has_both) = lookup "phil_has_both" pmap
                       (Just nV_phil_has_right) = lookup "phil_has_right" pmap
                   in \setPlaceMark m-> do
                         setPlaceMark nV_forks (concat $ intersperse "," $ map show $ forks m)
                         setPlaceMark nV_phil_ready (concat $ intersperse "," $ map show $ phil_ready m)
                         setPlaceMark nV_phil_has_both (concat $ intersperse "," $ map show $ phil_has_both m)
                         setPlaceMark nV_phil_has_right (concat $ intersperse "," $ map show $ phil_has_right m)

