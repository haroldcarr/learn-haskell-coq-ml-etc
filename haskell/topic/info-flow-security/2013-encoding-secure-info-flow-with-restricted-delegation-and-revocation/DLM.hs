module DLM where

import Data.List

import Debug.Trace

------------------------------------------------------------------------------
-- DLM: principals

data Principal = Name String
               | TopP        -- acts for all principals
               | BottomP     -- allows anyone to act for it
  deriving Eq

instance Show Principal where
  show TopP = "TopP"
  show BottomP = "BottomP"
  show (Name p) = p

type PermissionContext = [(Principal,Principal)]

-- if (p1,p2) is (transitively) in delegation chain, i.e., if p1 has
-- delegated authority to p2 then p2 actsFor p1
-- (actsFor is reflexive and transitive)

-- p2 actsFor p1 given delegation chain dc
actsFor :: PermissionContext -> Principal -> Principal -> Bool
actsFor _ TopP _ = True
actsFor _ _ BottomP = True
actsFor dc p2 p1 = p1 == p2 || p2 `elem` dps || any (actsFor dc p2) dps
  where dps = foldl (\ps (p,p2) -> if p==p1 then p2:ps else ps) [] dc
         
------------------------------------------------------------------------------
-- Reader policies

-- RP p1 p2 means that p1 is owner and is allowing p2 to read
-- More generally p1 permits p to read only if p actsFor p1 or p2
data ReaderPolicy = RP Principal Principal
                  | JoinRP ReaderPolicy ReaderPolicy
                  | MeetRP ReaderPolicy ReaderPolicy
  deriving (Eq,Show)

-- Given the set of all known principals and the current delegation chain, we
-- can calculate, for each user, the readers of policy from the perspective
-- of that user

readers :: [Principal] -> PermissionContext -> 
           Principal -> ReaderPolicy -> [Principal]
readers allps dc user (RP o r) = 
  if actsFor dc o user
  then [ q | q <- allps, actsFor dc q o || actsFor dc q r ]
  else --trace "Owner CANNOT act for user!!!" $ 
       allps
readers allps dc user (JoinRP rp1 rp2) = 
  readers allps dc user rp1 `intersect` readers allps dc user rp2
readers allps dc user (MeetRP rp1 rp2) = 
  readers allps dc user rp1 `union` readers allps dc user rp2

noMoreRestrictiveThanRP :: [Principal] -> PermissionContext -> Principal -> 
                           ReaderPolicy -> ReaderPolicy -> Bool
noMoreRestrictiveThanRP allps dc user rp1 rp2 = 
  let rs2 = readers allps dc user rp2 
      rs1 = readers allps dc user rp1
  in --trace ("Readers rs1 of " ++ show rp1 ++ " are " ++ show rs1) $
     --trace ("Readers rs2 of " ++ show rp2 ++ " are " ++ show rs2) $
     --trace ("Need rs2 subset rs1: " ++ show (rs2 `subset` rs1)) $ 
     rs2 `subset` rs1

-- everyone can read
leastRestrictiveRP :: ReaderPolicy
leastRestrictiveRP = RP BottomP BottomP

-- nobody can read
mostRestrictiveRP :: ReaderPolicy
mostRestrictiveRP = RP TopP TopP

meetRP :: ReaderPolicy -> ReaderPolicy -> ReaderPolicy
meetRP rp1 rp2 = 
  if rp1 == leastRestrictiveRP || rp2 == leastRestrictiveRP 
  then leastRestrictiveRP 
  else if rp1 == mostRestrictiveRP then rp2
  else if rp2 == mostRestrictiveRP then rp1
  else MeetRP rp1 rp2

joinRP :: ReaderPolicy -> ReaderPolicy -> ReaderPolicy
joinRP rp1 rp2 = 
  if rp1 == mostRestrictiveRP || rp2 == mostRestrictiveRP 
  then mostRestrictiveRP 
  else if rp1 == leastRestrictiveRP then rp2
  else if rp2 == leastRestrictiveRP then rp1
  else JoinRP rp1 rp2

------------------------------------------------------------------------------
-- Writer policies: A better name would be "principals that TRUST the data"
-- The least restrictive policy is for nobody to trust the data and then we
-- go up the lattice to the most restrictive policy which states that
-- everyone trusts the data

data WriterPolicy = WP Principal Principal
                  | JoinWP WriterPolicy WriterPolicy
                  | MeetWP WriterPolicy WriterPolicy
  deriving (Eq,Show)

writers :: [Principal] -> PermissionContext -> 
           Principal -> WriterPolicy -> [Principal]
writers allps dc user (WP o w) = 
  if actsFor dc o user
  then [ q | q <- allps, actsFor dc q o || actsFor dc q w ]
  else [] 
writers allps dc user (MeetWP wp1 wp2) = 
  writers allps dc user wp1 `intersect` writers allps dc user wp2
writers allps dc user (JoinWP wp1 wp2) = 
  writers allps dc user wp1 `union` writers allps dc user wp2

noMoreRestrictiveThanWP :: [Principal] -> PermissionContext -> Principal -> 
                           WriterPolicy -> WriterPolicy -> Bool
noMoreRestrictiveThanWP allps dc user wp1 wp2 = 
  writers allps dc user wp1 `subset` writers allps dc user wp2

leastRestrictiveWP :: WriterPolicy
leastRestrictiveWP = WP TopP TopP

mostRestrictiveWP :: WriterPolicy
mostRestrictiveWP = WP BottomP BottomP

meetWP :: WriterPolicy -> WriterPolicy -> WriterPolicy
meetWP wp1 wp2 = 
  if wp1 == leastRestrictiveWP || wp2 == leastRestrictiveWP 
  then leastRestrictiveWP 
  else if wp1 == mostRestrictiveWP then wp2
  else if wp2 == mostRestrictiveWP then wp1
  else MeetWP wp1 wp2

joinWP :: WriterPolicy -> WriterPolicy -> WriterPolicy
joinWP wp1 wp2 = 
  if wp1 == mostRestrictiveWP || wp2 == mostRestrictiveWP 
  then mostRestrictiveWP
  else if wp1 == leastRestrictiveWP then wp2
  else if wp2 == leastRestrictiveWP then wp1
  else MeetWP wp1 wp2

------------------------------------------------------------------------------
-- Labels

data Label = L ReaderPolicy WriterPolicy deriving (Eq,Show)

joinL :: Label -> Label -> Label
joinL (L rp1 wp1) (L rp2 wp2) = L (joinRP rp1 rp2) (joinWP wp1 wp2)

meetL :: Label -> Label -> Label
meetL (L rp1 wp1) (L rp2 wp2) = L (meetRP rp1 rp2) (meetWP wp1 wp2)
  
meetLs :: [Label] -> Label
meetLs = foldr meetL mostRestrictiveL

readersL :: [Principal] -> PermissionContext -> 
            Principal -> Label -> [Principal]
readersL allps dc user (L rp _) = readers allps dc user rp

writersL :: [Principal] -> PermissionContext -> 
            Principal -> Label -> [Principal]
writersL allps dc user (L _ wp) = writers allps dc user wp

noMoreRestrictiveThanL :: [Principal] -> PermissionContext -> Principal -> 
                           Label -> Label -> Bool
noMoreRestrictiveThanL allps dc user (L rp1 wp1) (L rp2 wp2) = 
  --trace ("Checking flow from " ++ show (L rp1 wp1) ++ " to " ++ show (L rp2 wp2)) $ 
  --trace ("User is " ++ show user) $
    noMoreRestrictiveThanRP allps dc user rp1 rp2 && 
    noMoreRestrictiveThanWP allps dc user wp1 wp2

leastRestrictiveL :: Label
leastRestrictiveL = L leastRestrictiveRP leastRestrictiveWP 

mostRestrictiveL :: Label 
mostRestrictiveL = L mostRestrictiveRP mostRestrictiveWP 

------------------------------------------------------------------------------
-- Helpers, examples, and tests

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (`elem` ys) xs

{--

p0 = Name "P0"
p1 = Name "P1"
p2 = Name "P2"
p3 = Name "P3"
p4 = Name "P4"
p5 = Name "P5"
p6 = Name "P6"
p7 = Name "P7"
p8 = Name "P8"
p9 = Name "P9"
r0 = Name "R0"
r1 = Name "R1"
r2 = Name "R2"
r3 = Name "R3"
r4 = Name "R4"
r5 = Name "R5"
r6 = Name "R6"
r7 = Name "R7"
r8 = Name "R8"
r9 = Name "R9"
o0 = Name "O0"
o1 = Name "O1"
o2 = Name "O2"
o3 = Name "O3"
o4 = Name "O4"
o5 = Name "O5"
o6 = Name "O6"
o7 = Name "O7"
o8 = Name "O8"
o9 = Name "O9"
grp = Name "Group"
bob = Name "Bob"
amy = Name "Amy"
manager = Name "Manager"
carl = Name "Carl" 
doctor = Name "Doctor"

allps = [p0, p1, p2, p3, p4, p5, p6, p7, p8, p9,
         r0, r1, r2, r3, r4, r5, r6, r7, r8, r9,
         o0, o1, o2, o3, o4, o5, o6, o7, o8, o9,
        grp, bob, amy, manager, carl, doctor]

perms = 
    [(grp,bob),(grp,amy),(bob,manager),(amy,manager),
     (manager,carl),(doctor,carl),
     (bob,o1),(bob,o2),(p9,o1),(p9,o2),(p9,r2)]

t0 = actsFor perms carl bob -- True
t1 = actsFor perms amy bob  -- False

rp1 = MeetRP (RP o1 r1) (RP o1 r2) 
rp2 = MeetRP (RP o2 r2) (RP o2 r3) 
rp3 = JoinRP rp1 rp2
lab = L rp3 leastRestrictiveWP
rp4 = MeetRP (RP amy bob) (RP amy carl)
rp5 = RP amy carl

t2 = readers allps perms p9 rp1
t3 = readers allps perms p9 rp2
t4 = readersL allps perms p9 lab

--}

bob = Name "Bob"
taxUser = Name "TaxUser"
taxPreparer = Name "TaxPreparer"

allps = [bob, taxUser, taxPreparer]

perms = [(bob,taxUser)]

lab1 = (RP bob bob) 
lab2 = (RP taxUser taxUser) 
lab3 = (RP taxPreparer taxPreparer) 
lab4 = joinRP lab2 lab3
    
------------------------------------------------------------------------------

