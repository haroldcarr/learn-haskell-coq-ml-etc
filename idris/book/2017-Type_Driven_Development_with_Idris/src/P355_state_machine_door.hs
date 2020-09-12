{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE StandaloneDeriving #-}

module P355_state_machine_door where

data DoorState = DoorOpen | DoorClosed deriving (Show)

data DoorCmd :: DoorState -> DoorState -> * where
  Open     :: DoorCmd DoorClosed DoorOpen
  Close    :: DoorCmd DoorOpen   DoorClosed
  RingBell :: DoorCmd DoorClosed DoorClosed
  Bind     :: DoorCmd state1     state2
           -> DoorCmd state2     state3
           -> DoorCmd state1     state3

deriving instance Show (DoorCmd doorStateBefore doorStateAfter)

doorProg :: DoorCmd 'DoorClosed 'DoorClosed
doorProg = RingBell `Bind` Open `Bind` Close -- `Bind` Open

