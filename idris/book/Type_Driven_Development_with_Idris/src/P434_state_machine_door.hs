{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module P434_state_machine_door where

data DoorState = DoorOpen | DoorClosed

data DoorCmd :: DoorState -> DoorState -> * where
  Open     :: DoorCmd DoorClosed DoorOpen
  Close    :: DoorCmd DoorOpen   DoorClosed
  RingBell :: DoorCmd DoorClosed DoorClosed
  Bind     :: DoorCmd state1     state2
           -> DoorCmd state2     state3
           -> DoorCmd state1     state3

doorProg :: DoorCmd DoorClosed DoorClosed
doorProg = RingBell `Bind` Open `Bind` Close -- `Bind` Open

