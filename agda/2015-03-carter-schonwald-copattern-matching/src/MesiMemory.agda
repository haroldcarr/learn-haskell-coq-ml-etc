module MesiMemory where

-- https://www.reddit.com/r/haskell/comments/4aju8f/simple_example_of_emulating_copattern_matching_in/d117b1k?utm_source=share&utm_medium=web2x&context=3
-- https://gist.github.com/cartazio/5b1d3d8dd12b9279866d

-- https://en.wikipedia.org/wiki/MESI_protocol

open import Data.Product

data MesiState : Set where
  Invalid   : MesiState
  Modified  : MesiState
  Exclusive : MesiState
  Shared    : MesiState
open MesiState


record MesiCommandI  : Set where
  coinductive
  field
    LocalReadI   : MesiState × MesiCommandI
    LocalWriteI  : MesiState × MesiCommandI
    RemoteReadI  : MesiState × MesiCommandI
    RemoteWriteI : MesiState × MesiCommandI
open MesiCommandI

denoteMCI : MesiState -> MesiCommandI
LocalReadI   (denoteMCI Invalid)   = Exclusive , denoteMCI Exclusive
LocalReadI   (denoteMCI Modified)  = Modified  , denoteMCI Modified
LocalReadI   (denoteMCI Exclusive) = Exclusive , denoteMCI Exclusive
LocalReadI   (denoteMCI Shared)    = Shared    , denoteMCI Shared   -- MesiState  ×  MesiCommandI

LocalWriteI  (denoteMCI Invalid)   = Modified  , denoteMCI Modified
LocalWriteI  (denoteMCI Modified)  = Modified  , denoteMCI Modified
LocalWriteI  (denoteMCI Exclusive) = Modified  , denoteMCI Modified
LocalWriteI  (denoteMCI Shared)    = Modified  , denoteMCI Modified -- MesiState  ×  MesiCommandI

RemoteReadI  (denoteMCI Invalid)   = Invalid   , denoteMCI Invalid  -- check
RemoteReadI  (denoteMCI Modified)  = Invalid   , denoteMCI Invalid
RemoteReadI  (denoteMCI Exclusive) = Shared    , denoteMCI Shared
RemoteReadI  (denoteMCI Shared)    = Shared    , denoteMCI Shared   -- MesiState  ×  MesiCommandI

RemoteWriteI (denoteMCI Invalid)   = Invalid   , denoteMCI Invalid  -- check
RemoteWriteI (denoteMCI Modified)  = Invalid   , denoteMCI Invalid
RemoteWriteI (denoteMCI Exclusive) = Invalid   , denoteMCI Invalid
RemoteWriteI (denoteMCI Shared)    = Invalid   , denoteMCI Invalid  -- MesiState  ×  MesiCommandI
