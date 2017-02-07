> module Lib where

------------------------------------------------------------------------------
2 Associated types: indexing types by types

ways to express relations on types
- multiparameter type classes express many-to-many relations
- type constructors express functional relations
  - e.g., relation between type of a list and type of list’s elements is functional relation
  - expressed by type constructor [] :: * -> *
  - maps type `a` to type `[a]`
  - *type constructor maps its args uniformly, using them in a more complex type without inspecting them*

Type functions
- topic of this paper
- also establish functional relations between types
- but *type function may perform case analysis on its argument types*



