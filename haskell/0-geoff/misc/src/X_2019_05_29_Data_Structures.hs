{-# LANGUAGE TypeOperators #-} -- needed to define (:+:)

{-
"So I'm guessing that a data structure is comprised of two types of things:
 1.nodes - (data)
 2.pointers , (or links?) to the next node - directions about how to get the next node/ data."

Basically right.

"So that the names of a type of data structures (heap, stack, list,
tree, array etc etc. ) describes the way the nodes/pointers are
organized?"

Correct.

"Graph theory
Vertex = node, edge = pointer.
Is this correct or am I missing something ? and is my terminology correct?"

Basically right"
-}

module X_2019_05_29_Data_Structures where

{-
The term "data structure" is often used to mean recursive structures like lists and trees.
But strictly speaking it refers to any thing that can be defined and created.
-}

-- A data TYPE definition that cannot be created (because it does not have any constructors).
data Zero

-- A data type definition that has one   possible value.
data One = One

-- A data type definition that has two   possible values
data Two = A | B

-- A data type definition that has three possible values
data Three = X | Y | Z

{-
Some people might not call the above "structures".
They are often referred to as "enumerations".
-}

------------------------------------------------------------------------------

{-
This section contain examples of simple (non-recursive) structures that can "hold" something.
-}

-- A data type definition that has a constructor that can hold exactly one Int.
-- Even though there is only one constructor there are an "infinite" number of values:
-- HasInt 0, HasInt 1, ...
data IntContainer        = HasInt      Int
-- Same kind of container, but it can hold ONE thing of any type.
data AnythingContainer a = HasAnything a

------------------------------------------------------------------------------

{-
Most books on algorithems and data structures focus on recursive types (types that
refer to themselves in their definitions).  This section shows various types often
singly-linked lists.

IntListContainer        : list that can only hold Int
AnythingListContainer a : list that can only any type
List                  a : same as AnythingListContainer - what you often see in tutorials
(:+:)                 a : same as List, but defined with infix symbols rather than prefix names.
                          (:+:) is essentially how Haskell's [a] is defined
                          (but you can't override the builtin definition)
-}

data IntListContainer        = IntListCons      Int IntListContainer          | IntListNil
data AnythingListContainer a = AnythingListCons a   (AnythingListContainer a) | AnythingListNil
data List                  a = Cons             a   (List                  a) | Nil
data (:+:)                 a = a :+:                (:+:) a                   | (:-:)

------------------------------------------------------------------------------

-- Another recursive data "structure"
data BTree a
  = Branch (BTree a) (BTree a)
  | Leaf a
  deriving Show

exampleBTree :: BTree Int
exampleBTree =
  Branch (Branch (Leaf 1)
                 (Branch (Leaf 2)
                         (Leaf 3)))
         (Leaf 4)
