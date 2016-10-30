> module Ch07_02_higher_kinded_polymorphism where

Higher-kinded polymorphism

Type-classes with one parameter type have kind `* -> *`, e.g.,:

  class Show  a :: * -> *
  class Maybe a :: * -> *

When declaring an instance of Show

  instance (Show a) => Show (Maybe a) where ...

the type-class parameters in the kind signatures need to be aligned

to match the kind of `a :: *` in `Show a`
- use        : `Maybe' b :: *`  -- i.e, `Maybe` is conceptually applied to the `a` in `(Show a)`
- instead of : `Maybe :: * -> *`


The Monad type-class is of a higher-order than Show and Maybe:

 class Monad m :: (* -> *) -> *

The Show  type-class is parameterized over type a :: *.

The Monad type-class is parameterized over type constructor m :: * -> *.

Hudak et al's History of Haskell:

"The first major, unanticipated development in the type-class story came when Mark Jones suggested
parameterizing a class over a type constructor instead of over a type...

"Jones's paper appeared in 1993, the same year that monads became popular for I/O.
The fact that type classes so directly supported monads made monads far more accessible and popular;
and dually, the usefulness of monadic I/O ensured the adoption of higher-kinded polymorphism."
