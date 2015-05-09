
#+BEGIN_SRC haskell
productFunctorLeft  :: (e -> a) -> (c -> e) -> (f -> b) -> (d -> f) -> (c, d) -> (a, b)
productFunctorLeft  g h i j = PDBC.product (g . h) (i . j)

productFunctorRight :: (e -> a) -> (c -> e) -> (f -> b) -> (d -> f) -> (c, d) -> (a, b)
productFunctorRight g h i j = PDBC.product g i . PDBC.product h j

p8 = U.tt "p8"
     [ ((productFunctorLeft  (+2) (+4) (+6.0) (+8.0))::(Int,Double)->(Int,Double))        (1,100.0)
     , ((productFunctorRight (+2) (+4) (+6.0) (+8.0))::(Int,Double)->(Int,Double))        (1,100.0)
     ]
     (7,114.0)
#+END_SRC
