-- non-optimized version
pcp                        :: (d -> a) -> (e -> b) -> (c -> d) -> (c -> e) -> c -> (a, b)
pcp                i j g h = PDBC.product i j . pair g h

-- optimized version via 2.20
productComposePair         :: (d -> a) -> (e -> b) -> (c -> d) -> (c -> e) -> c -> (a, b)
productComposePair i j g h = pair (i . g) (j . h)

p6 = U.tt "p6"
     [ pcp                show read (*2) show   4
     , productComposePair show read (*2) show   4
     ]
     ("8",4)
