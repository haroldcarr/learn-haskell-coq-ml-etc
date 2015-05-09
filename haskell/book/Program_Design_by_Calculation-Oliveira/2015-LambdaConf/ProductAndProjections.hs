p7 = U.tt "p7"
     [ (fst . (PDBC.product show show))        (3, 4)
     , show $ fst                              (3, 4) -- optimized via 2.26
     ]
     "3"
