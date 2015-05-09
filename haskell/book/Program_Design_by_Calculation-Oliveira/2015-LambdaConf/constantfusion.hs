c :: a -> Char
c = const 'c'

c0 = c         . (+1)       $ 45

c1 = const 30  . ("foo" ++) $ "bar"
