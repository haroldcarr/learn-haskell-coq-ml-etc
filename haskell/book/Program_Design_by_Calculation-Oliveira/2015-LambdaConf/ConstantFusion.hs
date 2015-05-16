module ConstantFusion where

c' :: a -> Char
c' = const 'c'

constantFusion :: (a -> c) -> (d -> a) -> d -> c
constantFusion c f = c . f

c0 :: Char
c0 = constantFusion c' (+1) 45

c1 :: Integer
c1 = const 30  . ("foo" ++) $ "bar"

c0' :: Char
c0' = c' 45

-- TODO NICTA example

-- End
