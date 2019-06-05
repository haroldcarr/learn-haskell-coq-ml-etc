module Misc where

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
data ExprL a
  = Const a
  | Add [ExprL a]
  | Mul [ExprL a]
  deriving (Eq, Show)

e :: [ExprL Integer]
e = [Add [Const 3, Mul [Const 4, Const 5]]]

i :: Num a => [ExprL a] -> [a]
i [] = []
i [x] = case x of
  Const i' -> [i']
  Add axs -> let [[x'],[y]] = map (\z -> i [z]) axs in [x'+y]
  Mul mxs -> let [[x'],[y]] = map (\z -> i [z]) mxs in [x'*y]
i _ = error "NO"

