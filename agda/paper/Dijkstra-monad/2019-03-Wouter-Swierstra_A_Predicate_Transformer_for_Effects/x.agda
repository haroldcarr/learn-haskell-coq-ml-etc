correct : SafeDiv ⊆ wpPartial ⟦_⟧ _⇓_

correct (Val n) tt = ⇓Base

correct (Div exprL exprR) (exprR⇓zero→⊥ , safeDivExprL , safeDivExprR)
  with  ⟦ exprL ⟧ | ⟦ exprR ⟧
... | Pure ln                    | Pure rn                    = {!!}
... | Pure ln                    | Step Abort r⊥→Free-C-R-Nat = exprR⇓zero→⊥ {!!}
... | Step Abort l⊥→Free-C-R-Nat | Pure rn                    = {!!}
... | Step Abort l⊥→Free-C-R-Nat | Step Abort r⊥→Free-C-R-Nat = {!!}

--   with exprL | exprR
-- ... | Val ln    | Val rn    = {!!}
-- ... | Val ln    | Div rl rr = {!!}
-- ... | Div ll lr | Val n     = {!!}
-- ... | Div ll lr | Div rl rr = {!!}


--  with (correct exprL) safeDivExprL | (correct exprR) safeDivExprR | ⟦ exprL ⟧ | ⟦ exprR ⟧ | exprL | exprR
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Pure r | Val x | Val x₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Pure r | Val x | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Pure r | Div el el₁ | Val x = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Pure r | Div el el₁ | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Step Abort r | Val x | Val x₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Step Abort r | Val x | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Step Abort r | Div el el₁ | Val x = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Pure l | Step Abort r | Div el el₁ | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Pure r | Val x | Val x₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Pure r | Val x | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Pure r | Div el el₁ | Val x = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Pure r | Div el el₁ | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Step Abort r | Val x | Val x₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Step Abort r | Val x | Div er er₁ = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Step Abort r | Div el el₁ | Val x = {!!}
-- ... | mustPT_⇓_exL⟦exL⟧ | mustPT_⇓_exR⟦exR⟧ | Step Abort l | Step Abort r | Div el el₁ | Div er er₁ = {!!}

{-
----- another try
correct (Val x)                                   tt                                        =
  ⇓Base
correct (Div (Val x1)          (Val x2))          (r2⇓zero→⊥ , tt , tt)                     =
  {!!}
correct (Div (Val x)           (Div expr2 expr3)) (r2⇓zero→⊥ , tt , fst , snd)              =
  {!!}
correct (Div (Div expr1 expr3) (Val x))           (r2⇓zero→⊥ , (fst , snd) , tt)            =
  {!!}
correct (Div (Div expr1 expr3) (Div expr2 expr4)) (r2⇓zero→⊥ , safeDivExpr1 , safeDivExpr2) =
  {!!}
-}


