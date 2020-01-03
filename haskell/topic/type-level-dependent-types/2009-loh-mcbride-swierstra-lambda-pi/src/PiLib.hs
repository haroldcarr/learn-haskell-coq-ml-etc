module PiLib where

import           PiEval
import           PiTypes
import           STEval
import           STTypes

lpte :: Ctx Value_
lpte =     [   (Global "Zero", VNat_),
               (Global "Succ", VPi_ VNat_ (const VNat_)),
               (Global "Nat", VStar_),
               (Global "natElim", VPi_ (VPi_ VNat_ (const VStar_)) (\m ->
                                 VPi_ (m `vapp_` VZero_) (\_ ->
                                 VPi_ (VPi_ VNat_ (\k -> VPi_ (m `vapp_` k) (const (m `vapp_` VSucc_ k)))) ( \_ ->
                                 VPi_ VNat_ (\n -> m `vapp_` n))))),
               (Global "Nil", VPi_ VStar_ (`VVec_` VZero_)),
               (Global "Cons", VPi_ VStar_ (\a ->
                              VPi_ VNat_ (\n ->
                              VPi_ a (\_ -> VPi_ (VVec_ a n) (\_ -> VVec_ a (VSucc_ n)))))),
               (Global "Vec", VPi_ VStar_ (\_ -> VPi_ VNat_ (const VStar_))),
               (Global "vecElim", VPi_ VStar_ (\a ->
                                 VPi_ (VPi_ VNat_ (\n -> VPi_ (VVec_ a n) (const VStar_))) (\m ->
                                 VPi_ (m `vapp_` VZero_ `vapp_` VNil_ a) (\_ ->
                                 VPi_ (VPi_ VNat_ (\n ->
                                       VPi_ a (\x ->
                                       VPi_ (VVec_ a n) (\xs ->
                                       VPi_ (m `vapp_` n `vapp_` xs) (\_ ->
                                       m `vapp_` VSucc_ n `vapp_` VCons_ a n x xs))))) (\_ ->
                                 VPi_ VNat_ (\n ->
                                 VPi_ (VVec_ a n) (\xs -> m `vapp_` n `vapp_` xs))))))),
               (Global "Refl", VPi_ VStar_ (\a -> VPi_ a (\x ->
                              VEq_ a x x))),
               (Global "Eq", VPi_ VStar_ (\a -> VPi_ a (\_x -> VPi_ a (const VStar_)))),
               (Global "eqElim", VPi_ VStar_ (\a ->
                                VPi_ (VPi_ a (\x -> VPi_ a (\y -> VPi_ (VEq_ a x y) (const VStar_)))) (\m ->
                                VPi_ (VPi_ a (\x -> m `vapp_` x `vapp_` x `vapp_` VRefl_ a x)) (\_ ->
                                VPi_ a (\x -> VPi_ a (\y ->
                                VPi_ (VEq_ a x y) (\eq ->
                                m `vapp_` x `vapp_` y `vapp_` eq))))))),
               (Global "FZero", VPi_ VNat_ (VFin_ . VSucc_)),
               (Global "FSucc", VPi_ VNat_ (\n -> VPi_ (VFin_ n) (\_f ->
                               VFin_ (VSucc_ n)))),
               (Global "Fin", VPi_ VNat_ (const VStar_)),
               (Global "finElim", VPi_ (VPi_ VNat_ (\n -> VPi_ (VFin_ n) (const VStar_))) (\m ->
                                 VPi_ (VPi_ VNat_ (\n -> m `vapp_` VSucc_ n `vapp_` VFZero_ n)) (\_ ->
                                 VPi_ (VPi_ VNat_ (\n -> VPi_ (VFin_ n) (\f -> VPi_ (m `vapp_` n `vapp_` f) (\_ -> m `vapp_` VSucc_ n `vapp_` VFSucc_ n f)))) (\_ ->
                                 VPi_ VNat_ (\n -> VPi_ (VFin_ n) (\f ->
                                 m `vapp_` n `vapp_` f))))))]

lpve :: Ctx Value_
lpve =     [   (Global "Zero", VZero_),
               (Global "Succ", VLam_ VSucc_),
               (Global "Nat", VNat_),
               (Global "natElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (NatElim_ (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0)))))))) ([], [])),
               (Global "Nil", VLam_ VNil_),
               (Global "Cons", VLam_ (\a -> VLam_ (\n -> VLam_ (VLam_ . VCons_ a n)))),
               (Global "Vec", VLam_ (VLam_ . VVec_)),
               (Global "vecElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (VecElim_ (Inf_ (Bound_ 5)) (Inf_ (Bound_ 4)) (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0)))))))))) ([],[])),
               (Global "Refl", VLam_ (VLam_ . VRefl_)),
               (Global "Eq", VLam_ (\a -> VLam_ (VLam_ . VEq_ a))),
               (Global "eqElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (EqElim_ (Inf_ (Bound_ 5)) (Inf_ (Bound_ 4)) (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0)))))))))) ([],[])),
               (Global "FZero", VLam_ VFZero_),
               (Global "FSucc", VLam_ (VLam_ . VFSucc_)),
               (Global "Fin", VLam_ VFin_),
               (Global "finElim", cEval_ (Lam_ (Lam_ (Lam_ (Lam_ (Lam_ (Inf_ (FinElim_ (Inf_ (Bound_ 4)) (Inf_ (Bound_ 3)) (Inf_ (Bound_ 2)) (Inf_ (Bound_ 1)) (Inf_ (Bound_ 0))))))))) ([],[]))]
