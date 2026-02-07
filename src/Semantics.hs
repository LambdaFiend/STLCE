module Semantics where

import Syntax
import Helper

evalSubst :: TermNode -> TermNode -> TermNode
evalSubst s t = shift' 0 (-1) (subst' 0 (shift' 0 1 s) t)

isVal :: TermNode -> Bool
isVal t = let tm = getTm t in
  case tm of
    TmAbs _ _ _ -> True
    TmTrue -> True
    TmFalse -> True
    TmZero -> True
    TmSucc nv | isVal nv -> True
    TmUnit -> True
    _ -> False

eval1 :: TermNode -> TermNode
eval1 t = TermNode fi $
  case tm of
    TmApp (TermNode _ (TmAbs _ _ t12)) v2
      | isVal v2 -> getTm $ evalSubst v2 t12
    TmApp v1 t2 | isVal v1 ->
      let t2' = eval1 t2
       in TmApp v1 t2'
    TmApp t1 t2 ->
      let t1' = eval1 t1
       in TmApp t1' t2
    TmIf (TermNode _ TmTrue) t2 t3 -> getTm t2
    TmIf (TermNode _ TmFalse) t2 t3 -> getTm t3
    TmIf t1 t2 t3 ->
      let t1' = eval1 t1
       in TmIf t1' t2 t3
    TmSucc t1 | not $ isVal t1 -> TmSucc $ eval1 t1
    TmPred (TermNode _ TmZero) -> TmZero
    TmPred (TermNode _ (TmSucc nv1)) | isVal nv1 -> getTm nv1
    TmPred t1 -> TmPred $ eval1 t1
    TmIsZero (TermNode _ TmZero) -> TmTrue
    TmIsZero (TermNode _ (TmSucc nv1)) | isVal nv1 -> TmFalse
    TmIsZero t1 -> TmIsZero $ eval1 t1
    TmUnit -> TmUnit
    _ -> error "No rule applies"
  where tm = getTm t
        fi = getFI t

eval :: TermNode -> TermNode
eval t | isVal t   = t
       | otherwise = eval $ eval1 t
