module Semantics where

import Syntax

-- considering that we're using De Bruijn's representation:

-- shift c d:
-- of a var k = if k < c then var k else var (k + d)
-- of an abs t1 = abs (shift (c + 1) d of t1)
-- of an app t1 t2 = app (shift c d of t1) (shift c d of t2)

-- substitution of bound var j by term s:
-- on a var k = if k == j then s else k
-- on an abs t1 = abs (substitution of bound var (j + 1) by term (shift 1 0 s) on t1)
-- on an app t1 t2 = app (substitution of bound var j by s on t1) (substitution of bound var j by s on t2)


shift :: Index -> Index -> TermNode -> TermNode
shift c d t = TermNode fi $
  case tm of
    TmVar k l -> TmVar (if k < c then k else k + d) (l + d)
    TmAbs x ty t1 -> TmAbs x ty (shift (c + 1) d t1)
    TmApp t1 t2 -> TmApp (shift' t1) (shift' t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (shift' t1) (shift' t2) (shift' t3)
    TmZero -> TmZero
    TmSucc t1 -> TmSucc $ shift' t1
    TmPred t1 -> TmPred $ shift' t1
    TmUnit -> TmUnit
  where tm = getTm t
        fi = getFI t
        shift' = shift c d


subst :: Index -> TermNode -> TermNode -> TermNode
subst j s t = TermNode fi $
  case tm of
    TmVar k l -> if k == j then getTm s else tm
    TmAbs x ty t1 -> TmAbs x ty (subst (j + 1) (shift 1 0 s) t1)
    TmApp t1 t2 -> TmApp (subst j s t1) (subst j s t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (subst j s t1) (subst j s t2) (subst j s t3)
    TmZero -> TmZero
    TmSucc t1 -> getTm $ subst' t1
    TmPred t1 -> getTm $ subst' t1
    TmUnit -> TmUnit
  where tm = getTm t
        fi = getFI t
        subst' = subst j s

evalSubst :: TermNode -> TermNode -> TermNode
evalSubst s t = shift 0 (-1) (subst 0 (shift 0 1 s) t)

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
