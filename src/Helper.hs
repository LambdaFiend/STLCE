module Helper where

import Syntax
import Typing

traverseTm :: (TermNode -> TermNode) -> TermNode -> TermNode
traverseTm f t = f $ TermNode fi $
  case tm of
    TmVar k l -> tm
    TmVarRaw x -> tm
    TmAbs x ty t1 -> TmAbs x ty (traverseTm' t1)
    TmApp t1 t2 -> TmApp (traverseTm' t1) (traverseTm' t2)
    TmTrue -> tm
    TmFalse -> tm
    TmIf t1 t2 t3 -> TmIf (traverseTm' t1) (traverseTm' t2) (traverseTm' t3)
    TmZero -> tm
    TmSucc t1 -> TmSucc $ traverseTm' t1
    TmPred t1 -> TmPred $ traverseTm' t1
    TmIsZero t1 -> TmIsZero $ traverseTm' t1
    TmUnit -> tm
    TmSeq t1 t2 -> TmSeq (traverseTm' t1) (traverseTm' t2)
  where tm = getTm t
        fi = getFI t
        traverseTm' = traverseTm f

deriveTm' :: TermNode -> TermNode
deriveTm' t = traverseTm deriveTm t

deriveTm :: TermNode -> TermNode
deriveTm (TermNode fi (TmSeq t1 t2)) =
  let ty = typeOf' t1
   in TermNode fi $ TmApp (TermNode fi $ TmAbs "x" ty t2) t1
deriveTm t = t

genIndex' :: TermNode -> TermNode
genIndex' t = genIndex [] t

genIndex :: [Name] -> TermNode -> TermNode
genIndex ctx t = TermNode fi $
  case tm of
    TmVar k l -> tm
    TmVarRaw x ->
      TmVar (length $ takeWhile (/= x) ctx) (length ctx)
    TmAbs x ty t1 ->
      TmAbs x ty (genIndex (x:ctx) t1)
    TmApp t1 t2 -> TmApp (genIndex' t1) (genIndex' t2)
    TmTrue -> tm
    TmFalse -> tm
    TmIf t1 t2 t3 -> TmIf (genIndex' t1) (genIndex' t2) (genIndex' t3)
    TmZero -> tm
    TmSucc t1 -> TmSucc $ genIndex' t1
    TmPred t1 -> TmPred $ genIndex' t1
    TmIsZero t1 -> TmIsZero $ genIndex' t1
    TmUnit -> tm
    TmSeq t1 t2 -> TmSeq (genIndex' t1) (genIndex' t2)
  where tm = getTm t
        fi = getFI t
        genIndex' = genIndex ctx
