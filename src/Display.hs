module Display where

import Syntax
import Lexer

showTm' :: TermNode -> String
showTm' t = showTm [] t

showTm :: Context -> TermNode -> String
showTm ctx t = let tm = getTm t in
  case tm of
    TmSucc t1 -> "succ " ++ "(" ++ showTm' t1 ++ ")"
    TmPred t1 -> "pred " ++ "(" ++ showTm' t1 ++ ")"
    TmIsZero t1 -> "iszero " ++ "(" ++ showTm' t1 ++ ")"
    TmZero -> "0"
    TmUnit -> "unit"
    TmSeq t1 t2 -> showTm' t1 ++ ";" ++ showTm' t2
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 -> "(" ++ "if " ++ showTm' t1
      ++ " then " ++ showTm' t2
      ++ " else " ++ showTm' t3 ++ ")"
    TmVar k l ->
      if (l == length ctx)
        then getNameFromContext ctx k
        else error "TmVar: bad context length"
    TmAbs x ty t1 ->
      let x' = fixName ctx x
       in "(" ++ "λ" ++ x' ++ ":" ++ showType ty ++ "." ++ showTm ((x', ty):ctx) t1 ++ ")"
    TmApp t1 t2 -> "(" ++ showTm' t1 ++ " " ++ showTm' t2 ++ ")"
  where showTm' = showTm ctx

getNameFromContext :: Context -> Index -> Name
getNameFromContext ctx ind | ind < length ctx = fst (ctx !! ind)
                           | otherwise = error "TmVar: no name context for var"

fixName :: Context -> Name -> Name
fixName ctx x | (length $ filter ((==) x . fst) ctx) < 1 = x
              | otherwise = fixName ctx (x ++ "\'")

showFileInfo :: FileInfo -> String
showFileInfo (AlexPn p l c) =
  "Absolute Offset" ++ show p ++ "\n"
  ++ "Line" ++ show l ++ "\n"
  ++ "Column" ++ show c ++ "\n"

showType :: Type -> String
showType ty =
  case ty of
    TyNat -> "Nat"
    TyBool -> "Bool"
    TyUnit -> "Unit"
    TyArr ty1 ty2 -> "(" ++ showType ty1 ++ "->" ++ showType ty2 ++ ")"
